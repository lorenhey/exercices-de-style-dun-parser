;;;; Parser V7 - Common Lisp (SBCL recomendado)
;;;; CKY + beam + unary-closure + tokenización (al/del + enclíticos 1)
;;;; Consume lexicon.json + grammar.json (archivos neutrales).
;;;;
;;;; Run:
;;;;   sbcl --script parser_v7.lisp -- --file corpus.txt --print --json out.json
;;;;   sbcl --script parser_v7.lisp -- --text "..." --trees --print

(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (progn
        (require :asdf)
        (handler-case
            (progn
              (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)) :if-does-not-exist nil)
              (when (find-package :ql)
                (funcall (intern "QUICKLOAD" :ql) :yason)))
          (error () nil)))
    (error () nil)))

(defpackage :parser-v7
  (:use :cl)
  (:export :main))
(in-package :parser-v7)

;; -----------------------------
;; Small utilities
;; -----------------------------

(defun starts-with-qmark-p (s)
  (and (stringp s)
       (> (length s) 0)
       (char= (char s 0) #\?)))

(defun letterp* (ch)
  ;; Prefer SBCL Unicode predicate when present, else alpha-char-p.
  (let* ((pkg (find-package :sb-unicode))
         (sym (and pkg (find-symbol "ALPHABETIC-P" pkg))))
    (cond
      ((and sym (fboundp sym)) (funcall (symbol-function sym) ch))
      (t (alpha-char-p ch)))))

(defun word-char-p (ch)
  (or (letterp* ch) (char= ch #\-) (char= ch #\')))

(defun string-downcase* (s)
  (string-downcase s))

(defun string-suffix-p (suffix s)
  (let ((ls (length s)) (lf (length suffix)))
    (and (>= ls lf)
         (string= suffix s :start2 (- ls lf)))))

(defun format-f (x &optional (digits 3))
  (format nil (concatenate 'string "~," (write-to-string digits) "f") x))

;; -----------------------------
;; Symtab
;; -----------------------------

(defstruct (symtab (:constructor make-symtab))
  (map (make-hash-table :test 'equal) :type hash-table)
  (vec (make-array 0 :adjustable t :fill-pointer 0)))

(defun symtab-intern (st s)
  (or (gethash s (symtab-map st))
      (let ((id (fill-pointer (symtab-vec st))))
        (vector-push-extend s (symtab-vec st))
        (setf (gethash s (symtab-map st)) id)
        id)))

(defun symtab-str (st id)
  (if (and (integerp id) (>= id 0) (< id (length (symtab-vec st))))
      (aref (symtab-vec st) id)
      "?"))

(defun symtab-var-p (st id)
  (starts-with-qmark-p (symtab-str st id)))

;; -----------------------------
;; Feats
;; feats = list of (cons key-id . val-id), sorted by key then val
;; -----------------------------

(defun feats-norm (fs)
  (sort (copy-list fs)
        (lambda (a b)
          (let ((ka (car a)) (va (cdr a))
                (kb (car b)) (vb (cdr b)))
            (or (< ka kb) (and (= ka kb) (< va vb)))))))

(defun feats-find (fs key-id)
  (dolist (kv fs nil)
    (when (= (car kv) key-id)
      (return (cdr kv)))))

(defconstant +fnv-off+ 1469598103934665603)
(defconstant +fnv-pr+  1099511628211)
(defconstant +mask64+  #xffffffffffffffff)

(defun fnv-step (h x)
  (logand (* (logxor h x) +fnv-pr+) +mask64+))

(defun feats-hash64 (fs)
  (let ((h +fnv-off+))
    (dolist (kv fs h)
      (setf h (fnv-step h (car kv)))
      (setf h (fnv-step h (cdr kv))))))

(defun feats-unify (st a b)
  ;; unify with ?var behavior on values
  (let ((mp (make-hash-table :test 'eql)))
    (dolist (kv a) (setf (gethash (car kv) mp) (cdr kv)))
    (dolist (kv b)
      (let* ((k (car kv)) (vb (cdr kv))
             (va (gethash k mp :none)))
        (cond
          ((eq va :none) (setf (gethash k mp) vb))
          ((= va vb) nil)
          ((and (symtab-var-p st va) (not (symtab-var-p st vb)))
           (setf (gethash k mp) vb))
          ((and (not (symtab-var-p st va)) (symtab-var-p st vb))
           nil)
          ((and (symtab-var-p st va) (symtab-var-p st vb))
           nil)
          (t (return-from feats-unify nil)))))
    (let (out)
      (maphash (lambda (k v) (push (cons k v) out)) mp)
      (feats-norm out))))

(defun feats-require (st fs k v)
  (feats-unify st fs (feats-norm (list (cons k v)))))

(defun feats-replace-value (fs from-v to-v)
  (feats-norm
   (mapcar (lambda (kv)
             (cons (car kv) (if (= (cdr kv) from-v) to-v (cdr kv))))
           fs)))

;; -----------------------------
;; Data structures
;; -----------------------------

(defstruct lex-entry pos weight feats)   ;; pos: id, feats: list
(defstruct rule lhs rhs-len rhs1 rhs2 weight op arg-key arg-val arg-type prop-idx-right)

(defstruct token raw text idx)

(defstruct node label is-leaf leaf-raw feats score left right child)

(defstruct item cat feats feats-h score node)

;; Arena as vector of nodes
(defstruct (arena (:constructor make-arena))
  (vec (make-array 0 :adjustable t :fill-pointer 0)))

(defun arena-add (ar n)
  (let ((id (fill-pointer (arena-vec ar))))
    (vector-push-extend n (arena-vec ar))
    id))

(defun arena-get (ar id)
  (aref (arena-vec ar) id))

(defun arena-clone-replace (ar root-id from-v to-v)
  (labels ((clone (nid)
             (let* ((n0 (arena-get ar nid))
                    (feats2 (feats-replace-value (node-feats n0) from-v to-v))
                    (child2 (and (node-child n0) (clone (node-child n0))))
                    (left2  (and (node-left  n0) (clone (node-left  n0))))
                    (right2 (and (node-right n0) (clone (node-right n0))))
                    (n2 (make-node :label (node-label n0)
                                   :is-leaf (node-is-leaf n0)
                                   :leaf-raw (node-leaf-raw n0)
                                   :feats feats2
                                   :score (node-score n0)
                                   :left left2 :right right2 :child child2)))
               (arena-add ar n2))))
    (clone root-id)))

(defun arena-pretty (st ar root-id)
  (labels ((feat-kv (kv)
             (format nil "~a=~a" (symtab-str st (car kv)) (symtab-str st (cdr kv))))
           (rec (nid ind)
             (let* ((n (arena-get ar nid))
                    (pad (make-string (* 2 ind) :initial-element #\Space)))
               (if (node-is-leaf n)
                   (format nil "~a~a~%" pad (or (node-leaf-raw n) ""))
                   (let* ((label (symtab-str st (node-label n)))
                          (featstr (if (null (node-feats n)) ""
                                       (if (null (node-feats n)) ""
                                           (if (endp (node-feats n)) ""
                                               (format nil " [~{~a~^, ~}]"
                                                       (mapcar #'feat-kv (node-feats n)))))))
                          (head (format nil "~a~a~a  (score=~a)~%"
                                        pad label featstr (format-f (node-score n) 3))))
                     (cond
                       ((node-child n)
                        (concatenate 'string head (rec (node-child n) (1+ ind))))
                       (t
                        (let ((out head))
                          (when (node-left n)
                            (setf out (concatenate 'string out (rec (node-left n) (1+ ind)))))
                          (when (node-right n)
                            (setf out (concatenate 'string out (rec (node-right n) (1+ ind)))))
                          out))))))))
    (rec root-id 0)))

;; -----------------------------
;; Bucket / Cell (beam + dedupe)
;; -----------------------------

(defstruct bucket items hashes) ;; items: list desc score, hashes: hash-table feats-h -> t

(defun make-bucket ()
  (make-bucket :items '() :hashes (make-hash-table :test 'eql)))

(defun bucket-has-hash-p (bk h)
  (gethash h (bucket-hashes bk)))

(defun insert-desc (it items)
  (cond
    ((null items) (list it))
    ((> (item-score it) (item-score (car items))) (cons it items))
    (t (cons (car items) (insert-desc it (cdr items))))))

(defun bucket-insert (bk it beam)
  (setf (gethash (item-feats-h it) (bucket-hashes bk)) t)
  (setf (bucket-items bk) (insert-desc it (bucket-items bk)))
  (let ((len (length (bucket-items bk))))
    (if (> len beam)
        (let ((pr (- len beam)))
          (setf (bucket-items bk) (subseq (bucket-items bk) 0 beam))
          pr)
        0)))

(defun cell-add-item (cell it beam)
  ;; cell: hash-table cat -> bucket
  (let* ((cat (item-cat it))
         (bk (or (gethash cat cell)
                 (setf (gethash cat cell) (make-bucket)))))
    (if (bucket-has-hash-p bk (item-feats-h it))
        (values 0 nil)
        (values (bucket-insert bk it beam) t))))

;; -----------------------------
;; Tokenización
;; -----------------------------

(defun split-sentences (text)
  (let ((out '())
        (start 0)
        (n (length text)))
    (labels ((flush (end)
               (let ((s (string-trim " \t" (subseq text start end))))
                 (when (> (length s) 0) (push s out)))))
      (loop for i from 0 below n
            for ch = (char text i) do
              (when (or (char= ch #\.) (char= ch #\Newline) (char= ch #\Return))
                (flush i)
                (setf start (1+ i))))
      (flush n))
    (nreverse out)))

(defun enclitic-split (raw low idx)
  (let* ((clitics '("me" "te" "se" "lo" "la" "los" "las" "le" "les" "nos" "os"))
         (best nil))
    (dolist (c clitics)
      (when (string-suffix-p c low)
        (when (or (null best) (> (length c) (length best)))
          (setf best c))))
    (when best
      (let* ((lw (length low))
             (lc (length best))
             (base-len (- lw lc)))
        (when (> base-len 2)
          (let ((base (subseq low 0 base-len)))
            (when (or (string-suffix-p "ar" base) (string-suffix-p "er" base) (string-suffix-p "ir" base)
                      (string-suffix-p "ando" base) (string-suffix-p "iendo" base))
              (let* ((raw-base (subseq raw 0 base-len))
                     (raw-cl   (subseq raw base-len))
                     (t1 (make-token :raw raw-base :text (string-downcase* raw-base) :idx idx))
                     (t2 (make-token :raw raw-cl   :text (string-downcase* raw-cl)   :idx (1+ idx))))
                (list t1 t2)))))))))

(defun tokenize (sent)
  (let ((tokens '())
        (idx 0)
        (i 0)
        (n (length sent)))
    (labels ((skip-nonletter ()
               (loop while (and (< i n) (not (letterp* (char sent i))))
                     do (incf i)))
             (take-word ()
               (let ((start i))
                 (incf i)
                 (loop while (and (< i n) (word-char-p (char sent i)))
                       do (incf i))
                 (subseq sent start i))))
      (loop while (< i n) do
        (skip-nonletter)
        (when (>= i n) (return))
        (let* ((raw (take-word))
               (low (string-downcase* raw)))
          (cond
            ((string= low "al")
             (push (make-token :raw "a"  :text "a"  :idx idx) tokens) (incf idx)
             (push (make-token :raw "el" :text "el" :idx idx) tokens) (incf idx))
            ((string= low "del")
             (push (make-token :raw "de" :text "de" :idx idx) tokens) (incf idx)
             (push (make-token :raw "el" :text "el" :idx idx) tokens) (incf idx))
            (t
             (let ((pair (enclitic-split raw low idx)))
               (if pair
                   (progn
                     (push (second pair) tokens)
                     (push (first pair) tokens)
                     (incf idx 2))
                   (progn
                     (push (make-token :raw raw :text low :idx idx) tokens)
                     (incf idx)))))))))
    (nreverse tokens)))

;; -----------------------------
;; OOV guess
;; -----------------------------

(defun det-word-p (low)
  (member low '("el" "la" "los" "las") :test #'string=))

(defun guess-lex (st c tok)
  ;; returns list of lex-entry (pos-id weight feats)
  (let* ((raw (token-raw tok))
         (low (token-text tok))
         (out '()))
    ;; PropN: inicial mayúscula y no determinante
    (when (and (> (length raw) 0)
               (upper-case-p (char raw 0))
               (not (det-word-p low)))
      (push (make-lex-entry :pos (getf c :PropN) :weight 0.03
                            :feats (feats-norm (list (cons (getf c :num) (getf c :sg)))))
            out))
    ;; Adv -mente
    (when (string-suffix-p "mente" low)
      (push (make-lex-entry :pos (getf c :Adv) :weight -0.03 :feats '()) out))
    ;; verb guesses
    (let ((base (feats-norm (list (cons (getf c :fin) (getf c :no))
                                  (cons (getf c :obl) (getf c :no))))))
      (cond
        ((or (string-suffix-p "ar" low) (string-suffix-p "er" low) (string-suffix-p "ir" low))
         (push (make-lex-entry :pos (getf c :Vi) :weight -0.12 :feats base) out)
         (push (make-lex-entry :pos (getf c :Vt) :weight -0.14 :feats base) out))
        ((or (string-suffix-p "ando" low) (string-suffix-p "iendo" low))
         (push (make-lex-entry :pos (getf c :Vi) :weight -0.14 :feats base) out)
         (push (make-lex-entry :pos (getf c :Vt) :weight -0.16 :feats base) out))))
    ;; fallback N
    (when (null out)
      (let ((vg (symtab-intern st "?g"))
            (vn (symtab-intern st "?n")))
        (push (make-lex-entry :pos (getf c :N) :weight -0.35
                              :feats (feats-norm (list (cons (getf c :gen) vg)
                                                       (cons (getf c :num) vn)))))
              out)))
    (nreverse out)))

;; -----------------------------
;; Load JSON lexicon / grammar
;; -----------------------------

(defun json-read-file (path)
  (let* ((yason (find-package :yason))
         (parse (and yason (find-symbol "PARSE" yason))))
    (unless (and parse (fboundp parse))
      (error "Falta yason. Instalar Quicklisp y (ql:quickload :yason)."))
    (with-open-file (in path :direction :input :external-format :utf-8)
      (funcall (symbol-function parse) in))))

(defun load-lexicon (st path)
  ;; returns hash-table word(string) -> list lex-entry (pos-id weight feats)
  (let* ((root (json-read-file path))
         (entries (gethash "entries" root))
         (lex (make-hash-table :test 'equal)))
    (maphash
     (lambda (word arr)
       (let ((ls '()))
         (dolist (obj arr)
           (let* ((pos (gethash "pos" obj))
                  (w (coerce (gethash "weight" obj) 'double-float))
                  (posid (symtab-intern st pos))
                  (fdict (or (gethash "feats" obj) (make-hash-table :test 'equal)))
                  (fs '()))
             (maphash (lambda (k v)
                        (push (cons (symtab-intern st k) (symtab-intern st v)) fs))
                      fdict)
             (push (make-lex-entry :pos posid :weight w :feats (feats-norm fs)) ls)))
         (setf (gethash word lex) (nreverse ls))))
     entries)
    lex))

(defun op-from (s)
  ;; keep as string, compare by string=
  s)

(defun load-grammar (st path)
  ;; returns plist: :rules list, :unary hash rhs1->rules, :binary hash (cons rhs1 rhs2)->rules
  (let* ((root (json-read-file path))
         (rulesj (gethash "rules" root))
         (rules '())
         (unary (make-hash-table :test 'eql))
         (binary (make-hash-table :test 'equal)))
    (dolist (r rulesj)
      (let* ((lhs (symtab-intern st (gethash "lhs" r)))
             (rhs (gethash "rhs" r))
             (len (length rhs))
             (rhs1 (symtab-intern st (first rhs)))
             (rhs2 (and (= len 2) (symtab-intern st (second rhs))))
             (w (coerce (gethash "weight" r) 'double-float))
             (op (op-from (or (gethash "op" r) "EMPTY")))
             (args (or (gethash "args" r) (make-hash-table :test 'equal)))
             (ak (and (gethash "key" args) (symtab-intern st (gethash "key" args))))
             (av (and (gethash "value" args) (symtab-intern st (gethash "value" args))))
             (at (and (gethash "type" args) (symtab-intern st (gethash "type" args))))
             (post (gethash "post" r))
             (prop (and post (member "PROPAGATE_IDX_TO_RIGHT" post :test #'string=))))
        (unless (or (= len 1) (= len 2))
          (error "grammar.json: rhs len debe ser 1 o 2"))
        (let ((ru (make-rule :lhs lhs :rhs-len len :rhs1 rhs1 :rhs2 rhs2 :weight w :op op
                             :arg-key ak :arg-val av :arg-type at :prop-idx-right (not (null prop)))))
          (push ru rules)
          (if (= len 1)
              (push ru (gethash rhs1 unary))
              (push ru (gethash (cons rhs1 rhs2) binary))))))
    (list :rules (nreverse rules) :unary unary :binary binary)))

(defun ensure-constants (st)
  (dolist (s '("idx" "?i" "gap" "obl" "yes" "fin" "no" "gen" "num" "sg"
               "TOK" "S" "VP_FIN" "Pinf" "VP_NF" "VP" "Cl"
               "N" "PropN" "Pron" "Adv" "Vi" "Vt"))
    (symtab-intern st s))
  (list
   :idx (symtab-intern st "idx")
   :qi  (symtab-intern st "?i")
   :gap (symtab-intern st "gap")
   :obl (symtab-intern st "obl")
   :yes (symtab-intern st "yes")
   :fin (symtab-intern st "fin")
   :no  (symtab-intern st "no")
   :gen (symtab-intern st "gen")
   :num (symtab-intern st "num")
   :sg  (symtab-intern st "sg")
   :TOK (symtab-intern st "TOK")
   :S   (symtab-intern st "S")
   :VP_FIN (symtab-intern st "VP_FIN")
   :Pinf   (symtab-intern st "Pinf")
   :VP_NF  (symtab-intern st "VP_NF")
   :VP     (symtab-intern st "VP")
   :Cl     (symtab-intern st "Cl")
   :N      (symtab-intern st "N")
   :PropN  (symtab-intern st "PropN")
   :Pron   (symtab-intern st "Pron")
   :Adv    (symtab-intern st "Adv")
   :Vi     (symtab-intern st "Vi")
   :Vt     (symtab-intern st "Vt")))

;; -----------------------------
;; Ops DSL
;; -----------------------------

(defun apply-op (st c rule lf rf)
  (let ((op (rule-op rule)))
    (cond
      ((string= op "EMPTY") '())
      ((string= op "LEFT") lf)
      ((string= op "RIGHT") rf)
      ((string= op "UNIFY") (feats-unify st lf rf))
      ((string= op "REQUIRE_LEFT")
       (and (rule-arg-key rule) (rule-arg-val rule)
            (feats-require st lf (rule-arg-key rule) (rule-arg-val rule))))
      ((string= op "REQUIRE_RIGHT")
       (and (rule-arg-key rule) (rule-arg-val rule)
            (feats-require st rf (rule-arg-key rule) (rule-arg-val rule))))
      ((string= op "MAKE_GAP")
       (and (rule-arg-type rule)
            (feats-norm (list (cons (getf c :idx) (getf c :qi))
                              (cons (getf c :gap) (rule-arg-type rule))))))
      ((string= op "RELCLAUSE_OBL")
       (and (feats-require st rf (getf c :obl) (getf c :yes))
            (feats-unify st lf (feats-norm (list (cons (getf c :gap) (getf c :obl)))))))
      (t nil))))

;; -----------------------------
;; Chart
;; chart = vector (n+1)*(n+1), each cell = hash-table cat->bucket
;; -----------------------------

(defun cidx (i j n) (+ (* i (1+ n)) j))

(defun chart-new (n)
  (let ((v (make-array (* (1+ n) (1+ n)))))
    (dotimes (k (length v))
      (setf (aref v k) (make-hash-table :test 'eql)))
    v))

(defun chart-get (chart i j n)
  (aref chart (cidx i j n)))

(defun chart-set (chart i j n cell)
  (setf (aref chart (cidx i j n)) cell))

;; -----------------------------
;; Unary closure
;; -----------------------------

(defun unary-closure (st c grammar cell arena beam)
  (let ((unary (getf grammar :unary))
        (pruned 0)
        (apps 0))
    (loop
      with changed-any = nil
      do (setf changed-any nil)
         ;; snapshot cats to avoid iterator invalidation
         (let ((cats '()))
           (maphash (lambda (k v) (declare (ignore v)) (push k cats)) cell)
           (dolist (rhs-cat cats)
             (let ((bk (gethash rhs-cat cell))
                   (rules (gethash rhs-cat unary)))
               (when (and bk rules)
                 (let ((items (copy-list (bucket-items bk))))
                   (dolist (ru rules)
                     (dolist (child items)
                       (let ((pf (apply-op st c ru (item-feats child) '())))
                         (when pf
                           (incf apps)
                           (let* ((score (+ (item-score child) (rule-weight ru)))
                                  (node (make-node :label (rule-lhs ru) :is-leaf nil :leaf-raw nil
                                                   :feats pf :score score
                                                   :left nil :right nil :child (item-node child)))
                                  (nid (arena-add arena node))
                                  (it (make-item :cat (rule-lhs ru) :feats pf :feats-h (feats-hash64 pf)
                                                 :score score :node nid)))
                             (multiple-value-bind (pr ch) (cell-add-item cell it beam)
                               (incf pruned pr)
                               (when ch (setf changed-any t))))))))))))))
      until (not changed-any))
    (values cell arena pruned apps)))

;; -----------------------------
;; Sanity checks
;; -----------------------------

(defun has-desc-label-p (arena node-id label)
  (let ((n (arena-get arena node-id)))
    (or (and (not (node-is-leaf n)) (= (node-label n) label))
        (and (node-child n) (has-desc-label-p arena (node-child n) label))
        (and (node-left n)  (has-desc-label-p arena (node-left n) label))
        (and (node-right n) (has-desc-label-p arena (node-right n) label)))))

(defun sanity-s-has-vpfin-p (arena c root-id)
  (let ((s (getf c :S)) (vpfin (getf c :VP_FIN)))
    (labels ((rec (nid)
               (let ((n (arena-get arena nid)))
                 (or (and (not (node-is-leaf n))
                          (= (node-label n) s)
                          (or (and (node-child n) (= (node-label (arena-get arena (node-child n))) vpfin))
                              (and (node-left  n) (= (node-label (arena-get arena (node-left n))) vpfin))
                              (and (node-right n) (= (node-label (arena-get arena (node-right n))) vpfin))))
                     (and (node-child n) (rec (node-child n)))
                     (and (node-left  n) (rec (node-left n)))
                     (and (node-right n) (rec (node-right n)))))))
      (rec root-id))))

(defun sanity-sin-takes-vpnf-p (arena c root-id)
  (let ((pinf (getf c :Pinf)) (vpnf (getf c :VP_NF)))
    (labels ((rec (nid)
               (let ((n (arena-get arena nid)))
                 (and (if (and (not (node-is-leaf n)) (= (node-label n) pinf))
                          (has-desc-label-p arena nid vpnf)
                          t)
                      (or (null (node-child n)) (rec (node-child n)))
                      (or (null (node-left n))  (rec (node-left n)))
                      (or (null (node-right n)) (rec (node-right n)))))))
      (rec root-id))))

(defun sanity-enclitic-only-nf-p (arena c root-id)
  (let ((vp (getf c :VP)) (cl (getf c :Cl)) (vt (getf c :Vt)) (vi (getf c :Vi)))
    (labels ((rec (nid)
               (let ((n (arena-get arena nid)))
                 (and (not (and (not (node-is-leaf n))
                                (= (node-label n) vp)
                                (node-left n) (node-right n)
                                (let ((ln (arena-get arena (node-left n)))
                                      (rn (arena-get arena (node-right n))))
                                  (and (not (node-is-leaf rn)) (= (node-label rn) cl)
                                       (not (node-is-leaf ln)) (or (= (node-label ln) vt) (= (node-label ln) vi))))))
                      (or (null (node-child n)) (rec (node-child n)))
                      (or (null (node-left n))  (rec (node-left n)))
                      (or (null (node-right n)) (rec (node-right n)))))))
      (rec root-id))))

;; -----------------------------
;; Parsing
;; -----------------------------

(defun emit-lex (st c tok le arena cell beam tids)
  (let* ((pos (lex-entry-pos le))
         (w (lex-entry-weight le))
         (fs0 (lex-entry-feats le))
         (fs1 fs0))
    (when (or (= pos (getf c :N)) (= pos (getf c :PropN)) (= pos (getf c :Pron)))
      (when (null (feats-find fs0 (getf c :idx)))
        (let* ((tid (gethash (token-idx tok) tids))
               (uni (feats-unify st fs0 (feats-norm (list (cons (getf c :idx) tid))))))
          (when uni (setf fs1 uni)))))
    (let* ((leaf (make-node :label (getf c :TOK) :is-leaf t :leaf-raw (token-raw tok)
                            :feats '() :score w :left nil :right nil :child nil))
           (leaf-id (arena-add arena leaf))
           (pre (make-node :label pos :is-leaf nil :leaf-raw nil :feats fs1 :score w
                           :left nil :right nil :child leaf-id))
           (pre-id (arena-add arena pre))
           (it (make-item :cat pos :feats fs1 :feats-h (feats-hash64 fs1) :score w :node pre-id)))
      (cell-add-item cell it beam))))

(defun parse-sentence (st c lex grammar sent &key (beam 16) (topk 1) (want-trees nil) (want-print nil))
  (let* ((t0 (get-internal-real-time))
         (toks (tokenize sent))
         (n (length toks)))
    (when (= n 0)
      (return-from parse-sentence
        (values (list :sentence sent :tokens 0 :oovTokens 0 :parsed nil :nParsesReturned 0
                      :bestScore nil :timeMs 0.0
                      :chartItemsTotal 0 :chartItemsMaxCell 0 :prunedByBeam 0
                      :unaryApplications 0 :ambiguousCells 0
                      :sanitySHasVpFin nil :sanitySinTakesVpNf nil :sanityEncliticOnlyNf nil
                      :notes (list "empty") :bestTree nil)
                0 0 0 0.0)))

    ;; tids
    (let ((tids (make-hash-table :test 'eql)))
      (dotimes (i n)
        (setf (gethash i tids) (symtab-intern st (format nil "t~d" i))))

      (let* ((chart (chart-new n))
             (arena (make-arena))
             (oov 0)
             (pruned-total 0)
             (unary-total 0))

        ;; lexical init
        (dotimes (i n)
          (let* ((tok (nth i toks))
                 (cell (chart-get chart i (1+ i) n))
                 (entries (gethash (token-text tok) lex)))
            (unless entries
              (incf oov)
              (setf entries (guess-lex st c tok)))
            (dolist (le entries)
              (multiple-value-bind (pr ch) (emit-lex st c tok le arena cell beam tids)
                (declare (ignore ch))
                (incf pruned-total pr)))
            (multiple-value-bind (cell2 arena2 pr2 u2) (unary-closure st c grammar cell arena beam)
              (declare (ignore arena2))
              (incf pruned-total pr2)
              (incf unary-total u2)
              (chart-set chart i (1+ i) n cell2))))

        ;; CKY spans
        (let ((binary (getf grammar :binary)))
          (loop for span from 2 to n do
            (loop for i from 0 to (- n span) do
              (let* ((j (+ i span))
                     (cell (chart-get chart i j n)))
                (loop for k from (1+ i) to (1- j) do
                  (let ((lcell (chart-get chart i k n))
                        (rcell (chart-get chart k j n)))
                    (when (and (> (hash-table-count lcell) 0) (> (hash-table-count rcell) 0))
                      (maphash
                       (lambda (catl bkl)
                         (declare (ignore bkl))
                         (maphash
                          (lambda (catr bkr)
                            (declare (ignore bkr))
                            (let ((rules (gethash (cons catl catr) binary)))
                              (when rules
                                (let ((itemsL (bucket-items (gethash catl lcell)))
                                      (itemsR (bucket-items (gethash catr rcell))))
                                  (dolist (ru rules)
                                    (dolist (il itemsL)
                                      (dolist (ir itemsR)
                                        (let ((pf (apply-op st c ru (item-feats il) (item-feats ir))))
                                          (when pf
                                            (let* ((score (+ (item-score il) (item-score ir) (rule-weight ru)))
                                                   (right-node (item-node ir)))
                                              (when (rule-prop-idx-right ru)
                                                (let ((idxv (feats-find (item-feats il) (getf c :idx))))
                                                  (when idxv
                                                    (setf right-node (arena-clone-replace arena (item-node ir) (getf c :qi) idxv)))))
                                              (let* ((node (make-node :label (rule-lhs ru) :is-leaf nil :leaf-raw nil
                                                                      :feats pf :score score
                                                                      :left (item-node il) :right right-node :child nil))
                                                     (nid (arena-add arena node))
                                                     (it (make-item :cat (rule-lhs ru) :feats pf :feats-h (feats-hash64 pf)
                                                                    :score score :node nid)))
                                                (multiple-value-bind (pr ch) (cell-add-item cell it beam)
                                                  (declare (ignore ch))
                                                  (incf pruned-total pr)))))))))))))))
                          rcell))
                       lcell))))
                (multiple-value-bind (cell2 arena2 pr2 u2) (unary-closure st c grammar cell arena beam)
                  (declare (ignore arena2))
                  (incf pruned-total pr2)
                  (incf unary-total u2)
                  (chart-set chart i j n cell2))))))

        ;; metrics
        (let ((tot-items 0) (max-cell 0) (amb 0))
          (dotimes (ii (1+ n))
            (dotimes (jj (1+ n))
              (let* ((cell (chart-get chart ii jj n))
                     (count 0))
                (when (>= (hash-table-count cell) 2) (incf amb))
                (maphash (lambda (_cat bk)
                           (declare (ignore _cat))
                           (incf count (length (bucket-items bk))))
                         cell)
                (incf tot-items count)
                (when (> count max-cell) (setf max-cell count)))))

          ;; best S
          (let* ((cell-sn (chart-get chart 0 n n))
                 (bkS (gethash (getf c :S) cell-sn))
                 (best-it (and bkS (car (bucket-items bkS)))))
            (let ((parsed (not (null best-it)))
                  (best-score (and best-it (item-score best-it)))
                  (nret 0)
                  (notes '())
                  (best-tree nil)
                  (s1 nil) (s2 nil) (s3 nil))
              (if (not parsed)
                  (setf notes (list "NO_PARSE"))
                  (progn
                    (let* ((itemsS (bucket-items bkS))
                           (lenS (length itemsS))
                           (tree-id (item-node best-it)))
                      (setf nret (min topk lenS))
                      (setf s1 (sanity-s-has-vpfin-p arena c tree-id))
                      (setf s2 (sanity-sin-takes-vpnf-p arena c tree-id))
                      (setf s3 (sanity-enclitic-only-nf-p arena c tree-id))
                      (unless s1 (push "WARN: S sin VP_FIN visible" notes))
                      (unless s2 (push "WARN: 'sin' sin VP_NF bajo Pinf" notes))
                      (unless s3 (push "WARN: enclítico con verbo finito" notes))
                      (setf notes (nreverse notes))
                      (when want-trees
                        (setf best-tree (arena-pretty st arena tree-id))))))

              (let* ((t1 (get-internal-real-time))
                     (time-ms (* 1000.0 (/ (- t1 t0) internal-time-units-per-second))))
                (when want-print
                  (format t "==============================================================================~%")
                  (format t "~a~%" sent)
                  (format t "tokens=~d  oov=~d  parsed=~d  parses=~d  bestScore=~a  time_ms=~a~%"
                          n oov (if parsed 1 0) nret (if best-score (format-f best-score 6) "null") (format-f time-ms 1))
                  (format t "chart_items=~d  max_cell=~d  pruned=~d  unary_apps=~d  amb_cells=~d~%"
                          tot-items max-cell pruned-total unary-total amb)
                  (when notes (format t "notes: ~{~a~^; ~}~%" notes))
                  (when (and want-trees best-tree) (format t "~a" best-tree)))

                (values
                 (list :sentence sent
                       :tokens n
                       :oovTokens oov
                       :parsed parsed
                       :nParsesReturned nret
                       :bestScore best-score
                       :timeMs time-ms
                       :chartItemsTotal tot-items
                       :chartItemsMaxCell max-cell
                       :prunedByBeam pruned-total
                       :unaryApplications unary-total
                       :ambiguousCells amb
                       :sanitySHasVpFin s1
                       :sanitySinTakesVpNf s2
                       :sanityEncliticOnlyNf s3
                       :notes notes
                       :bestTree best-tree)
                 (if parsed 1 0) n oov time-ms))))))))))

;; -----------------------------
;; CLI + JSON output
;; -----------------------------

(defun get-argv ()
  (let* ((uiop (find-package :uiop))
         (fn (and uiop (find-symbol "COMMAND-LINE-ARGUMENTS" uiop))))
    (if (and fn (fboundp fn))
        (funcall (symbol-function fn))
        '())))

(defun parse-cli (argv)
  (let ((opts (list :lex "lexicon.json"
                    :grammar "grammar.json"
                    :file "corpus.txt"
                    :text nil
                    :json nil
                    :beam 16
                    :topk 1
                    :trees nil
                    :print nil)))
    (labels ((setopt (k v) (setf (getf opts k) v)))
      (loop for i from 0 below (length argv) do
        (let ((a (nth i argv)))
          (cond
            ((string= a "--help")
             (format t "Uso: sbcl --script parser_v7.lisp -- [--lex lexicon.json] [--grammar grammar.json] [--file corpus.txt | --text \"...\"] [--beam 16] [--topk 1] [--trees] [--print] [--json out.json]~%")
             (quit))
            ((string= a "--lex")     (setopt :lex (nth (incf i) argv)))
            ((string= a "--grammar") (setopt :grammar (nth (incf i) argv)))
            ((string= a "--file")    (setopt :file (nth (incf i) argv)))
            ((string= a "--text")    (setopt :text (nth (incf i) argv)))
            ((string= a "--json")    (setopt :json (nth (incf i) argv)))
            ((string= a "--beam")    (setopt :beam (parse-integer (nth (incf i) argv))))
            ((string= a "--topk")    (setopt :topk (parse-integer (nth (incf i) argv))))
            ((string= a "--trees")   (setopt :trees t))
            ((string= a "--print")   (setopt :print t))
            (t (format t "Arg desconocido: ~a~%" a) (quit 2))))))
    opts))

(defun read-file-utf8 (path)
  (with-open-file (in path :direction :input :external-format :utf-8)
    (let ((s (make-string (file-length in))))
      (read-sequence s in)
      s)))

(defun plist->hash (pl)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pl by #'cddr do
      (setf (gethash (string-downcase* (subseq (symbol-name k) 1)) h) v))
    h))

(defun write-json-file (path obj)
  (let* ((yason (find-package :yason))
         (encode (and yason (find-symbol "ENCODE" yason))))
    (unless (and encode (fboundp encode)) (error "yason no disponible para escribir JSON"))
    (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
      (funcall (symbol-function encode) obj out))))

(defun main ()
  (let* ((argv (get-argv))
         ;; SBCL passes script args including "--" sometimes; keep everything after "--" if present.
         (sep (position "--" argv :test #'string=))
         (args (if sep (subseq argv (1+ sep)) argv))
         (opts (parse-cli args))
         (st (make-symtab))
         (lex (load-lexicon st (getf opts :lex)))
         (grammar (load-grammar st (getf opts :grammar)))
         (c (ensure-constants st))
         (text (or (getf opts :text) (read-file-utf8 (getf opts :file))))
         (sents (split-sentences text))
         (rows '())
         (parsed 0)
         (tot-tok 0)
         (tot-oov 0)
         (tot-time 0.0))
    (dolist (s sents)
      (multiple-value-bind (row parsed01 tok oov time-ms)
          (parse-sentence st c lex grammar s
                         :beam (getf opts :beam)
                         :topk (getf opts :topk)
                         :want-trees (getf opts :trees)
                         :want-print (getf opts :print))
        (push row rows)
        (incf parsed parsed01)
        (incf tot-tok tok)
        (incf tot-oov oov)
        (incf tot-time time-ms)))
    (setf rows (nreverse rows))
    (let* ((sn (length sents))
           (coverage (if (= sn 0) 0.0 (/ (float parsed) sn)))
           (avg-tok (if (= sn 0) 0.0 (/ (float tot-tok) sn)))
           (avg-oov (if (= sn 0) 0.0 (/ (float tot-oov) sn)))
           (avg-time (if (= sn 0) 0.0 (/ tot-time sn))))
      (if (getf opts :print)
          (progn
            (format t "==============================================================================~%")
            (format t "SUMMARY~%")
            (format t "sentences=~d  coverage=~a  avg_tokens=~a  avg_oov=~a  avg_time_ms=~a  beam=~d  top_k=~d~%"
                    sn (format-f coverage 3) (format-f avg-tok 2) (format-f avg-oov 2) (format-f avg-time 1)
                    (getf opts :beam) (getf opts :topk)))
          (format t "SUMMARY: sentences=~d coverage=~a avg_time_ms=~a beam=~d top_k=~d~%"
                  sn (format-f coverage 3) (format-f avg-time 1) (getf opts :beam) (getf opts :topk)))
      (when (getf opts :json)
        ;; JSON: build simple hash-tables for yason
        (let ((rows-json (mapcar #'plist->hash rows))
              (sum (make-hash-table :test 'equal)))
          (setf (gethash "sentences" sum) sn)
          (setf (gethash "coverage" sum) coverage)
          (setf (gethash "avgTokens" sum) avg-tok)
          (setf (gethash "avgOov" sum) avg-oov)
          (setf (gethash "totalTimeMs" sum) tot-time)
          (setf (gethash "avgTimeMs" sum) avg-time)
          (setf (gethash "beam" sum) (getf opts :beam))
          (setf (gethash "topK" sum) (getf opts :topk))
          (setf (gethash "rows" sum) rows-json)
          (write-json-file (getf opts :json) sum)
          (format t "Wrote JSON: ~a~%" (getf opts :json)))))))

;; entry point
(main)
