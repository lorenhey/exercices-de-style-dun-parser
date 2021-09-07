# ParserV7.R
# CKY + beam + unary-closure + features + constraints
# + modo incremental real: resetIncremental(), step(token), bestTree()

suppressWarnings({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Falta jsonlite. Instalá con: install.packages('jsonlite')")
  }
})

ParserV7 <- R6::R6Class(
  "ParserV7",
  public = list(
    beam = 8,

    # symbol table
    sym2id = NULL,  # env: string -> int
    id2sym = NULL,  # character vector 1-based
    startSym = 0L,

    # lexicon: word -> list(entries)
    lexByWord = NULL,

    # grammar rules
    rules = NULL,

    # indices
    unaryIndex = NULL,   # env: rhs1 -> integer vector of rule idx
    binaryIndex = NULL,  # env: "rhs1,rhs2" -> integer vector

    # arena nodes (1-based)
    arena = NULL,

    # incremental state
    incTokens = NULL,
    incChart  = NULL,    # list of rows; chart[[i]][[j]] is a cell for span [i,j)

    initialize = function(grammarPath, lexiconPath, beam = 8L) {
      self$beam <- as.integer(max(1L, beam))

      self$sym2id <- new.env(parent = emptyenv())
      self$id2sym <- character(0)

      self$lexByWord <- new.env(parent = emptyenv())
      self$rules <- list()

      self$unaryIndex  <- new.env(parent = emptyenv())
      self$binaryIndex <- new.env(parent = emptyenv())

      self$arena <- list(NULL)

      self$loadResources(grammarPath, lexiconPath)
      self$resetIncremental()
    },

    # -------------------------
    # Public API
    # -------------------------
    loadResources = function(grammarPath, lexiconPath) {
      g <- private$readJson(grammarPath)
      l <- private$readJson(lexiconPath)

      start <- g$start %||% g$root %||% g$start_symbol %||% "S"
      self$startSym <- private$intern(start)

      private$loadLexicon(l)
      private$loadGrammar(g)
      private$buildRuleIndex()

      invisible(TRUE)
    },

    parseSentence = function(sentence) {
      toks0 <- private$tokenize(sentence)
      toks  <- private$splitMorphology(toks0)
      n <- length(toks)
      if (n == 0) return("")

      self$arena <- list(NULL)

      # chart[[i]][[j]] span [i,j), i:1..n, j:2..n+1
      chart <- vector("list", n)
      for (i in seq_len(n)) chart[[i]] <- vector("list", n + 1)

      # seed lexical
      for (i in seq_len(n)) {
        cell <- private$initCell()
        private$seedLexical(cell, toks[[i]])
        private$unaryClosure(cell)
        chart[[i]][[i + 1]] <- cell
      }

      # CKY
      for (span in 2:n) {
        for (i in 1:(n - span + 1)) {
          j <- i + span
          cellIJ <- private$initCell()

          for (k in (i + 1):(j - 1)) {
            left  <- chart[[i]][[k]]
            right <- chart[[k]][[j]]
            if (is.null(left) || is.null(right)) next
            private$combineCells(cellIJ, left, right)
          }

          private$unaryClosure(cellIJ)
          chart[[i]][[j]] <- cellIJ
        }
      }

      rootCell <- chart[[1]][[n + 1]]
      best <- private$pickBestRoot(rootCell)
      if (is.null(best)) return("")
      private$renderTree(best$node)
    },

    # -------------------------
    # Incremental real API
    # -------------------------
    resetIncremental = function() {
      self$incTokens <- character(0)
      self$incChart  <- list()
      self$arena     <- list(NULL)
      invisible(TRUE)
    },

    step = function(token) {
      # admite token con puntuación, o incluso varios tokens
      parts <- private$tokenize(token)
      if (length(parts) == 0) return(self$bestTree())

      for (p in parts) {
        m <- private$splitMorphology(list(p))
        for (t in m) private$stepOne(t)
      }
      self$bestTree()
    },

    bestTree = function() {
      n <- length(self$incTokens)
      if (n == 0) return("")
      rootCell <- self$incChart[[1]][[n + 1]] %||% NULL
      best <- private$pickBestRoot(rootCell)
      if (is.null(best)) return("")
      private$renderTree(best$node)
    },

    parseSentenceIncremental = function(sentence) {
      self$resetIncremental()
      toks0 <- private$tokenize(sentence)
      toks  <- private$splitMorphology(toks0)
      for (t in toks) private$stepOne(t)
      self$bestTree()
    }
  ),

  private = list(
    # -------------------------
    # JSON
    # -------------------------
    readJson = function(path) {
      txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      jsonlite::fromJSON(txt, simplifyVector = FALSE)
    },

    # -------------------------
    # Tokenization / morphology
    # -------------------------
    tokenize = function(sentence) {
      s <- enc2utf8(as.character(sentence %||% ""))
      m <- gregexpr("[\\p{L}\\p{N}_]+", s, perl = TRUE)
      toks <- regmatches(s, m)[[1]]
      as.list(toks)
    },

    splitMorphology = function(toks0) {
      encl <- c("me","te","se","lo","la","los","las","le","les","nos","os")
      out <- character(0)

      for (t0 in toks0) {
        t <- tolower(enc2utf8(as.character(t0)))
        if (t == "al") { out <- c(out, "a", "el"); next }
        if (t == "del"){ out <- c(out, "de","el"); next }

        ms <- private$maybeSplitEnclitic(t, encl)
        if (nzchar(ms$suf)) out <- c(out, ms$base, ms$suf) else out <- c(out, t)
      }
      as.list(out)
    },

    maybeSplitEnclitic = function(tok, encl) {
      # heurística: termina en pronombre y antes termina en r/d/n
      for (suf in encl) {
        if (nchar(tok, type="chars") <= nchar(suf, type="chars") + 2) next
        if (!endsWith(tok, suf)) next
        cut <- nchar(tok, type="chars") - nchar(suf, type="chars")
        base <- substr(tok, 1, cut)
        last <- substr(base, nchar(base), nchar(base))
        if (last %in% c("r","d","n")) {
          return(list(base = base, suf = suf))
        }
      }
      list(base = tok, suf = "")
    },

    # -------------------------
    # Symbol table
    # -------------------------
    intern = function(s) {
      ss <- toupper(trimws(enc2utf8(as.character(s %||% ""))))
      if (!nzchar(ss)) return(0L)
      if (exists(ss, envir = self$sym2id, inherits = FALSE)) {
        return(get(ss, envir = self$sym2id, inherits = FALSE))
      }
      id <- as.integer(length(self$id2sym) + 1L)
      assign(ss, id, envir = self$sym2id)
      self$id2sym[id] <- ss
      id
    },

    symStr = function(id) {
      id <- as.integer(id)
      if (id <= 0L || id > length(self$id2sym)) return("<?>")
      self$id2sym[id]
    },

    # -------------------------
    # Lexicon / Grammar loaders
    # -------------------------
    loadLexicon = function(l) {
      self$lexByWord <- new.env(parent = emptyenv())
      entries <- l$entries %||% l$lexicon %||% list()
      if (!is.list(entries)) entries <- list()

      for (e in entries) {
        if (!is.list(e)) next
        w <- tolower(enc2utf8(as.character(e$word %||% e$form %||% "")))
        if (!nzchar(w)) next

        pos <- private$intern(e$pos %||% e$tag %||% "X")
        ww  <- e$weight %||% e$prob %||% 1.0
        ww  <- if (is.numeric(ww) && ww > 0) as.numeric(ww) else 1e-12

        feats <- private$readFeats(e)  # named int vector key->val
        sig   <- private$featSig(feats)

        entry <- list(
          word = w,
          pos  = pos,
          logw = log(ww),
          feats= feats,
          sig  = sig
        )

        bucket <- if (exists(w, envir=self$lexByWord, inherits=FALSE)) get(w, envir=self$lexByWord) else list()
        bucket[[length(bucket) + 1]] <- entry
        assign(w, bucket, envir=self$lexByWord)
      }
    },

    readFeats = function(e) {
      raw <- e$feats %||% e$features
      if (is.null(raw)) return(integer(0))

      feats <- integer(0)

      # raw puede venir como named list (mapping) o list de pares {k,v}
      if (is.list(raw) && !is.null(names(raw)) && any(nzchar(names(raw)))) {
        for (k in names(raw)) {
          kid <- private$intern(k)
          vid <- private$intern(as.character(raw[[k]]))
          if (kid != 0L && vid != 0L) feats[as.character(kid)] <- vid
        }
      } else if (is.list(raw)) {
        for (p in raw) {
          if (!is.list(p)) next
          if (is.null(p$k) || is.null(p$v)) next
          kid <- private$intern(p$k)
          vid <- private$intern(p$v)
          if (kid != 0L && vid != 0L) feats[as.character(kid)] <- vid
        }
      }

      # ordenar por key numérico
      if (length(feats) > 0) {
        ord <- order(as.integer(names(feats)))
        feats <- feats[ord]
      }
      feats
    },

    loadGrammar = function(g) {
      self$rules <- list()
      rules <- g$rules %||% g$productions %||% list()
      if (!is.list(rules)) rules <- list()

      for (r in rules) {
        if (!is.list(r)) next
        lhs <- private$intern(r$lhs %||% "<?>")

        rhs <- r$rhs %||% r$rhs_symbols
        rhsArr <- character(0)

        if (is.list(rhs) && length(rhs) > 0) {
          rhsArr <- vapply(rhs, function(x) as.character(x), character(1))
        } else {
          r1 <- r$rhs1; r2 <- r$rhs2
          if (!is.null(r1) && nzchar(trimws(as.character(r1)))) rhsArr <- c(rhsArr, as.character(r1))
          if (!is.null(r2) && nzchar(trimws(as.character(r2)))) rhsArr <- c(rhsArr, as.character(r2))
        }

        if (length(rhsArr) < 1 || length(rhsArr) > 2) next
        rhsIds <- vapply(rhsArr, private$intern, integer(1))

        ww <- r$weight %||% r$prob %||% 1.0
        ww <- if (is.numeric(ww) && ww > 0) as.numeric(ww) else 1e-12

        prop <- toupper(trimws(as.character(r$propagate %||% "MERGE")))
        if (!nzchar(prop)) prop <- "MERGE"

        csRaw <- r$constraints %||% r$conds %||% list()
        cs <- list()
        if (is.list(csRaw)) {
          for (c in csRaw) {
            if (!is.list(c)) next
            cs[[length(cs) + 1]] <- list(
              type   = toupper(as.character(c$type %||% "REQUIRE")),
              target = toupper(as.character(c$target %||% "LEFT")),
              target2= toupper(as.character(c$target2 %||% c$other %||% "RIGHT")),
              key    = private$intern(c$key %||% ""),
              val    = private$intern(c$value %||% ""),
              key2   = private$intern(c$key2 %||% "")
            )
          }
        }

        self$rules[[length(self$rules) + 1]] <- list(
          lhs    = lhs,
          rhsLen = length(rhsIds),
          rhs1   = rhsIds[[1]],
          rhs2   = if (length(rhsIds) == 2) rhsIds[[2]] else 0L,
          logw   = log(ww),
          prop   = prop,
          cs     = cs
        )
      }
    },

    buildRuleIndex = function() {
      self$unaryIndex  <- new.env(parent = emptyenv())
      self$binaryIndex <- new.env(parent = emptyenv())

      for (i in seq_along(self$rules)) {
        r <- self$rules[[i]]
        if (r$rhsLen == 1) {
          k <- as.character(r$rhs1)
          v <- if (exists(k, envir=self$unaryIndex, inherits=FALSE)) get(k, envir=self$unaryIndex) else integer(0)
          v <- c(v, as.integer(i))
          assign(k, v, envir=self$unaryIndex)
        } else {
          k <- paste0(r$rhs1, ",", r$rhs2)
          v <- if (exists(k, envir=self$binaryIndex, inherits=FALSE)) get(k, envir=self$binaryIndex) else integer(0)
          v <- c(v, as.integer(i))
          assign(k, v, envir=self$binaryIndex)
        }
      }
    },

    # -------------------------
    # Arena + render
    # -------------------------
    addLeafNode = function(label, leaf) {
      self$arena[[length(self$arena) + 1]] <- list(label=as.integer(label), left=0L, right=0L, isLeaf=TRUE, leaf=as.character(leaf))
      length(self$arena)
    },

    addUnaryNode = function(label, child) {
      self$arena[[length(self$arena) + 1]] <- list(label=as.integer(label), left=as.integer(child), right=0L, isLeaf=FALSE, leaf="")
      length(self$arena)
    },

    addBinaryNode = function(label, left, right) {
      self$arena[[length(self$arena) + 1]] <- list(label=as.integer(label), left=as.integer(left), right=as.integer(right), isLeaf=FALSE, leaf="")
      length(self$arena)
    },

    renderTree = function(nodeId) {
      n <- self$arena[[as.integer(nodeId)]]
      lab <- private$symStr(n$label)
      if (isTRUE(n$isLeaf)) {
        return(paste0("(", lab, " ", n$leaf, ")"))
      }
      if (as.integer(n$right) == 0L) {
        a <- private$renderTree(n$left)
        return(paste0("(", lab, " ", a, ")"))
      }
      a <- private$renderTree(n$left)
      b <- private$renderTree(n$right)
      paste0("(", lab, " ", a, " ", b, ")")
    },

    # -------------------------
    # CKY cell + items
    # -------------------------
    initCell = function() {
      list(items = list(), byKey = new.env(parent = emptyenv()))
    },

    featSig = function(feats) {
      if (length(feats) == 0) return("")
      keys <- as.integer(names(feats))
      ord <- order(keys)
      keys <- keys[ord]
      vals <- as.integer(feats)[ord]
      paste(paste0(keys, ":", vals), collapse = ",")
    },

    makeItem = function(cat, score, node, feats) {
      # feats: named int vector (keyId -> valId)
      if (length(feats) > 0) {
        ord <- order(as.integer(names(feats)))
        feats <- feats[ord]
      }
      list(
        cat = as.integer(cat),
        score = as.numeric(score),
        node = as.integer(node),
        feats = feats,
        sig = private$featSig(feats)
      )
    },

    sortTrim = function(cell) {
      if (length(cell$items) == 0) return(invisible(NULL))
      scores <- vapply(cell$items, function(it) it$score, numeric(1))
      ord <- order(scores, decreasing = TRUE)
      cell$items <- cell$items[ord]
      if (length(cell$items) > self$beam) cell$items <- cell$items[seq_len(self$beam)]

      # rebuild byKey
      cell$byKey <- new.env(parent = emptyenv())
      for (i in seq_along(cell$items)) {
        it <- cell$items[[i]]
        key <- paste0(it$cat, "|", it$sig)
        assign(key, as.integer(i), envir = cell$byKey)
      }
      invisible(NULL)
    },

    cellInsert = function(cell, it) {
      key <- paste0(it$cat, "|", it$sig)
      if (exists(key, envir=cell$byKey, inherits=FALSE)) {
        idx <- get(key, envir=cell$byKey, inherits=FALSE)
        cur <- cell$items[[idx]]
        if (it$score > cur$score) cell$items[[idx]] <- it
        private$sortTrim(cell)
        return(invisible(NULL))
      }
      cell$items[[length(cell$items) + 1]] <- it
      private$sortTrim(cell)
      invisible(NULL)
    },

    # -------------------------
    # Lexical seeding
    # -------------------------
    seedLexical = function(cell, word) {
      wSurface <- enc2utf8(as.character(word))
      w <- tolower(wSurface)

      entries <- if (exists(w, envir=self$lexByWord, inherits=FALSE)) get(w, envir=self$lexByWord) else NULL
      if (!is.null(entries) && length(entries) > 0) {
        for (e in entries) {
          node <- private$addLeafNode(e$pos, wSurface)
          it <- private$makeItem(e$pos, e$logw, node, e$feats)
          private$cellInsert(cell, it)
        }
        return(invisible(NULL))
      }

      # OOV fallback: si empieza con mayúscula => PROPN
      first <- substr(wSurface, 1, 1)
      isUpper <- grepl("^\\p{Lu}$", first, perl=TRUE)
      cat <- private$intern(if (isUpper) "PROPN" else "NOUN")
      node <- private$addLeafNode(cat, wSurface)
      it <- private$makeItem(cat, log(1e-6), node, integer(0))
      private$cellInsert(cell, it)
      invisible(NULL)
    },

    # -------------------------
    # Unary closure
    # -------------------------
    unaryClosure = function(cell) {
      changed <- TRUE
      iter <- 0L
      while (changed && iter < 64L) {
        iter <- iter + 1L
        changed <- FALSE
        snapshot <- cell$items

        for (src in snapshot) {
          k <- as.character(src$cat)
          if (!exists(k, envir=self$unaryIndex, inherits=FALSE)) next
          idxs <- get(k, envir=self$unaryIndex, inherits=FALSE)

          for (ri in idxs) {
            rule <- self$rules[[ri]]
            res <- private$applyUnaryConstraints(rule, src$feats)
            if (!res$ok) next

            node <- private$addUnaryNode(rule$lhs, src$node)
            it <- private$makeItem(rule$lhs, src$score + rule$logw, node, res$feats)

            before <- length(cell$items)
            private$cellInsert(cell, it)
            if (length(cell$items) > before) changed <- TRUE
          }
        }
      }
      invisible(NULL)
    },

    applyUnaryConstraints = function(rule, childFeats) {
      out <- childFeats
      for (c in rule$cs) {
        typ <- c$type
        if (typ == "REQUIRE") {
          key <- as.character(c$key)
          v <- if (key %in% names(out)) out[[key]] else 0L
          if (as.integer(v) != as.integer(c$val)) return(list(ok=FALSE, feats=out))
        } else if (typ == "ASSIGN") {
          if (c$key != 0L && c$val != 0L) out[as.character(c$key)] <- as.integer(c$val)
        }
      }
      if (length(out) > 0) out <- out[order(as.integer(names(out)))]
      list(ok=TRUE, feats=out)
    },

    # -------------------------
    # Combine (binary)
    # -------------------------
    combineCells = function(outCell, leftCell, rightCell) {
      for (L in leftCell$items) {
        for (R in rightCell$items) {
          key <- paste0(L$cat, ",", R$cat)
          if (!exists(key, envir=self$binaryIndex, inherits=FALSE)) next
          idxs <- get(key, envir=self$binaryIndex, inherits=FALSE)

          for (ri in idxs) {
            rule <- self$rules[[ri]]
            res <- private$applyBinaryConstraints(rule, L$feats, R$feats)
            if (!res$ok) next

            node <- private$addBinaryNode(rule$lhs, L$node, R$node)
            it <- private$makeItem(rule$lhs, L$score + R$score + rule$logw, node, res$feats)
            private$cellInsert(outCell, it)
          }
        }
      }
      invisible(NULL)
    },

    applyBinaryConstraints = function(rule, lf, rf) {
      # propagate base feats
      out <- integer(0)
      prop <- rule$prop

      if (prop == "LEFT") {
        out <- lf
      } else if (prop == "RIGHT") {
        out <- rf
      } else {
        out <- lf
        # merge: add missing from right
        for (k in names(rf)) {
          if (!(k %in% names(out))) out[k] <- rf[[k]]
        }
      }

      for (c in rule$cs) {
        typ <- c$type

        if (typ == "REQUIRE") {
          src <- if (c$target == "RIGHT") rf else lf
          k <- as.character(c$key)
          v <- if (k %in% names(src)) src[[k]] else 0L
          if (as.integer(v) != as.integer(c$val)) return(list(ok=FALSE, feats=out))

        } else if (typ == "UNIFY") {
          k <- as.character(c$key)
          v1 <- if (k %in% names(lf)) lf[[k]] else 0L
          v2 <- if (k %in% names(rf)) rf[[k]] else 0L
          if (v1 != 0L && v2 != 0L && as.integer(v1) != as.integer(v2)) return(list(ok=FALSE, feats=out))
          if (v1 != 0L) out[k] <- as.integer(v1) else if (v2 != 0L) out[k] <- as.integer(v2)

        } else if (typ == "AGREE") {
          k <- as.character(c$key)
          v1 <- if (k %in% names(lf)) lf[[k]] else 0L
          v2 <- if (k %in% names(rf)) rf[[k]] else 0L
          if (v1 == 0L || v2 == 0L || as.integer(v1) != as.integer(v2)) return(list(ok=FALSE, feats=out))
          out[k] <- as.integer(v1)

        } else if (typ == "ASSIGN") {
          if (c$key != 0L && c$val != 0L) out[as.character(c$key)] <- as.integer(c$val)
        }
      }

      if (length(out) > 0) out <- out[order(as.integer(names(out)))]
      list(ok=TRUE, feats=out)
    },

    # -------------------------
    # Root pick
    # -------------------------
    pickBestRoot = function(cell) {
      if (is.null(cell) || length(cell$items) == 0) return(NULL)
      best <- NULL
      for (it in cell$items) {
        if (as.integer(it$cat) == as.integer(self$startSym)) {
          if (is.null(best) || it$score > best$score) best <- it
        }
      }
      best
    },

    # -------------------------
    # Incremental core: stepOne()
    # -------------------------
    stepOne = function(token) {
      token <- enc2utf8(as.character(token))
      self$incTokens <- c(self$incTokens, token)

      n <- length(self$incTokens)
      j <- n + 1  # right boundary in [i,j) convention

      # ensure row for i = n
      if (length(self$incChart) < n) self$incChart[[n]] <- vector("list", n + 1)

      # 1) lexical cell [n, n+1)
      cell <- private$initCell()
      private$seedLexical(cell, token)
      private$unaryClosure(cell)
      self$incChart[[n]][[j]] <- cell

      # 2) build all spans ending at j: i = n-1 .. 1
      if (n >= 2) {
        for (i in (n - 1):1) {
          if (length(self$incChart) < i) self$incChart[[i]] <- vector("list", n + 1)
          if (is.null(self$incChart[[i]])) self$incChart[[i]] <- vector("list", n + 1)

          cellIJ <- private$initCell()

          # splits k in (i, j): k = i+1 .. j-1
          for (k in (i + 1):(j - 1)) {
            left  <- self$incChart[[i]][[k]] %||% NULL
            right <- self$incChart[[k]][[j]] %||% NULL
            if (is.null(left) || is.null(right)) next
            private$combineCells(cellIJ, left, right)
          }

          private$unaryClosure(cellIJ)
          self$incChart[[i]][[j]] <- cellIJ
        }
      }

      invisible(TRUE)
    }
  )
)

# util: null-coalescing for R
`%||%` <- function(a, b) if (!is.null(a)) a else b
