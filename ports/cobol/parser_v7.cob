       IDENTIFICATION DIVISION.
       PROGRAM-ID. NLPARSER7.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRAMMAR-FILE ASSIGN TO "resources/grammar.json"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LEXICON-FILE ASSIGN TO "resources/lexicon.json"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  GRAMMAR-FILE.
       01  GRAMMAR-LINE               PIC X(1024).

       FD  LEXICON-FILE.
       01  LEXICON-LINE               PIC X(1024).

       WORKING-STORAGE SECTION.

       77  MAX-TOKENS                 PIC 9(3) VALUE 64.
       77  MAX-RULES                  PIC 9(4) VALUE 2000.
       77  MAX-LEX                    PIC 9(4) VALUE 5000.
       77  MAX-FEATS                  PIC 9(2) VALUE 8.
       77  MAX-CONSTR                 PIC 9(2) VALUE 8.
       77  MAX-SYMS                   PIC 9(4) VALUE 4096.
       77  BEAM                       PIC 9(2) VALUE 8.
       77  MAX-NODES                  PIC 9(5) VALUE 60000.

       77  I                          PIC 9(4) COMP-5.
       77  J                          PIC 9(4) COMP-5.
       77  K                          PIC 9(4) COMP-5.
       77  R                          PIC 9(4) COMP-5.
       77  A                          PIC 9(4) COMP-5.
       77  B                          PIC 9(4) COMP-5.
       77  N                          PIC 9(4) COMP-5.
       77  SPAN                       PIC 9(4) COMP-5.
       77  POS                        PIC 9(4) COMP-5.
       77  FOUND                      PIC X VALUE "N".
       77  EOF1                       PIC X VALUE "N".
       77  EOF2                       PIC X VALUE "N".

       77  TMP-ID                     PIC 9(4) COMP-5.
       77  TMP-ID2                    PIC 9(4) COMP-5.

       77  BEST-NODE                  PIC 9(5) COMP-5 VALUE 0.
       77  BEST-SCORE                 COMP-2 VALUE -1.0E308.

       77  START-ID                   PIC 9(4) COMP-5 VALUE 0.

       77  JSON-LEN                   PIC 9(7) COMP-5 VALUE 0.

       01  JSON-BUFFER.
           05 JSON-TEXT               PIC X(2000000).

       01  INTERN-IN                  PIC X(32).
       01  INTERN-OUT                 PIC 9(4) COMP-5.

       01  SYMTAB.
           05 SYM-COUNT               PIC 9(4) COMP-5 VALUE 0.
           05 SYM-STR OCCURS 4096 TIMES
                                     PIC X(32).

       * -----------------------------
       * JSON mapping structures
       * -----------------------------

       01  GRAMMAR-JSON.
           05 G-START                 PIC X(32).
           05 G-RULES OCCURS 2000 TIMES.
              10 G-LHS                PIC X(32).
              10 G-RHS OCCURS 2 TIMES PIC X(32).
              10 G-WEIGHT-TXT         PIC X(32).
              10 G-PROPAGATE          PIC X(16).
              10 G-CONSTRAINTS OCCURS 8 TIMES.
                 15 GC-TYPE           PIC X(16).
                 15 GC-TARGET         PIC X(16).
                 15 GC-TARGET2        PIC X(16).
                 15 GC-KEY            PIC X(32).
                 15 GC-VALUE          PIC X(32).
                 15 GC-KEY2           PIC X(32).

       01  LEXICON-JSON.
           05 L-ENTRIES OCCURS 5000 TIMES.
              10 L-WORD               PIC X(64).
              10 L-POS                PIC X(32).
              10 L-WEIGHT-TXT         PIC X(32).
              10 L-FEATS OCCURS 8 TIMES.
                 15 LF-K              PIC X(32).
                 15 LF-V              PIC X(32).

       * -----------------------------
       * Internal grammar/lexicon
       * -----------------------------

       01  RULETAB.
           05 RULE-N                  PIC 9(4) COMP-5 VALUE 0.
           05 RULE OCCURS 2000 TIMES.
              10 RL-LHS-ID            PIC 9(4) COMP-5.
              10 RL-RHS-LEN           PIC 9(1) COMP-5.
              10 RL-RHS1-ID           PIC 9(4) COMP-5.
              10 RL-RHS2-ID           PIC 9(4) COMP-5.
              10 RL-W                 COMP-2.
              10 RL-PROP              PIC X(8).
              10 RL-NC                PIC 9(2) COMP-5.
              10 RL-C OCCURS 8 TIMES.
                 15 RC-TYPE           PIC X(8).
                 15 RC-TGT            PIC X(8).
                 15 RC-TGT2           PIC X(8).
                 15 RC-KEY-ID         PIC 9(4) COMP-5.
                 15 RC-VAL-ID         PIC 9(4) COMP-5.
                 15 RC-KEY2-ID        PIC 9(4) COMP-5.

       01  LEXTAB.
           05 LEX-N                   PIC 9(4) COMP-5 VALUE 0.
           05 LEX OCCURS 5000 TIMES.
              10 LX-WORD              PIC X(64).
              10 LX-POS-ID            PIC 9(4) COMP-5.
              10 LX-W                 COMP-2.
              10 LX-FN                PIC 9(2) COMP-5.
              10 LX-FKEY OCCURS 8 TIMES PIC 9(4) COMP-5.
              10 LX-FVAL OCCURS 8 TIMES PIC 9(4) COMP-5.

       * -----------------------------
       * Tokenization
       * -----------------------------
       01  TOKENS.
           05 TOK-N                   PIC 9(3) COMP-5 VALUE 0.
           05 TOK OCCURS 64 TIMES     PIC X(64).

       01  SENTENCE                   PIC X(1024).
       01  SENT-LEN                   PIC 9(4) COMP-5 VALUE 0.

       * -----------------------------
       * Arena (nodes)
       * -----------------------------
       01  ARENA.
           05 NODE-N                  PIC 9(5) COMP-5 VALUE 0.
           05 ND OCCURS 60000 TIMES.
              10 ND-LABEL             PIC 9(4) COMP-5.
              10 ND-LEFT              PIC 9(5) COMP-5.
              10 ND-RIGHT             PIC 9(5) COMP-5.
              10 ND-IS-LEAF           PIC X.
              10 ND-LEAF              PIC X(64).

       * -----------------------------
       * Chart: cells [1..N][1..N+1]
       * Each cell stores up to BEAM items total (dedup by cat+feats)
       * -----------------------------
       01  CHART.
           05 CELL OCCURS 64 TIMES.
              10 CELL2 OCCURS 65 TIMES.
                 15 C-N               PIC 9(2) COMP-5.
                 15 C-IT OCCURS 16 TIMES.
                    20 IT-CAT         PIC 9(4) COMP-5.
                    20 IT-SCORE       COMP-2.
                    20 IT-NODE        PIC 9(5) COMP-5.
                    20 IT-FN          PIC 9(2) COMP-5.
                    20 IT-FK OCCURS 8 TIMES PIC 9(4) COMP-5.
                    20 IT-FV OCCURS 8 TIMES PIC 9(4) COMP-5.

       * temp item buffers
       01  TMP-ITEM.
           05 T-CAT                   PIC 9(4) COMP-5.
           05 T-SCORE                 COMP-2.
           05 T-NODE                  PIC 9(5) COMP-5.
           05 T-FN                    PIC 9(2) COMP-5.
           05 T-FK OCCURS 8 TIMES     PIC 9(4) COMP-5.
           05 T-FV OCCURS 8 TIMES     PIC 9(4) COMP-5.

       01  TMP-ITEM2.
           05 U-CAT                   PIC 9(4) COMP-5.
           05 U-SCORE                 COMP-2.
           05 U-NODE                  PIC 9(5) COMP-5.
           05 U-FN                    PIC 9(2) COMP-5.
           05 U-FK OCCURS 8 TIMES     PIC 9(4) COMP-5.
           05 U-FV OCCURS 8 TIMES     PIC 9(4) COMP-5.

       * -----------------------------
       * Output
       * -----------------------------
       01  OUT-TREE                   PIC X(8000).

       * Stack for iterative tree render
       01  STK.
           05 STK-N                   PIC 9(5) COMP-5 VALUE 0.
           05 STK-NODE OCCURS 60000 TIMES PIC 9(5) COMP-5.
           05 STK-STATE OCCURS 60000 TIMES PIC 9 COMP-5.

       PROCEDURE DIVISION.

       MAIN.
           PERFORM INIT-SYMTAB
           PERFORM LOAD-GRAMMAR
           PERFORM LOAD-LEXICON

           DISPLAY "Ready. Pegá 1 oración por línea (CTRL+D/EOF para salir)."

           PERFORM UNTIL 1 = 0
               ACCEPT SENTENCE
                   ON EXCEPTION
                       EXIT PERFORM
               END-ACCEPT

               IF SENTENCE = SPACES
                   CONTINUE
               ELSE
                   PERFORM PARSE-SENTENCE
               END-IF
           END-PERFORM

           STOP RUN.

       INIT-SYMTAB.
           MOVE 0 TO SYM-COUNT
           .

       * -----------------------------
       * JSON file read helpers
       * -----------------------------
       READ-GRAMMAR-INTO-JSON.
           MOVE SPACES TO JSON-TEXT
           MOVE 0 TO JSON-LEN
           MOVE "N" TO EOF1
           OPEN INPUT GRAMMAR-FILE
           PERFORM UNTIL EOF1 = "Y"
               READ GRAMMAR-FILE
                   AT END
                       MOVE "Y" TO EOF1
                   NOT AT END
                       PERFORM APPEND-LINE
               END-READ
           END-PERFORM
           CLOSE GRAMMAR-FILE
           .

       READ-LEXICON-INTO-JSON.
           MOVE SPACES TO JSON-TEXT
           MOVE 0 TO JSON-LEN
           MOVE "N" TO EOF2
           OPEN INPUT LEXICON-FILE
           PERFORM UNTIL EOF2 = "Y"
               READ LEXICON-FILE
                   AT END
                       MOVE "Y" TO EOF2
                   NOT AT END
                       PERFORM APPEND-LINE2
               END-READ
           END-PERFORM
           CLOSE LEXICON-FILE
           .

       APPEND-LINE.
           IF JSON-LEN < 1999000
               ADD 1 TO JSON-LEN
               STRING JSON-TEXT(1:JSON-LEN)
                      TRIM(GRAMMAR-LINE)
                      INTO JSON-TEXT
               END-STRING
               ADD FUNCTION LENGTH(TRIM(GRAMMAR-LINE)) TO JSON-LEN
           END-IF
           .

       APPEND-LINE2.
           IF JSON-LEN < 1999000
               ADD 1 TO JSON-LEN
               STRING JSON-TEXT(1:JSON-LEN)
                      TRIM(LEXICON-LINE)
                      INTO JSON-TEXT
               END-STRING
               ADD FUNCTION LENGTH(TRIM(LEXICON-LINE)) TO JSON-LEN
           END-IF
           .

       * -----------------------------
       * Load grammar.json -> RULETAB
       * Uses JSON PARSE (IBM/MF)
       * IBM syntax supports NAME ... OMITTED. citeturn1view0
       * -----------------------------
       LOAD-GRAMMAR.
           PERFORM READ-GRAMMAR-INTO-JSON
           MOVE SPACES TO GRAMMAR-JSON

           JSON PARSE JSON-TEXT
             INTO GRAMMAR-JSON
             NAME OF GRAMMAR-JSON IS OMITTED
           ON EXCEPTION
             DISPLAY "ERROR: JSON PARSE grammar.json falló."
             STOP RUN
           END-JSON

           MOVE FUNCTION TRIM(G-START) TO INTERN-IN
           PERFORM INTERN
           MOVE INTERN-OUT TO START-ID

           MOVE 0 TO RULE-N
           PERFORM VARYING R FROM 1 BY 1 UNTIL R > 2000
               IF FUNCTION TRIM(G-LHS(R)) = SPACES
                   EXIT PERFORM
               END-IF

               ADD 1 TO RULE-N
               MOVE FUNCTION TRIM(G-LHS(R)) TO INTERN-IN
               PERFORM INTERN
               MOVE INTERN-OUT TO RL-LHS-ID(RULE-N)

               * rhs len
               IF FUNCTION TRIM(G-RHS(R 2)) NOT = SPACES
                   MOVE 2 TO RL-RHS-LEN(RULE-N)
                   MOVE FUNCTION TRIM(G-RHS(R 1)) TO INTERN-IN
                   PERFORM INTERN
                   MOVE INTERN-OUT TO RL-RHS1-ID(RULE-N)
                   MOVE FUNCTION TRIM(G-RHS(R 2)) TO INTERN-IN
                   PERFORM INTERN
                   MOVE INTERN-OUT TO RL-RHS2-ID(RULE-N)
               ELSE
                   MOVE 1 TO RL-RHS-LEN(RULE-N)
                   MOVE FUNCTION TRIM(G-RHS(R 1)) TO INTERN-IN
                   PERFORM INTERN
                   MOVE INTERN-OUT TO RL-RHS1-ID(RULE-N)
                   MOVE 0 TO RL-RHS2-ID(RULE-N)
               END-IF

               * weight
               IF FUNCTION TRIM(G-WEIGHT-TXT(R)) = SPACES
                   MOVE 1.0 TO RL-W(RULE-N)
               ELSE
                   COMPUTE RL-W(RULE-N) =
                       FUNCTION LOG(FUNCTION NUMVAL(G-WEIGHT-TXT(R)))
               END-IF

               * propagate
               MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(G-PROPAGATE(R)))
                   TO RL-PROP(RULE-N)
               IF RL-PROP(RULE-N) = SPACES
                   MOVE "MERGE" TO RL-PROP(RULE-N)
               END-IF

               * constraints
               MOVE 0 TO RL-NC(RULE-N)
               PERFORM VARYING C FROM 1 BY 1 UNTIL C > 8
                   IF FUNCTION TRIM(GC-TYPE(R C)) = SPACES
                       CONTINUE
                   ELSE
                       ADD 1 TO RL-NC(RULE-N)
                       MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(GC-TYPE(R C)))
                           TO RC-TYPE(RULE-N RL-NC(RULE-N))
                       MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(GC-TARGET(R C)))
                           TO RC-TGT(RULE-N RL-NC(RULE-N))
                       MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(GC-TARGET2(R C)))
                           TO RC-TGT2(RULE-N RL-NC(RULE-N))

                       MOVE FUNCTION TRIM(GC-KEY(R C)) TO INTERN-IN
                       PERFORM INTERN
                       MOVE INTERN-OUT TO RC-KEY-ID(RULE-N RL-NC(RULE-N))

                       MOVE FUNCTION TRIM(GC-VALUE(R C)) TO INTERN-IN
                       PERFORM INTERN
                       MOVE INTERN-OUT TO RC-VAL-ID(RULE-N RL-NC(RULE-N))

                       MOVE FUNCTION TRIM(GC-KEY2(R C)) TO INTERN-IN
                       PERFORM INTERN
                       MOVE INTERN-OUT TO RC-KEY2-ID(RULE-N RL-NC(RULE-N))
                   END-IF
               END-PERFORM
           END-PERFORM

           DISPLAY "Grammar loaded. Rules=" RULE-N
           .

       * -----------------------------
       * Load lexicon.json -> LEXTAB
       * -----------------------------
       LOAD-LEXICON.
           PERFORM READ-LEXICON-INTO-JSON
           MOVE SPACES TO LEXICON-JSON

           JSON PARSE JSON-TEXT
             INTO LEXICON-JSON
             NAME OF LEXICON-JSON IS OMITTED
           ON EXCEPTION
             DISPLAY "ERROR: JSON PARSE lexicon.json falló."
             STOP RUN
           END-JSON

           MOVE 0 TO LEX-N
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5000
               IF FUNCTION TRIM(L-WORD(I)) = SPACES
                   EXIT PERFORM
               END-IF

               ADD 1 TO LEX-N
               MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(L-WORD(I)))
                   TO LX-WORD(LEX-N)

               MOVE FUNCTION TRIM(L-POS(I)) TO INTERN-IN
               PERFORM INTERN
               MOVE INTERN-OUT TO LX-POS-ID(LEX-N)

               IF FUNCTION TRIM(L-WEIGHT-TXT(I)) = SPACES
                   MOVE 1.0 TO LX-W(LEX-N)
               ELSE
                   COMPUTE LX-W(LEX-N) =
                       FUNCTION LOG(FUNCTION NUMVAL(L-WEIGHT-TXT(I)))
               END-IF

               MOVE 0 TO LX-FN(LEX-N)
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 8
                   IF FUNCTION TRIM(LF-K(I J)) = SPACES
                       CONTINUE
                   ELSE
                       ADD 1 TO LX-FN(LEX-N)
                       MOVE FUNCTION TRIM(LF-K(I J)) TO INTERN-IN
                       PERFORM INTERN
                       MOVE INTERN-OUT TO LX-FKEY(LEX-N LX-FN(LEX-N))

                       MOVE FUNCTION TRIM(LF-V(I J)) TO INTERN-IN
                       PERFORM INTERN
                       MOVE INTERN-OUT TO LX-FVAL(LEX-N LX-FN(LEX-N))
                   END-IF
               END-PERFORM
           END-PERFORM

           DISPLAY "Lexicon loaded. Entries=" LEX-N
           .

       * -----------------------------
       * INTERN: string -> integer symbol id
       * -----------------------------
       INTERN.
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INTERN-IN)) TO INTERN-IN
           IF INTERN-IN = SPACES
               MOVE 0 TO INTERN-OUT
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SYM-COUNT
               IF SYM-STR(I) = INTERN-IN
                   MOVE I TO INTERN-OUT
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM

           ADD 1 TO SYM-COUNT
           MOVE INTERN-IN TO SYM-STR(SYM-COUNT)
           MOVE SYM-COUNT TO INTERN-OUT
           .

       * -----------------------------
       * Tokenize + split al/del
       * -----------------------------
       TOKENIZE.
           MOVE 0 TO TOK-N
           MOVE FUNCTION LENGTH(FUNCTION TRIM(SENTENCE)) TO SENT-LEN

           MOVE 1 TO POS
           PERFORM UNTIL POS > SENT-LEN
               PERFORM SKIP-NONWORD
               IF POS > SENT-LEN
                   EXIT PERFORM
               END-IF
               PERFORM READ-WORD
           END-PERFORM

           PERFORM SPLIT-CONTRACTIONS
           .

       SKIP-NONWORD.
           PERFORM UNTIL POS > SENT-LEN
               IF SENTENCE(POS:1) IS ALPHABETIC
                   EXIT PERFORM
               ELSE IF SENTENCE(POS:1) IS NUMERIC
                   EXIT PERFORM
               ELSE
                   ADD 1 TO POS
               END-IF
           END-PERFORM
           .

       READ-WORD.
           ADD 1 TO TOK-N
           MOVE SPACES TO TOK(TOK-N)

           MOVE 1 TO J
           PERFORM UNTIL POS > SENT-LEN
               IF SENTENCE(POS:1) IS ALPHABETIC OR SENTENCE(POS:1) IS NUMERIC
                   MOVE SENTENCE(POS:1) TO TOK(TOK-N)(J:1)
                   ADD 1 TO J
                   ADD 1 TO POS
               ELSE
                   EXIT PERFORM
               END-IF
               IF J > 64
                   EXIT PERFORM
               END-IF
           END-PERFORM

           MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(TOK(TOK-N))) TO TOK(TOK-N)
           .

       SPLIT-CONTRACTIONS.
           * al -> a el ; del -> de el
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOK-N
               IF TOK(I) = "al"
                   PERFORM SHIFT-RIGHT-FROM-I
                   MOVE "a"  TO TOK(I)
                   MOVE "el" TO TOK(I + 1)
                   ADD 1 TO TOK-N
               ELSE IF TOK(I) = "del"
                   PERFORM SHIFT-RIGHT-FROM-I
                   MOVE "de" TO TOK(I)
                   MOVE "el" TO TOK(I + 1)
                   ADD 1 TO TOK-N
               END-IF
           END-PERFORM
           .

       SHIFT-RIGHT-FROM-I.
           PERFORM VARYING J FROM TOK-N BY -1 UNTIL J < I
               MOVE TOK(J) TO TOK(J + 1)
           END-PERFORM
           .

       * -----------------------------
       * Parse sentence: CKY + unary closure + beam
       * -----------------------------
       PARSE-SENTENCE.
           PERFORM TOKENIZE
           IF TOK-N = 0
               DISPLAY "(NO-PARSE)"
               EXIT PARAGRAPH
           END-IF

           PERFORM RESET-ARENA
           PERFORM CLEAR-CHART

           * seed lexical + unary closure per token
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TOK-N
               PERFORM INIT-CELL
                   USING I I + 1
               PERFORM SEED-LEX
                   USING I I + 1 TOK(I)
               PERFORM UNARY-CLOSURE
                   USING I I + 1
           END-PERFORM

           * CKY spans
           PERFORM VARYING SPAN FROM 2 BY 1 UNTIL SPAN > TOK-N
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > (TOK-N - SPAN + 1)
                   COMPUTE J = I + SPAN
                   PERFORM INIT-CELL USING I J

                   PERFORM VARYING K FROM I + 1 BY 1 UNTIL K > J - 1
                       PERFORM COMBINE
                           USING I K K J I J
                   END-PERFORM

                   PERFORM UNARY-CLOSURE USING I J
               END-PERFORM
           END-PERFORM

           PERFORM PICK-BEST-ROOT
           IF BEST-NODE = 0
               DISPLAY "(NO-PARSE)"
           ELSE
               PERFORM RENDER-TREE
               DISPLAY FUNCTION TRIM(OUT-TREE)
           END-IF
           .

       RESET-ARENA.
           MOVE 0 TO NODE-N
           .

       CLEAR-CHART.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 64
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 65
                   MOVE 0 TO C-N(I J)
               END-PERFORM
           END-PERFORM
           .

       INIT-CELL USING BY VALUE I BY VALUE J.
           MOVE 0 TO C-N(I J)
           .

       * Add node, returns NODE-N in TMP-ID
       ADD-LEAF-NODE USING BY VALUE CAT-ID BY REFERENCE WORD.
           ADD 1 TO NODE-N
           MOVE CAT-ID TO ND-LABEL(NODE-N)
           MOVE 0      TO ND-LEFT(NODE-N)
           MOVE 0      TO ND-RIGHT(NODE-N)
           MOVE "Y"    TO ND-IS-LEAF(NODE-N)
           MOVE WORD   TO ND-LEAF(NODE-N)
           MOVE NODE-N TO TMP-ID
           .

       ADD-BIN-NODE USING BY VALUE CAT-ID BY VALUE LID BY VALUE RID.
           ADD 1 TO NODE-N
           MOVE CAT-ID TO ND-LABEL(NODE-N)
           MOVE LID    TO ND-LEFT(NODE-N)
           MOVE RID    TO ND-RIGHT(NODE-N)
           MOVE "N"    TO ND-IS-LEAF(NODE-N)
           MOVE SPACES TO ND-LEAF(NODE-N)
           MOVE NODE-N TO TMP-ID
           .

       ADD-UN-NODE USING BY VALUE CAT-ID BY VALUE CID.
           ADD 1 TO NODE-N
           MOVE CAT-ID TO ND-LABEL(NODE-N)
           MOVE CID    TO ND-LEFT(NODE-N)
           MOVE 0      TO ND-RIGHT(NODE-N)
           MOVE "N"    TO ND-IS-LEAF(NODE-N)
           MOVE SPACES TO ND-LEAF(NODE-N)
           MOVE NODE-N TO TMP-ID
           .

       * Insert item with dedup (cat+feats) and beam keep-best
       CELL-ADD-ITEM USING BY VALUE I BY VALUE J.
           IF C-N(I J) = 0
               MOVE 1 TO C-N(I J)
               MOVE T-CAT   TO IT-CAT(I J 1)
               MOVE T-SCORE TO IT-SCORE(I J 1)
               MOVE T-NODE  TO IT-NODE(I J 1)
               MOVE T-FN    TO IT-FN(I J 1)
               PERFORM COPY-FEATS-TO-CELL USING I J 1
               EXIT PARAGRAPH
           END-IF

           * dedup
           PERFORM VARYING A FROM 1 BY 1 UNTIL A > C-N(I J)
               IF IT-CAT(I J A) = T-CAT
                   PERFORM FEATS-EQUAL CHECKING A
                   IF FOUND = "Y"
                       IF T-SCORE > IT-SCORE(I J A)
                           MOVE T-SCORE TO IT-SCORE(I J A)
                           MOVE T-NODE  TO IT-NODE(I J A)
                           MOVE T-FN    TO IT-FN(I J A)
                           PERFORM COPY-FEATS-TO-CELL USING I J A
                       END-IF
                       EXIT PARAGRAPH
                   END-IF
               END-IF
           END-PERFORM

           * append if room
           IF C-N(I J) < BEAM
               ADD 1 TO C-N(I J)
               MOVE T-CAT   TO IT-CAT(I J C-N(I J))
               MOVE T-SCORE TO IT-SCORE(I J C-N(I J))
               MOVE T-NODE  TO IT-NODE(I J C-N(I J))
               MOVE T-FN    TO IT-FN(I J C-N(I J))
               PERFORM COPY-FEATS-TO-CELL USING I J C-N(I J)
               PERFORM SORT-CELL USING I J
               EXIT PARAGRAPH
           END-IF

           * replace worst if better
           PERFORM SORT-CELL USING I J
           IF T-SCORE > IT-SCORE(I J BEAM)
               MOVE T-CAT   TO IT-CAT(I J BEAM)
               MOVE T-SCORE TO IT-SCORE(I J BEAM)
               MOVE T-NODE  TO IT-NODE(I J BEAM)
               MOVE T-FN    TO IT-FN(I J BEAM)
               PERFORM COPY-FEATS-TO-CELL USING I J BEAM
               PERFORM SORT-CELL USING I J
           END-IF
           .

       FEATS-EQUAL CHECKING A.
           MOVE "N" TO FOUND
           IF IT-FN(I J A) NOT = T-FN
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING B FROM 1 BY 1 UNTIL B > T-FN
               IF IT-FK(I J A B) NOT = T-FK(B)
                   EXIT PARAGRAPH
               END-IF
               IF IT-FV(I J A B) NOT = T-FV(B)
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM
           MOVE "Y" TO FOUND
           .

       COPY-FEATS-TO-CELL USING BY VALUE I BY VALUE J BY VALUE A.
           MOVE T-FN TO IT-FN(I J A)
           PERFORM VARYING B FROM 1 BY 1 UNTIL B > 8
               MOVE T-FK(B) TO IT-FK(I J A B)
               MOVE T-FV(B) TO IT-FV(I J A B)
           END-PERFORM
           .

       SORT-CELL USING BY VALUE I BY VALUE J.
           * insertion-like sort by score desc (beam pequeño)
           PERFORM VARYING A FROM 2 BY 1 UNTIL A > C-N(I J)
               PERFORM VARYING B FROM A BY -1 UNTIL B <= 1
                   IF IT-SCORE(I J B) > IT-SCORE(I J B - 1)
                       PERFORM SWAP-ITEMS USING I J B B - 1
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM
           .

       SWAP-ITEMS USING BY VALUE I BY VALUE J BY VALUE P BY VALUE Q.
           * swap item slots P and Q in cell(I,J)
           MOVE IT-CAT(I J P)   TO U-CAT
           MOVE IT-SCORE(I J P) TO U-SCORE
           MOVE IT-NODE(I J P)  TO U-NODE
           MOVE IT-FN(I J P)    TO U-FN
           PERFORM VARYING B FROM 1 BY 1 UNTIL B > 8
               MOVE IT-FK(I J P B) TO U-FK(B)
               MOVE IT-FV(I J P B) TO U-FV(B)
           END-PERFORM

           MOVE IT-CAT(I J Q)   TO IT-CAT(I J P)
           MOVE IT-SCORE(I J Q) TO IT-SCORE(I J P)
           MOVE IT-NODE(I J Q)  TO IT-NODE(I J P)
           MOVE IT-FN(I J Q)    TO IT-FN(I J P)
           PERFORM VARYING B FROM 1 BY 1 UNTIL B > 8
               MOVE IT-FK(I J Q B) TO IT-FK(I J P B)
               MOVE IT-FV(I J Q B) TO IT-FV(I J P B)
           END-PERFORM

           MOVE U-CAT   TO IT-CAT(I J Q)
           MOVE U-SCORE TO IT-SCORE(I J Q)
           MOVE U-NODE  TO IT-NODE(I J Q)
           MOVE U-FN    TO IT-FN(I J Q)
           PERFORM VARYING B FROM 1 BY 1 UNTIL B > 8
               MOVE U-FK(B) TO IT-FK(I J Q B)
               MOVE U-FV(B) TO IT-FV(I J Q B)
           END-PERFORM
           .

       * -----------------------------
       * Lexical seeding
       * -----------------------------
       SEED-LEX USING BY VALUE I BY VALUE J BY REFERENCE WORD.
           MOVE 0 TO FOUND
           PERFORM VARYING A FROM 1 BY 1 UNTIL A > LEX-N
               IF LX-WORD(A) = WORD
                   MOVE 1 TO FOUND

                   MOVE LX-POS-ID(A) TO T-CAT
                   MOVE LX-W(A)      TO T-SCORE
                   MOVE LX-FN(A)     TO T-FN
                   PERFORM VARYING B FROM 1 BY 1 UNTIL B > 8
                       MOVE LX-FKEY(A B) TO T-FK(B)
                       MOVE LX-FVAL(A B) TO T-FV(B)
                   END-PERFORM

                   PERFORM ADD-LEAF-NODE USING T-CAT WORD
                   MOVE TMP-ID TO T-NODE
                   PERFORM CELL-ADD-ITEM USING I J
               END-IF
           END-PERFORM

           IF FOUND = 0
               * OOV fallback: NOUN
               MOVE "NOUN" TO INTERN-IN
               PERFORM INTERN
               MOVE INTERN-OUT TO T-CAT
               MOVE -13.815510557964274 TO T-SCORE  *> log(1e-6)
               MOVE 0 TO T-FN
               PERFORM VARYING B FROM 1 BY 1 UNTIL B > 8
                   MOVE 0 TO T-FK(B)
                   MOVE 0 TO T-FV(B)
               END-PERFORM
               PERFORM ADD-LEAF-NODE USING T-CAT WORD
               MOVE TMP-ID TO T-NODE
               PERFORM CELL-ADD-ITEM USING I J
           END-IF
           .

       * -----------------------------
       * Unary closure
       * -----------------------------
       UNARY-CLOSURE USING BY VALUE I BY VALUE J.
           PERFORM VARYING A FROM 1 BY 1 UNTIL A > C-N(I J)
               PERFORM VARYING R FROM 1 BY 1 UNTIL R > RULE-N
                   IF RL-RHS-LEN(R) = 1 AND RL-RHS1-ID(R) = IT-CAT(I J A)
                       PERFORM BUILD-UNARY USING I J A R
                   END-IF
               END-PERFORM
           END-PERFORM
           .

       BUILD-UNARY USING BY VALUE I BY VALUE J BY VALUE A BY VALUE R.
           * copy child item -> TMP then apply constraints -> parent
           MOVE RL-LHS-ID(R) TO T-CAT
           COMPUTE T-SCORE = IT-SCORE(I J A) + RL-W(R)
           MOVE IT-FN(I J A) TO T-FN
           PERFORM VARYING B FROM 1 BY 1 UNTIL B > 8
               MOVE IT-FK(I J A B) TO T-FK(B)
               MOVE IT-FV(I J A B) TO T-FV(B)
           END-PERFORM

           PERFORM APPLY-CONSTRAINTS-UNARY USING I J A R
           IF FOUND = "N"
               EXIT PARAGRAPH
           END-IF

           PERFORM ADD-UN-NODE USING T-CAT IT-NODE(I J A)
           MOVE TMP-ID TO T-NODE
           PERFORM CELL-ADD-ITEM USING I J
           .

       APPLY-CONSTRAINTS-UNARY USING BY VALUE I BY VALUE J BY VALUE A BY VALUE R.
           MOVE "Y" TO FOUND
           PERFORM VARYING B FROM 1 BY 1 UNTIL B > RL-NC(R)
               EVALUATE RC-TYPE(R B)
               WHEN "REQUIRE"
                   PERFORM FEAT-GET USING IT-FN(I J A)
                                          IT-FK(I J A 1)
                                          IT-FV(I J A 1)
                                          RC-KEY-ID(R B)
                   IF TMP-ID NOT = RC-VAL-ID(R B)
                       MOVE "N" TO FOUND
                       EXIT PERFORM
                   END-IF
               WHEN "ASSIGN"
                   PERFORM FEAT-PUT USING RC-KEY-ID(R B) RC-VAL-ID(R B)
               WHEN OTHER
                   CONTINUE
               END-EVALUATE
           END-PERFORM
           .

       * -----------------------------
       * Combine: left cell (I,K) + right cell (K,J) -> out cell (I,J)
       * -----------------------------
       COMBINE USING BY VALUE LI BY VALUE LK
                     BY VALUE RK BY VALUE RJ
                     BY VALUE OI BY VALUE OJ.
           PERFORM VARYING A FROM 1 BY 1 UNTIL A > C-N(LI LK)
               PERFORM VARYING B FROM 1 BY 1 UNTIL B > C-N(RK RJ)
                   PERFORM VARYING R FROM 1 BY 1 UNTIL R > RULE-N
                       IF RL-RHS-LEN(R) = 2
                          AND RL-RHS1-ID(R) = IT-CAT(LI LK A)
                          AND RL-RHS2-ID(R) = IT-CAT(RK RJ B)
                           PERFORM BUILD-BINARY USING LI LK A RK RJ B OI OJ R
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM
           .

       BUILD-BINARY USING BY VALUE LI BY VALUE LK BY VALUE A
                           BY VALUE RI BY VALUE RJ BY VALUE B
                           BY VALUE OI BY VALUE OJ
                           BY VALUE R.
           MOVE RL-LHS-ID(R) TO T-CAT
           COMPUTE T-SCORE =
               IT-SCORE(LI LK A) + IT-SCORE(RI RJ B) + RL-W(R)

           PERFORM PROPAGATE-FEATS USING LI LK A RI RJ B R
           PERFORM APPLY-CONSTRAINTS-BIN USING LI LK A RI RJ B R
           IF FOUND = "N"
               EXIT PARAGRAPH
           END-IF

           PERFORM ADD-BIN-NODE USING T-CAT IT-NODE(LI LK A) IT-NODE(RI RJ B)
           MOVE TMP-ID TO T-NODE
           PERFORM CELL-ADD-ITEM USING OI OJ
           .

       PROPAGATE-FEATS USING BY VALUE LI BY VALUE LK BY VALUE A
                             BY VALUE RI BY VALUE RJ BY VALUE B
                             BY VALUE R.
           * MERGE/LEFT/RIGHT
           MOVE 0 TO T-FN
           PERFORM CLEAR-TMP-FEATS
           IF RL-PROP(R) = "LEFT"
               PERFORM COPY-FEATS-FROM-CELL USING LI LK A
           ELSE IF RL-PROP(R) = "RIGHT"
               PERFORM COPY-FEATS-FROM-CELL USING RI RJ B
           ELSE
               PERFORM COPY-FEATS-FROM-CELL USING LI LK A
               PERFORM MERGE-FEATS-FROM-CELL USING RI RJ B
           END-IF
           .

       CLEAR-TMP-FEATS.
           MOVE 0 TO T-FN
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 8
               MOVE 0 TO T-FK(K)
               MOVE 0 TO T-FV(K)
           END-PERFORM
           .

       COPY-FEATS-FROM-CELL USING BY VALUE CI BY VALUE CJ BY VALUE IDX.
           MOVE IT-FN(CI CJ IDX) TO T-FN
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 8
               MOVE IT-FK(CI CJ IDX K) TO T-FK(K)
               MOVE IT-FV(CI CJ IDX K) TO T-FV(K)
           END-PERFORM
           .

       MERGE-FEATS-FROM-CELL USING BY VALUE CI BY VALUE CJ BY VALUE IDX.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > IT-FN(CI CJ IDX)
               PERFORM FEAT-GET-IN-TMP USING IT-FK(CI CJ IDX K)
               IF FOUND = "N"
                   ADD 1 TO T-FN
                   IF T-FN <= 8
                       MOVE IT-FK(CI CJ IDX K) TO T-FK(T-FN)
                       MOVE IT-FV(CI CJ IDX K) TO T-FV(T-FN)
                   END-IF
               END-IF
           END-PERFORM
           .

       FEAT-GET-IN-TMP USING BY VALUE KEYID.
           MOVE "N" TO FOUND
           PERFORM VARYING POS FROM 1 BY 1 UNTIL POS > T-FN
               IF T-FK(POS) = KEYID
                   MOVE "Y" TO FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

       APPLY-CONSTRAINTS-BIN USING BY VALUE LI BY VALUE LK BY VALUE A
                                  BY VALUE RI BY VALUE RJ BY VALUE B
                                  BY VALUE R.
           MOVE "Y" TO FOUND
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > RL-NC(R)
               EVALUATE RC-TYPE(R K)
               WHEN "REQUIRE"
                   * require on left/right depending target
                   IF RC-TGT(R K) = "RIGHT"
                       PERFORM FEAT-GET-CELL USING RI RJ B RC-KEY-ID(R K)
                   ELSE
                       PERFORM FEAT-GET-CELL USING LI LK A RC-KEY-ID(R K)
                   END-IF
                   IF TMP-ID NOT = RC-VAL-ID(R K)
                       MOVE "N" TO FOUND
                       EXIT PERFORM
                   END-IF

               WHEN "UNIFY"
                   PERFORM FEAT-GET-CELL USING LI LK A RC-KEY-ID(R K)
                   MOVE TMP-ID TO TMP-ID2
                   PERFORM FEAT-GET-CELL USING RI RJ B RC-KEY-ID(R K)
                   IF TMP-ID2 NOT = 0 AND TMP-ID NOT = 0 AND TMP-ID2 NOT = TMP-ID
                       MOVE "N" TO FOUND
                       EXIT PERFORM
                   END-IF
                   IF TMP-ID2 NOT = 0
                       PERFORM FEAT-PUT USING RC-KEY-ID(R K) TMP-ID2
                   ELSE IF TMP-ID NOT = 0
                       PERFORM FEAT-PUT USING RC-KEY-ID(R K) TMP-ID
                   END-IF

               WHEN "AGREE"
                   PERFORM FEAT-GET-CELL USING LI LK A RC-KEY-ID(R K)
                   MOVE TMP-ID TO TMP-ID2
                   PERFORM FEAT-GET-CELL USING RI RJ B RC-KEY-ID(R K)
                   IF TMP-ID2 = 0 OR TMP-ID = 0 OR TMP-ID2 NOT = TMP-ID
                       MOVE "N" TO FOUND
                       EXIT PERFORM
                   END-IF
                   PERFORM FEAT-PUT USING RC-KEY-ID(R K) TMP-ID

               WHEN "ASSIGN"
                   PERFORM FEAT-PUT USING RC-KEY-ID(R K) RC-VAL-ID(R K)

               WHEN OTHER
                   CONTINUE
               END-EVALUATE
           END-PERFORM
           .

       FEAT-GET-CELL USING BY VALUE CI BY VALUE CJ BY VALUE IDX BY VALUE KEYID.
           MOVE 0 TO TMP-ID
           PERFORM VARYING POS FROM 1 BY 1 UNTIL POS > IT-FN(CI CJ IDX)
               IF IT-FK(CI CJ IDX POS) = KEYID
                   MOVE IT-FV(CI CJ IDX POS) TO TMP-ID
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .

       FEAT-GET USING BY VALUE FN BY VALUE FK1 BY VALUE FV1 BY VALUE KEYID.
           * not used (placeholder)
           MOVE 0 TO TMP-ID
           .

       FEAT-PUT USING BY VALUE KEYID BY VALUE VALID.
           * set/overwrite in TMP feats
           PERFORM VARYING POS FROM 1 BY 1 UNTIL POS > T-FN
               IF T-FK(POS) = KEYID
                   MOVE VALID TO T-FV(POS)
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM
           ADD 1 TO T-FN
           IF T-FN <= 8
               MOVE KEYID TO T-FK(T-FN)
               MOVE VALID TO T-FV(T-FN)
           END-IF
           .

       * -----------------------------
       * Pick best root (start symbol)
       * -----------------------------
       PICK-BEST-ROOT.
           MOVE 0 TO BEST-NODE
           MOVE -1.0E308 TO BEST-SCORE
           MOVE TOK-N TO N
           ADD 1 TO N

           PERFORM VARYING A FROM 1 BY 1 UNTIL A > C-N(1 N)
               IF IT-CAT(1 N A) = START-ID
                   IF IT-SCORE(1 N A) > BEST-SCORE
                       MOVE IT-SCORE(1 N A) TO BEST-SCORE
                       MOVE IT-NODE(1 N A)  TO BEST-NODE
                   END-IF
               END-IF
           END-PERFORM
           .

       * -----------------------------
       * Render bracketed tree (iterative stack)
       * -----------------------------
       RENDER-TREE.
           MOVE SPACES TO OUT-TREE
           MOVE 0 TO STK-N
           PERFORM PUSH USING BEST-NODE 0
           MOVE 1 TO POS

           PERFORM UNTIL STK-N = 0
               MOVE STK-NODE(STK-N)  TO TMP-ID
               MOVE STK-STATE(STK-N) TO TMP-ID2

               IF TMP-ID2 = 0
                   * open: "(LABEL "
                   PERFORM APPEND-OPEN USING TMP-ID
                   MOVE 1 TO STK-STATE(STK-N)

                   IF ND-IS-LEAF(TMP-ID) = "Y"
                       PERFORM APPEND-LEAF USING TMP-ID
                       PERFORM APPEND-CLOSE
                       SUBTRACT 1 FROM STK-N
                   ELSE
                       * push left first
                       PERFORM PUSH USING ND-LEFT(TMP-ID) 0
                   END-IF

               ELSE IF TMP-ID2 = 1
                   MOVE 2 TO STK-STATE(STK-N)
                   IF ND-RIGHT(TMP-ID) NOT = 0
                       PERFORM PUSH USING ND-RIGHT(TMP-ID) 0
                   ELSE
                       PERFORM APPEND-CLOSE
                       SUBTRACT 1 FROM STK-N
                   END-IF

               ELSE
                   PERFORM APPEND-CLOSE
                   SUBTRACT 1 FROM STK-N
               END-IF
           END-PERFORM
           .

       PUSH USING BY VALUE NID BY VALUE STATE.
           ADD 1 TO STK-N
           MOVE NID   TO STK-NODE(STK-N)
           MOVE STATE TO STK-STATE(STK-N)
           .

       APPEND-OPEN USING BY VALUE NID.
           * label -> string
           MOVE SYM-STR(ND-LABEL(NID)) TO INTERN-IN
           STRING OUT-TREE(1:POS)
                  "("
                  FUNCTION TRIM(INTERN-IN)
                  " "
               INTO OUT-TREE
           END-STRING
           ADD 2 TO POS
           ADD FUNCTION LENGTH(FUNCTION TRIM(INTERN-IN)) TO POS
           ADD 1 TO POS
           .

       APPEND-LEAF USING BY VALUE NID.
           STRING OUT-TREE(1:POS)
                  FUNCTION TRIM(ND-LEAF(NID))
               INTO OUT-TREE
           END-STRING
           ADD FUNCTION LENGTH(FUNCTION TRIM(ND-LEAF(NID))) TO POS
           .

       APPEND-CLOSE.
           STRING OUT-TREE(1:POS)
                  ")"
               INTO OUT-TREE
           END-STRING
           ADD 1 TO POS
           .

       END PROGRAM NLPARSER7.
