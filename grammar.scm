(use comparse srfi-14 data-structures matchable)

;; combinators

(define (char->operator s)
  (match s
    [#\+ 'add]
    [#\- 'sub]
    [#\* 'mul]
    [#\/ 'div]
    [#\d 'sum-die]
    [(#\l #\d) 'list-die]
    [(#\k #\h) 'kh-n]
    [(#\k #\l) 'kl-n]))

(define (char->unary s)
  (match s
    [#\- 'neg]
    [#\! 'fac]
    [#\d 'unary-die]
    [#\s 'sum]
    [(#\k #\h) 'kh-one]
    [(#\k #\l) 'kl-one]))

(define (as-operator parser)
  (bind parser
        (o result char->operator)))

(define (as-unary parser)
  (bind parser
        (o result char->unary)))

(define (as-number parser)
  (bind (as-string parser)
        (o result string->number)))

;; primitives

(define ws
  (zero-or-more (in char-set:whitespace)))

(define begin-paren
  (is #\())

(define end-paren
  (is #\)))

(define number
  (as-number (one-or-more (in char-set:digit))))

(define expr-op
  (as-operator (in #\+ #\-)))

(define term-op
  (as-operator (in #\* #\/)))

(define die-expr-op
  (as-operator
   (any-of
    (in #\d)
    (sequence (is #\l) (is #\d))
    (sequence (is #\k) (is #\h))
    (sequence (is #\k) (is #\l)))))

(define left-unary-op
  (as-unary (in #\- #\d)))

(define right-unary-op
  (as-unary
   (any-of
    (in #\! #\s)
    (sequence (is #\k) (is #\h))
    (sequence (is #\k) (is #\l)))))

(define (group-ops head tail)
  (let ((ops (map car tail))
        (elts (map cdr tail)))
    (list ops (cons head elts))))

(define (reduce-ops head tail)
  (result (match tail
            [() head]
            [_ (group-ops head tail)])))

;; rules

(define die-expr-tail
  (recursive-parser
   (sequence* ((op die-expr-op)
               (tail atom))
     (result (cons op tail)))))

(define die-expr
  (recursive-parser
   (sequence* ((head atom)
               (tail (zero-or-more die-expr-tail)))
     (reduce-ops head tail))))


(define term-tail
  (recursive-parser
   (sequence* ((op term-op)
               (tail unary))
     (result (cons op tail)))))

(define term
  (recursive-parser
   (sequence* ((head unary)
               (tail (zero-or-more term-tail)))
     (reduce-ops head tail))))


(define expr-tail
  (recursive-parser
   (sequence* ((op expr-op)
               (tail term))
     (result (cons op tail)))))

(define expr
  (recursive-parser
   (sequence* ((head term)
               (tail (zero-or-more expr-tail)))
     (reduce-ops head tail))))

(define paren-expr
  (recursive-parser
   (enclosed-by
    (preceded-by begin-paren ws)
    expr
    end-paren)))

(define atom
  (enclosed-by ws (any-of number paren-expr) ws))

(define left-unary
  (sequence* ((op left-unary-op)
              (arg die-expr))
    (result (cons op arg))))

(define right-unary
  (sequence* ((arg die-expr)
              (op right-unary-op))
    (result (cons op arg))))

(define left-right-unary
  (sequence* ((op-l left-unary-op)
              (arg die-expr)
              (op-r right-unary-op))
    (result (cons (list op-r op-l) arg))))

(define unary
  (any-of left-right-unary right-unary left-unary die-expr))

;; api

(define (parse-string s)
  (parse (followed-by expr end-of-input) s))
