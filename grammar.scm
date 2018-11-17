(module grammar
    (parse-expr)

  (import scheme chicken)
  (use comparse srfi-14 data-structures matchable)

  ;; combinators

  (define (char->operator s)
    (case s
      ((#\+) 'add)
      ((#\-) 'sub)
      ((#\*) 'mul)
      ((#\/) 'div)))

  (define (char->unary s)
    (case s
      ((#\-) 'neg)
      ((#\!) 'fac)))

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

  (define left-unary-op
    (as-unary (in #\-)))

  (define right-unary-op
    (as-unary (in #\!)))

  (define (group-ops head tail)
    (let ((ops (map car tail))
          (elts (map cdr tail)))
      (list ops (cons head elts))))

  (define (reduce-ops head tail)
    (result (match tail
              [() head]
              [_ (group-ops head tail)])))

  ;; rules

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
                (arg atom))
      (result (cons (list op) arg))))

  (define right-unary
    (sequence* ((arg atom)
                (op right-unary-op))
      (result (cons (list op) arg))))

  (define left-right-unary
    (sequence* ((op-l left-unary-op)
                (arg atom)
                (op-r right-unary-op))
      (result (cons (list op-r op-l) arg))))

  (define unary
    (any-of left-right-unary right-unary left-unary atom))

  ;; api

  (define (parse-expr s)
    (parse (followed-by expr end-of-input) s)))
