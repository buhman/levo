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

  (define (as-operator parser)
    (bind parser
          (o result char->operator)))

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

  (define tail-op
    (as-operator (in #\* #\/)))

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
     (sequence* ((op tail-op)
                 (tail atom))
       (result (cons op tail)))))

  (define term
    (recursive-parser
     (sequence* ((head atom)
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

  ;; api

  (define (parse-expr s)
    (parse expr s)))
