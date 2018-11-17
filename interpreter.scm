(use matchable)


; write our own, because why not
(define (fac n)
  (case n
    ((0) 1)
    (else (* n (fac (- n 1))))))

(define (op-func op)
  (match op
    ['add +]
    ['sub -]
    ['mul *]
    ['div /]
    ['neg -]
    ['fac fac]))

(define (apply-op op args)
  (let ((func (op-func op))
        (a (car args))
        (b (cadr args)))
    (cons (func a b) (cddr args))))

(define (apply-unary op arg)
  ((op-func op) arg))

(define (apply-ops ops args)
  (let ((app-func (match args
                    [(_ . _) apply-op]
                    [arg apply-unary])))
    (match ops
      [(op . rest) (apply-ops rest (app-func op args))]
      [() (match args
            [(arg . ()) arg]
            [arg arg])])))

(define (interp expr)
  (match expr
    [(ops . (args . ()))
     (let* ((args (map interp args)))
       (apply-ops ops args))]
    [(ops . arg)
     (apply-ops ops (interp arg))]
    [x x]))
