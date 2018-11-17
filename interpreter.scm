(use matchable)

(define (op-func op)
  (match op
    ['add +]
    ['sub -]
    ['mul *]
    ['div /]
    ['neg -]))

(define (apply-op op args)
  (let ((func (op-func op))
        (a (car args))
        (b (cadr args)))
    (cons (func a b) (cddr args))))

(define (apply-ops ops args)
  (match ops
    [(op . rest) (apply-ops rest (apply-op op args))]
    [() (car args)]
    ;; a unary op; can only have one arg
    [op ((op-func op) args)]))

(define (interp expr)
  (match expr
    [(ops . (args . ()))
     (let* ((args (map interp args)))
       (apply-ops ops args))]
    [(ops . arg)
     (apply-ops ops (interp arg))]
    [x x]))
