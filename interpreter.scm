(use matchable)

(define (op-func op)
  (match op
    ['add +]
    ['sub -]
    ['mul *]
    ['div /]))

(define (apply-op op args)
  (let ((func (op-func op))
        (a (car args))
        (b (cadr args)))
    (cons (func a b) (cddr args))))

(define (apply-ops ops args)
  (match ops
    [(op . rest) (apply-ops rest (apply-op op args))]
    [() args]))

(define (interp expr)
  (match expr
    [(ops . args)
     (let* ((args (map interp (car args)))
            (stack (apply-ops ops args)))
       (car stack))]
    [x x]))
