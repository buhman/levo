(import matchable
        srfi-1
        srfi-27
        entropy-clock
        (chicken sort))

;; operator implementations

(random-source-randomize!
 (current-random-source)
 (make-entropy-source-system-clock))

;; random integer in range [1,d]

(define (random d)
  (+ 1 ((current-random-integer) d)))

(define (fac n)
  (case n
    ((0) 1)
    (else (* n (fac (- n 1))))))

(define (roll n d)
  (case n
    ((0) '())
    (else (cons (random d)
                (roll (- n 1) d)))))

(define (log-result prefix val)
  (begin
    (print prefix val)
    val))

(define log-roll
  (compose
   ;;(lambda (v) (log-result "roll " v))
   roll))

(define (sum l)
  (apply + l))

(define (take-cmp cmp n xs)
  (take (sort xs cmp) n))

;; operators

(define (op-func op)
  (match op
    ['add +]
    ['sub -]
    ['mul *]
    ['div /]
    ['neg -]
    ['fac fac]
    ['unary-die (compose car (lambda (d) (log-roll 1 d)))]
    ['sum-die (compose sum log-roll)]
    ['list-die log-roll]
    ['sum sum]
    ['kh-one (compose car (lambda (xs) (take-cmp > 1 xs)))]
    ['kl-one (compose car (lambda (xs) (take-cmp < 1 xs)))]
    ['kh-n (lambda (xs n) (take-cmp > n xs))]
    ['kl-n (lambda (xs n) (take-cmp < n xs))]))

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
            [arg arg])]
      [op (apply-unary op args)])))

;; api

(define (interpret expr)
  (match expr
    [(ops . (args . ()))
     (let* ((args (map interpret args)))
       (apply-ops ops args))]
    [(ops . arg)
     (apply-ops ops (interpret arg))]
    [x x]))
