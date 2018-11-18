(use test)

(use grammar
     interpreter
     srfi-27)

;; grammar

(test-group
 "grammar"
 (test "literal 1"
       1
       (parse-expr "1"))
 (test "whitespace"
       1
       (parse-expr " 1 "))
 (test "single operator"
       '((add) (1 2))
       (parse-expr "1+2"))
 (test "multiple operator"
       '((add sub) (1 2 3))
       (parse-expr "1+2-3"))
 (test "operator whitespace"
       '((add sub) (1 2 3))
       (parse-expr "1 + 2 - 3"))
 (test "operator precedence"
       '((add) (((mul) (1 2)) 3))
       (parse-expr "1*2+3"))
 (test "paren reduction"
       '((add) (1 2))
       (parse-expr "(((1)+2))"))
 (test "paren precedence"
       '((mul) (1 ((add) (2 3))))
       (parse-expr "1*(2+3)"))
 (test "unary negation"
       '(neg . 1)
       (parse-expr "-1"))
 (test "unary left precedence"
       '((add) ((neg . 1) 2))
       (parse-expr "-1 + 2"))
 (test "unary left invalid"
       #f
       (parse-expr "1 + -2"))
 (test "unary right invalid"
       #f
       (parse-expr "1! + 2"))
 (test "unary right with paren"
       '((add) ((fac . 1) 2))
       (parse-expr "(1!) + 2"))
 ;; XXX is this a misfeature?
 (test "unary left right precedence"
       '((fac neg) . 1)
       (parse-expr "-1!")))

(test-group
 "grammar dice"
 (test "unary die"
       '(unary-die . 20)
       (parse-expr "d20"))
 (test "dice precedence"
       '((add add) (1 ((sum-die) (2 3)) 4))
       (parse-expr "1+2d3+4"))
 (test "dice sum"
       '(sum (list-die) (1 2))
       (parse-expr "1ld2s"))
 (test "dice keep unary"
       '(kh-one (list-die) (1 2))
       (parse-expr "1ld2kh"))
 (test "dice keep operator"
       '((list-die kh-n) (1 2 3))
       (parse-expr "1ld2kh3"))
 (test "dice dependent roll"
       '((sum-die sum-die) (1 2 3))
       (parse-expr "1d2d3"))
 (test "dice dependent roll with precedence"
       '((sum-die) (1 ((sum-die) (2 3))))
       (parse-expr "1d(2d3)"))
 (test "dice dependent roll keep"
       '(kl-one (sum-die list-die) (1 2 3))
       (parse-expr "1d2ld3kl")))

;; interpreter

(define interp (o interpret parse-expr))

(test-group
 "interpreter"
 (test "quick maffs"
       3
       (interp "1+2"))
 (test "left to right"
       -4
       (interp "1-2-3"))
 (test "multiply precedence"
       7
       (interp "1+2*3"))
 (test "paren precedence"
       9
       (interp "(1+2)*3"))
 (test "negation"
       -1
       (interp "-1"))
 (test "paren negation"
       -3
       (interp "-(1+2)"))
 (test "factorial"
       120
       (interp "5!")))

(define (reset-random x)
  (random-source-pseudo-randomize! default-random-source 1 1)
  x)

(define interp-rand (o interp reset-random))

(test-assert "pseudo-random sanity"
  (let* ((f (o (current-random-integer) reset-random))
         (a (f 20))
         (b (f 20)))
    (= a b 15)))

(test-group
 "interpreter dice"
 (test "single die"
       16
       (interp-rand "d20"))
 (test "roll with modifier"
       17
       (interp-rand "d20+1"))
 (test "compound roll"
       16
       (interp-rand "1d20"))
 (test "compound multiple roll"
       25
       (interp-rand "2d20"))
 (test "multiply roll"
       50
       (interp-rand "2 * 2d20"))
 (test "multiply dice"
       46
       (interp-rand "(2*2)d20"))
 (test "list dice"
       '(16 9)
       (interp-rand "2ld20"))
 (test "list dice highest"
       16
       (interp-rand "2ld20kh"))
 (test "list dice highest-n"
       '(6 5 4)
       (interp-rand "5ld6kh3"))
 (test "dice distribution"
       '(20 1)
       (let* ((h (lambda (xs) (car (sort xs >))))
              (l (lambda (xs) (car (sort xs <))))
              (xs (interp-rand "40ld20")))
         (list (h xs) (l xs)))))

(test-exit)
