(define (function1 state environment)
  (if (> state 3)
    (cons 1 0)
    (cons (+ 1 x) x)))

(cons 0 function1)
