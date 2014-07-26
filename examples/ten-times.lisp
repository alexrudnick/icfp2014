(defun function1 (state environment)
  (if (>= state 40)
    (cons 0 0)
    (cons (+ 1 state) (/ state 10))))

(cons 0 function1)
