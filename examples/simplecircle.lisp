(defun function1 (state environment)
  (if (> state 3)
    (cons 1 0)
    (cons (+ 1 state) state)))

(cons 0 function1)
