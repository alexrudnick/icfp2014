(defun elt (ls i)
  (if (== 0 i)
      (car ls)
      (elt (cdr ls) (- i 1))))

(defun map-value-at (themap x y)
  (elt (elt themap y) x))

(defun buildmap ()
    (cons
       (cons 0 (cons 0 (cons 0 (cons 0 0))))
         (cons
           (cons 0 (cons 1 (cons 1 (cons 0 0))))
             (cons
               (cons 0 (cons 1 (cons 2 (cons 0 0))))
                  (cons
                     (cons 0 (cons 0 (cons 0 (cons 0 0))))
                        0)))) )

(map-value-at (buildmap) 2 2)
