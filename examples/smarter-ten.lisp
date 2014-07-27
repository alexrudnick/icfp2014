(defun calc-direction (state)
  (/ state 10))

(defun themap (world)
  ;; Given the world representation, return the map object out of it
  (car world))

(defun location (world)
  ;; Given the world representation, return Lambda-Man's location tuple
  (car (cdr (car (cdr world)))))

(defun iswall-row (row x)
  ;; For a given row, check whether we've got a wall.
  (if (== 0 x)
    (== 0 (car row)) ;; 0 means wall
    (iswall-row (cdr row) (- x 1))))

(defun iswall (mymap x y)
  ;; Recursively check whether this map (list of lists) has a wall at those
  ;; coordinates.
  (if (== 0 y)
    (iswall-row (car mymap) x)
    (iswall (cdr mymap) x (- y 1))))

;; 0 up
;; 1 right
;; 2 down
;; 3 left
(defun directiondx (direction)
  (if (or (== direction 0)
          (== direction 2))
    0
    (if (== direction 1)
      1
      (- 0 1))))

(defun directiondy (direction)
  (if (or (== direction 1)
          (== direction 3))
    0
    (if (== direction 2)
      1
      (- 0 1))))

(defun wallp (world x y direction)
  ;; Given a world representation and your current x and y, return whether the
  ;; proposed direction is a wall.
  (iswall (themap world)
          (+ x  (directiondx direction))
          (+ y  (directiondy direction))))

(defun smarter-ten (state world)
  ;; if we haven't looped yet, try going in the appropriate direction, else loop
  (if (<= state 40)
    (if (wallp world (car (location world))
                     (cdr (location world)) ;; xxx aren't locations pairs?
                     (calc-direction state))
      (smarter-ten (+ 10 state) world)
      (cons (+ 1 state) (calc-direction state)))
    (cons 0 0)))

;; for testing
;;(smarter-ten 0
;;    (cons 
;;
;;     ;; map
;;     (cons
;;       (cons 0 (cons 0 (cons 0 0)))
;;       (cons 
;;         (cons 0 (cons 2 (cons 2 0)))
;;         0))
;;
;;      (cons 
;;        ;; status
;;        (cons 0 (cons (cons 1 1) 0))
;;
;;        0)))

(cons 0 smarter-ten)
