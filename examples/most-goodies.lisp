(defun themap (world)
  ;; Given the world representation, return the map object out of it
  (car world))

(defun location (world)
  ;; Given the world representation, return Lambda-Man's location tuple
  (car (cdr (car (cdr world)))))

(defun item-at-row (row x)
  ;; For a given row, check whether we've got a wall.
  (if (== 0 x)
    (car row) ;; 0 means wall
    (item-at-row (cdr row) (- x 1))))

(defun item-at (mymap x y)
  ;; Recursively check whether this map (list of lists) has a wall at those
  ;; coordinates.
  (if (== 0 y)
    (item-at-row (car mymap) x)
    (item-at (cdr mymap) x (- y 1))))

;; 0 up
;; 1 right
;; 2 down
;; 3 left
(defun move-x (x direction)
  (if (or (== direction 0)
          (== direction 2))
    x
    (if (== direction 1)
      (+ x 1)
      (- x 1))))

(defun move-y (y direction)
  (if (or (== direction 1)
          (== direction 3))
    y
    (if (== direction 2)
      (+ y 1)
      (- y 1))))

(defun is-goodie (item)
    (and (> item 1) (< item 5)))

(defun goodies-in-direction (map x y direction)
    (if (== 0 (item-at map x y)) ; Stop at walls.
        0
        (if (is-goodie (item-at map x y))
            (+ 1 (goodies-in-direction map (move-x x direction) (move-y y direction) direction))
            (goodies-in-direction map (move-x x direction) (move-y y direction) direction))))

(defun biggest-cdr (a b)
  (if (> (cdr a) (cdr b)) a b))

(defun direction-of-most-goodies (up-pair right-pair down-pair left-pair)
  (car (biggest-cdr
     (biggest-cdr up-pair right-pair)
     (biggest-cdr down-pair left-pair))))

(defun all-pairs-zero (up-pair right-pair down-pair left-pair)
    (== 0 (+ (+ (cdr up-pair) (cdr right-pair)) (+ (cdr down-pair) (cdr left-pair)))))

;;(defun go-clockwise (old-direction)
;;  (if (== 3 old-direction) 0 (+ 1 old-direction)))

(defun choose-direction-from-pairs (up-pair right-pair down-pair left-pair fallback-direction)
    (if (all-pairs-zero up-pair right-pair down-pair left-pair)
      fallback-direction
      (direction-of-most-goodies up-pair right-pair down-pair left-pair)))

(defun mod (x y)
  (- x (* (/ x y) y)))

(defun find-fallback (map x y old-direction ticks)
  (if (== 0 (item-at map (move-x x old-direction) (move-y y old-direction)))
    (mod (+ old-direction (* (* x ticks) y)) 4)
    old-direction))
    

(defun choose-new-direction (map x y old-direction)
  (choose-direction-from-pairs
    (cons 0 (goodies-in-direction map x y 0))
    (cons 1 (goodies-in-direction map x y 1))
    (cons 2 (goodies-in-direction map x y 2))
    (cons 3 (goodies-in-direction map x y 3))
    old-direction))

(defun should-continue (map x y direction)
  (and
    ;; There isn't a wall in that direction.
    (not (== 0 (item-at map (move-x x direction) (move-y y direction))))

    ;; There are beans at all in that direction.
    (> (goodies-in-direction map x y direction) 0)))

(defun double-up (thing)
  (cons thing thing))

(defun direction-to-grab-the-cash (map x y old-direction ticks)
  (if (should-continue map x y old-direction)
    old-direction
    (choose-new-direction map x y (find-fallback map x y old-direction ticks))))

(defun make-answer (new-direction old-ticks)
  (cons (cons (+ 1 old-ticks) new-direction) new-direction))

(defun grab-the-cash (state world)
  (debug (car state))
  (make-answer
    (direction-to-grab-the-cash
      (themap world)
      (car (location world))
      (cdr (location world))
      (cdr state)
      (car state))
    (car state)))

(cons (cons 0 0) grab-the-cash)
