(defun location (world)
  ;; Given the world representation, return Lambda-Man's location tuple
  (car (cdr (car (cdr world)))))

(defun getthemap (world)
  ;; Given the world representation, return the map object out of it
  (car world))

(defun iswall (themap x y)
  (== 0 (map-value-at themap x y)))

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
  (iswall (getthemap world)
          (+ x  (directiondx direction))
          (+ y  (directiondy direction))))

(defun location-eq (loc1 loc2)
  ;; true if two locations are equal. locations are represented as x,y pairs
  (and (== (car loc1) (car loc2))
       (== (cdr loc1) (cdr loc2))))

(defun location-not-in-seen (loc seen)
  (if (atom seen) 1
      (if (location-eq loc (car seen))
          0
          (location-not-in-seen loc (cdr seen)))))

(defun search-node-is-dot (searchnode themap)
  (member
    (map-value-at themap
                  (car (car searchnode))
                  (cdr (car searchnode)))
    (cons 2 (cons 3 0))))

(defun elt (ls i)
  (if (== 0 i)
      (car ls)
      (elt (cdr ls) (- i 1))))

(defun member (item ls)
  ;; true if the item is in the list
  (if (atom ls)
      0
      (if (== (car ls) item)
          1
          (member item (cdr ls)))))

(defun append (ls item)
  ;; build a new list where the item is the new last thing in the list
  (if (atom ls)
      (cons item 0)
      (cons (car ls) (append (cdr ls) item))))

(defun maybe-append (node themap seen queue)
  ;; might return a new queue, or the same old queue
  ;; if the specified direction from the current search node has not been seen,
  ;; and is not a wall, add it to the queue.
  (if (and (location-not-in-seen (car node) seen)
           (not (iswall themap (car (car node))
                               (cdr (car node)))))
      ;; true case: return a new queue where we've appended a new search node.
      (append queue node)
      ;; false case: skip it and give back the same queue
      queue))

(defun newsearchnode (oldnode direction)
  ;; search nodes are (cons LOCATION PATH)
  (cons (cons (+ (directiondx direction) (car (car oldnode)))
              (+ (directiondy direction) (cdr (car oldnode))))
        (append (cdr oldnode) direction)))

;; so we really want to be like ...
;; get a thing from the queue. if it's a winner, return the path!
;; for each possible direction from that thing, if it's not in the seen
;; list, enqueue it.
;; search states are represented as (cons LOCATION PATH)
;; ... where PATH is a proper list containing the list of directions for
;; getting there from the location we're searching from.
(defun bfs (themap queue seen)
  (if (search-node-is-dot (car queue) themap)
      (cdr (car queue)) 
      (bfs
        themap
        (maybe-append (newsearchnode (car queue) 0) themap seen
          (maybe-append (newsearchnode (car queue) 1) themap seen
            (maybe-append (newsearchnode (car queue) 2) themap seen
              (maybe-append (newsearchnode (car queue) 3) themap seen
                (cdr queue)))))
        (cons (car (car queue)) seen))))

(defun bfs-path-to-pill (themap x y)
  (bfs themap
       (cons (cons (cons x y) 0)
             0)
       0))

(defun bfs-bot (state world)
  ;; find the path to the nearest pill or power pill and take a step towards it
  (cons 0 (car (bfs-path-to-pill
                  (getthemap world)
                  (car (location world))
                  (cdr (location world))))))

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

(defun build-search-node ()
  (cons (cons 1 1) 0))

;;(bfs-path-to-pill
;;     (buildmap)
;;     1 1)

;; for reals, do this.
(cons 0 bfs-bot)
