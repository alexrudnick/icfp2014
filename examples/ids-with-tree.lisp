(defun location (world)
  ;; Given the world representation, return Lambda-Man's location tuple
  (car (cdr (car (cdr world)))))

(defun getthemap (world)
  ;; Given the world representation, return the map object out of it
  (car world))

(defun iswall (map-tree width treedepth x y)
  (== 0 (map-tree-lookup x y map-tree width treedepth)))

;;; Code for map-as-tree

;; given a list of lists, turn it into just a flat list
(defun flatten (lss)
  (if (atom lss) 0
      (if (atom (cdr lss))
          (car lss) ;; the last sublist
          (if (atom (cdr (car lss)))
              (cons (car (car lss)) (flatten (cdr lss)))
              (cons (car (car lss))
                    (flatten (cons (cdr (car lss))
                                   (cdr lss))))))))

(defun build-map ()
    (cons
       (cons 0 (cons 0 (cons 0 (cons 0 0))))
         (cons
           (cons 0 (cons 1 (cons 1 (cons 0 0))))
             (cons
               (cons 0 (cons 1 (cons 27 (cons 0 0))))
                  (cons
                     (cons 0 (cons 0 (cons 0 (cons 0 0))))
                        0)))) )

(defun map-width (themap)
  (length (car themap)))

(defun number-of-squares (themap)
  (* (length themap) (map-width themap)))

(defun tree-depth-help (nsquares hypothesis current-exponent)
  (if (>= current-exponent nsquares)
    hypothesis
    (tree-depth-help nsquares (+ 1 hypothesis) (* 2 current-exponent))))

;; compute how many bits (depth of the tree) we're going to need to represent a
;; tree structure for this map
(defun tree-depth (themap)
  (tree-depth-help (number-of-squares themap) 1 2))

(defun coords-to-index (x y width)
  (+ (* width y) x))

(defun rev-help (ls acc)
  (if (atom ls)
    acc
    (rev-help (cdr ls) (cons (car ls) acc))))

(defun reverse (ls)
  (rev-help ls 0))

;; return this number as a list of binary digits, most significant last
(defun index-to-path-help (n depth)
  (if (== depth 1)
    (cons n 0)
    ;; cons the lowest bit onto the rest.
    (cons (- n (* (/ n 2) 2))
          (index-to-path-help (/ n 2) (- depth 1)))))

(defun index-to-path (n depth)
  (reverse (index-to-path-help n depth)))

;; return either more tree, or a map code number
(defun build-map-tree-help (flatmap num-squares depth index)
  ;; if we're at depth 0, we should have calculated our index by now -- get that
  ;; element from the map
  (if (== 0 depth)
    (if (> num-squares index)
      (elt flatmap index)
      (- 0 1))
    ;; otherwise, recursively build more tree
    (cons
      (build-map-tree-help flatmap num-squares (- depth 1) (* 2 index))
      (build-map-tree-help flatmap num-squares (- depth 1) (+ 1 (* 2 index))))))

(defun build-map-tree (themap)
  (build-map-tree-help
    (flatten themap)
    (number-of-squares themap)
    (tree-depth themap)
    0))

(defun map-tree-lookup-help (map-tree path)
  (if (atom path)
      map-tree
      (if (car path)
          (map-tree-lookup-help (cdr map-tree) (cdr path))
          (map-tree-lookup-help (car map-tree) (cdr path)))))

;; given a map-tree, look up the item at map location x y
(defun map-tree-lookup (x y map-tree width treedepth)
  (map-tree-lookup-help map-tree
                        (index-to-path (coords-to-index x y width)
                                       treedepth)))

;;; End code for map-as-tree




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

(defun location-eq (loc1 loc2)
  ;; true if two locations are equal. locations are represented as x,y pairs
  (and (== (car loc1) (car loc2))
       (== (cdr loc1) (cdr loc2))))

(defun location-not-in-seen (loc seen)
  (if (atom seen) 1
      (if (location-eq loc (car seen))
          0
          (location-not-in-seen loc (cdr seen)))))

(defun search-node-is-dot (searchnode map-tree width treedepth)
  (member
    (map-tree-lookup (car (car searchnode))
                     (cdr (car searchnode))
                     map-tree
                     width
                     treedepth)
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

(defun length (ls)
  (if (atom ls)
      0
      (+ 1 (length (cdr ls)))))

(defun maybe-cons (node map-tree width treedepth depth seen stack)
  ;; might return a new stack, or the same old stack
  ;; if the specified direction from the current search node has not been seen,
  ;; and is not a wall, add it to the stack.
  (if (and (location-not-in-seen (car node) seen)
           (and
               (< (length (cdr node)) depth)
               (not (iswall map-tree width treedepth
                                   (car (car node))
                                   (cdr (car node))))))
      ;; true case: return a new queue where we've added a new search node.
      (cons node stack)
      ;; false case: skip it and give back the same stack
      stack))

(defun newsearchnode (oldnode direction)
  ;; search nodes are (cons LOCATION PATH)
  (cons (cons (+ (directiondx direction) (car (car oldnode)))
              (+ (directiondy direction) (cdr (car oldnode))))
        (append (cdr oldnode) direction)))

;; so we really want to be like ...
;; get a thing from the queue. if it's a winner, return the path!
;; for each possible direction from that thing, if it's not in the seen
;; list or a wall, enqueue it.
;; search states are represented as (cons LOCATION PATH)
;; ... where PATH is a proper list containing the list of directions for
;; getting there from the location we're searching from.

(defun dfs-to-depth (depth stack seen map-tree width treedepth)
  (if (atom stack)
      0 ;;; 999
      (if (search-node-is-dot (car stack) map-tree width treedepth)
          (cdr (car stack))
          (dfs-to-depth
            depth
            (maybe-cons (newsearchnode (car stack) 0)
                        map-tree width treedepth 
                        depth (cons (car (car stack)) seen)
              (maybe-cons (newsearchnode (car stack) 1)
                          map-tree width treedepth 
                          depth (cons (car (car stack)) seen)
                (maybe-cons (newsearchnode (car stack) 2)
                            map-tree width treedepth 
                            depth (cons (car (car stack)) seen)
                  (maybe-cons (newsearchnode (car stack) 3)
                              map-tree width treedepth 
                              depth (cons (car (car stack)) seen)
                    (cdr stack)))))
            (cons (car (car stack)) seen)
            map-tree
            width
            treedepth))))

(defun ids (depth fresh-stack seen map-tree width treedepth)
  (ids-help depth
            (dfs-to-depth depth fresh-stack seen
                          map-tree width treedepth)
            fresh-stack
            map-tree
            width
            treedepth))

(defun ids-help (depth result fresh-stack map-tree width treedepth)
  (if (not (atom result)) result
      (ids (+ 1 depth) ;; we can bump this up to go faster, or multiply even!
           fresh-stack
           0
           map-tree width treedepth)))

(defun ids-path-to-pill (x y map-tree width treedepth)
    (ids 1             ;; could also start with bigger search for speed
                       ;; (at the expense of accuracy, of course)
         (cons (cons (cons x y) 0) 0) ;; search node in list
         0
         map-tree width treedepth))

(defun ids-return-path (path)
  ;; given a path, return a tuple containing the cdr of the path and the move we
  ;; want to make, ie, the car of the path.
  (cons (cdr path) (car path)))

(defun ids-bot (state world)
  ;; find the path to the nearest pill or power pill and take a step towards it.
  ;; state, if non-zero, is the current path we're on.
  (if 0 ;; (not (atom state))
    (cons (cdr state) (car state))

    ;; otherwise, we have to re-plan.
    (ids-return-path
      (ids-path-to-pill (car (location world))
                        (cdr (location world))
                        (build-map-tree (getthemap world))
                        (map-width (getthemap world))
                        (tree-depth (getthemap world)) ))))

(defun build-map ()
    (cons
       (cons 0 (cons 0 (cons 0 (cons 0 0))))
         (cons
           (cons 0 (cons 2 (cons 1 (cons 0 0))))
             (cons
               (cons 0 (cons 1 (cons 1 (cons 0 0))))
                  (cons
                     (cons 0 (cons 0 (cons 0 (cons 0 0))))
                        0)))) )

(defun build-search-node ()
  (cons (cons 1 1) 0))

(defun build-demo-world ()
  (cons (build-map)
    (cons
          ;; lambda-man's status
          (cons 0   ;; lambda-man's status
            (cons (cons 2 2) 0))
          0)))

;; (ids-bot 0 (build-demo-world))

;;(dfs-to-depth 5
;;              (cons (build-search-node) 0) ;; search node in list
;;              0
;;              (build-map-tree (build-map) )
;;              4
;;              4)

;; for reals, do this.
(cons 0 ids-bot)
