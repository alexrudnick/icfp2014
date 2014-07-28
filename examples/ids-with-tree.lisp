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
    (map-tree-lookup (car (car (cdr searchnode)))
                     (cdr (car (cdr searchnode)))
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
  (if (and (location-not-in-seen (searchnode-location node) seen)
           (and
               (<= (car node) depth)
               (not (iswall map-tree width treedepth
                                   (car (searchnode-location node))
                                   (cdr (searchnode-location node))))))
      ;; true case: return a new queue where we've added a new search node.
      (cons node stack)
      ;; false case: skip it and give back the same stack
      stack))

(defun newsearchnode (oldnode direction)
  ;; search nodes are (LENGTH, LOCATION, PATH)
  (cons (+ 1 (car oldnode))
        (cons (cons (+ (directiondx direction) (car (searchnode-location  oldnode)))
                    (+ (directiondy direction) (cdr (searchnode-location oldnode))))
              (cons direction (cdr (cdr oldnode))))))

;; so we really want to be like ...
;; get a thing from the queue. if it's a winner, return the path!
;; for each possible direction from that thing, if it's not in the seen
;; list or a wall, enqueue it.
;; search states are represented as (LENGTH, LOCATION, PATH)
;; ... where PATH is a proper list containing the list of directions for
;; getting there from the location we're searching from.

(defun searchnode-location (node)
  (car (cdr node)))

(defun dfs-to-depth (depth stack seen map-tree width treedepth)
  (if (atom stack)
      0 ;;; 999
      (if (search-node-is-dot (car stack) map-tree width treedepth)
          ;; return that path, just reverse it.
          (reverse (cdr (cdr (car stack))))
          (dfs-to-depth
            depth
            (maybe-cons (newsearchnode (car stack) 0)
                        map-tree width treedepth 
                        depth (cons (searchnode-location (car stack)) seen)
              (maybe-cons (newsearchnode (car stack) 1)
                          map-tree width treedepth 
                          depth (cons (searchnode-location (car stack)) seen)
                (maybe-cons (newsearchnode (car stack) 2)
                            map-tree width treedepth 
                            depth (cons (searchnode-location (car stack)) seen)
                  (maybe-cons (newsearchnode (car stack) 3)
                              map-tree width treedepth 
                              depth (cons (searchnode-location (car stack)) seen)
                    (cdr stack)))))
            ;; cons the location into seen
            (cons (searchnode-location (car stack)) seen)
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
         (cons
            (cons 0 (cons (cons x y) 0))
            0) ;; search node in list
         0
         map-tree width treedepth))

(defun update-tree (tr path newvalue)
  (if (atom path)
      newvalue
      (if (car path) ;; if we go right...
          (cons
           (car tr)
           (update-tree (cdr tr)
                        (cdr path)
                        newvalue))
          (cons
           (update-tree (car tr)
                        (cdr path)
                        newvalue)
           (cdr tr)))))

;; state is now (CURRENTPLAN, map-tree, width, treedepth)
(defun construct-new-state (theplan map-tree width treedepth x y direction)
  (cons theplan
    (cons (update-tree map-tree
                       (index-to-path (coords-to-index (+ x (directiondx direction))
                                                       (+ y (directiondy direction))
                                                       width)
                                      treedepth)
                       1)
      (cons width treedepth))))

(defun state-map-tree (state)
  (car (cdr state)))

(defun state-width (state)
  (car (cdr (cdr state))))

(defun state-treedepth (state)
  (cdr (cdr (cdr state))))

(defun ids-return-with-new-plan (thenewplan state world)
    (cons (construct-new-state (cdr thenewplan) ;; path, minus first step
                               (state-map-tree state) ;; map-tree, etc...
                               (state-width state)
                               (state-treedepth state)
                               (car (location world))
                               (cdr (location world))
                               (car thenewplan))
          (car thenewplan)))

(defun ids-bot (state world)
  ;; find the path to the nearest pill or power pill and take a step towards it.
    
    
    ;;(if (atom state)
    ;;;; first run, build up representation...
    ;;(ids-bot (cons 0
    ;;           (cons (build-map-tree (car world))
    ;;                 (cons (map-width (car world))
    ;;                       (tree-depth (car world)))))
    ;;         world)

    ;; subsequent runs
    (ids-return-with-new-plan
      (if (not (atom (car state)))
        ;; existing plan
        (car state)
        ;; otherwise, we have to re-plan.
        (ids-path-to-pill (car (location world))
                          (cdr (location world))
                          (state-map-tree state)
                          (state-width state) 
                          (state-treedepth state)))
    state world))

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
  (cons 0 (cons (cons 1 1) 0)))

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

;; state is now (CURRENTPLAN, map-tree, width, treedepth)
(cons 
      (cons 0
            (cons (build-map-tree (car FIRSTARG))
                  (cons (map-width (car FIRSTARG))
                        (tree-depth (car FIRSTARG)))))
      ids-bot)
