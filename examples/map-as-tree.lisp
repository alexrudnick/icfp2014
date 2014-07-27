(defun iswall (themap x y)
  (== 0 (map-value-at themap x y)))

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

(defun map-value-at (themap x y)
  (elt (elt themap y) x))

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
    (if (< index num-squares)
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
(defun map-tree-lookup (map-tree x y width depth)
  (map-tree-lookup-help map-tree
                        (index-to-path (coords-to-index x y width)
                                       depth)))

;; (tree-depth (buildmap))
(map-tree-lookup (build-map-tree (buildmap))
                 2 2
                 4
                 (tree-depth (buildmap)))
