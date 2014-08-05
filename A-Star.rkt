; graph is defined as a list of (x, y, weight)
(define test-graph
  '((0 1 5)
    (0 2 10)
    (1 3 20)
    (2 3 10)
    (2 4 1000)
    (3 4 30)))

; build graph node
(define (graph-node from to weight)
  (list from to weight))

(define (edge-from edge)
  (car edge))
(define (edge-to edge)
  (cadr edge))
(define (edge-weight edge)
  (caddr edge))

; find edge by vertices
(define (find-edge graph from to)
  (cond
    ((null? graph) '())
    (#t 
     (let ((edge (car graph)))
       (cond
         ((and (= (edge-from edge) from) (= (edge-to edge) to)) edge)
         (#t (find-edge (cdr graph) from to)))))))

; find edges from a vertex
(define (find-neighbours graph vertex)
  (define (find-neighbours-0 graph collector)
    (cond
      ((null? graph) collector)
      ((eq? (edge-from (car graph)) vertex) 
       (find-neighbours-0 (cdr graph) (cons (car graph) collector)))
      (#t
       (find-neighbours-0 (cdr graph) collector))))
  (find-neighbours-0 graph '()))

; frontier is defines as a list of (vertex, weight, previous) sorted by weight
(define test-frontier '((1 1 1 '()) (2 2 2 '()) (3 3 3 '()) (5 5 5 '()) (7 7 7 '())))

; build frontier node
(define (fr-node vertex weight estimate previous)
  (list vertex weight estimate previous))

; vertex this node describes
(define (fr-vertex f-node)
  (car f-node))
; current path weight
(define (fr-weight f-node)
  (cadr f-node))
; current path + estimated value
(define (fr-estimate f-node)
  (caddr f-node))
; link to previous node in path
(define (fr-previous f-node)
  (cadddr f-node))

; insert new node in frontier
(define (insert-frontier-node frontier node)
  (cond
    ((null? frontier) (cons node '()))
    ((> (fr-estimate node) (fr-estimate (car frontier)))
     (cons (car frontier) (insert-frontier-node (cdr frontier) node)))
    (#t (cons node frontier))))
; check if node should be inserted (i.e. node not present or present, but worse)
(define (update-frontier? frontier node)
  (cond
    ((null? frontier) #t)
    ((= (fr-vertex node) (fr-vertex (car frontier))) 
     (< (fr-weight node) (fr-weight (car frontier))))
    (#t (update-frontier? (cdr frontier) node))))
     
; remove node from frontier (using only vertex number)
(define (remove-frontier-node frontier node)
  (cond
    ((null? frontier) '())
    ((= (fr-vertex node) (fr-vertex (car frontier))) (cdr frontier))
    (#t (cons (car frontier) (remove-frontier-node (cdr frontier) node)))))

; update frontier if it contains no node or worse node
(define (update-frontier frontier node)
  (if (update-frontier? frontier node)
      (insert-frontier-node (remove-frontier-node frontier node) node)
      frontier))

; check if vertex is contained
(define (contains-node? processed node)
  (cond
    ((null? processed) #f)
    ((= (fr-vertex node) (car processed)) #t)
    (#t (contains-node? (cdr processed) node))))

; update frontier with all relevant nodes
(define (add-all-nodes frontier processed nodes)
  (cond
    ((null? nodes) frontier)
    ((contains-node? processed (car nodes)) 
     (add-all-nodes frontier processed (cdr node)))
    (#t
     (add-all-nodes (update-frontier frontier (car nodes)) processed (cdr nodes)))))

; list of neighbours to list of frontier nodes
(define (neighbours-to-nodes neighbours previous weight-function)
  (define (neighbours-to-nodes-0 neighbours collector)
    (cond
      ((null? neighbours) collector)
      (#t
       (let
           ((node (car neighbours)))
         (neighbours-to-nodes-0 
          (cdr neighbours) 
          (cons 
           (fr-node (edge-to node)
                    (+ (edge-weight node)
                       (fr-weight previous))
                    (+ (edge-weight node)
                       (fr-weight previous)
                       (weight-function (edge-to node)))
                    previous)
           collector))))))
  (neighbours-to-nodes-0 neighbours '()))

; processed nodes are stored in a plain list

; search outline:
; 1) get frontier node
; 2) check if we are at destination
; 3) get neighbours
; 4) check if not processed
; 5) update frontier
; 6) recurse

; this is dijkstra, to make it a* we need to pass a weight function (current,goal=>weight)
; this will require additional node info in graph

(define (a-star-search graph weight-function from to)
  ; helper from current to final
  (define (weight-to-dest from)
    (weight-function from to))
  ; main serach
  (define (search-0 processed frontier)
    (cond
      ; no paths
      ((null? frontier) '())
      (#t
       (let
           ((node (car frontier)))
         (cond
           ; we've reached destination
           ((= (fr-vertex node) to) node)
           (#t
            (let*
                (
                 ; update list of processed nodes
                 (new-processed (cons (fr-vertex node) processed))
                 ; get new neighbours from graph
                 (neighbours 
                  (neighbours-to-nodes (find-neighbours graph (fr-vertex node))
                                       node
                                       weight-to-dest))
                 ; update frontier: remove top node, add neighbours
                 (new-frontier 
                  (add-all-nodes (cdr frontier) new-processed neighbours)))
              (search-0 new-processed new-frontier))))))))
  
  ; reverse path reducing it to just needed info
  (define (collect-path path collector)
    (cond
      ((null? path) collector)
      (#t (collect-path (fr-previous path) (cons (fr-vertex path) collector)))))
  
  ; find path and collect results             
  (let
      ((path (search-0 '() (list (fr-node from 0 0 '())))))
    (cond
      ((null? path) path)
      (#t 
       (cons
        (collect-path path '())
        (fr-weight path))))))

; weight function that could be used to estimate distance in a*
; zero function would make a* act as dijkstra
(define (zero-weight from to)
  0)

; define locations
(define test-locations
  '((0 0 0) (1 5 0) (2 3 6) (3 8 3) (4 8 6)))

; vertex search function
(define (find-vertex locations index)
  (cond
    ((null? locations) '())
    ((= (car (car locations)) index)
     (car locations))
    (#t (find-vertex (cdr locations) index))))

; Euclidean distance
(define (distance x1 y1 x2 y2)
  (sqrt
   (+ (expt (- x2 x1) 2)
      (expt (- y2 y1) 2))))

; Function that gives Eucleadean distances for nodes in graph
(define (spatial-weight-for locations)
  (lambda (from to)
    (let
        ((node1 (find-vertex locations from))
         (node2 (find-vertex locations to)))
      (distance (cadr node1) (caddr node1)
                (cadr node2) (caddr node2)))))
