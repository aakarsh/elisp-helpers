(require 'cl)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro an/string:set (str v)
  `(setq k ,str ,v))

(defmacro an/string:concat(str  &rest s)
  `(setq ,str (concat ,str ,@s)))

(defun an/string:repeat(c n)
  (let ((r ""))
  (while (> n 0)
    (an/string:concat r c)
    (setq n (- n 1)))r))

(defun an/string:ltrim (str)
  (let ((trim-pos (string-match "\\s +$" str)))
    (if trim-pos
        (substring str 0 trim-pos)
      str)))

(defun an/string:empty (str)
  (equalp (length (an/string:trim str)) 0))

(defun an/string:trim (str)
  (an/string:rtrim (an/string:ltrim str)))

(defun an/string:split (str &optional sep omit-nulls trim)
  (if (not sep)
      (setf  sep "\\s+"))
   (split-string str sep omit-nulls trim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun an/list:split (pos list)
  (let ((first) (second list) (count 0 ))
      (while (< count pos)
            (setq first (append first (list (car second))))
            (setq second (cdr second))
            (setq count (+ count 1)))
      (list first second)))

(defun an/list:join(lst)
  (mapconcat 'identity lst " , "))

(defun an/list:filter (fn l)
  "Filter a list using a predicate"
  (if (not l)
      '()
    (if (funcall fn (car l))
        (cons (car l)
              (an/list:filter fn (cdr l)))
      (an/list:filter fn (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun g/vector-list(ls)
  (loop with v = (make-vector (length ls) 0)
        for l in ls
        for i = 0 then (+ i 1)
        do (aset v i l)
        finally (return v)))

(defun an/buffer:clear-current-buffer()
  (an/buffer:clear (current-buffer)))

(defun an/buffer:clear (&optional buffer)
  (if (not buffer)
      (setq buffer (current-buffer)))
  (with-current-buffer (get-buffer buffer)
    (if (> (point-max) 1)
        (delete-region (point-min) (point-max)))))

(defmacro an/buffer:run-in-scratch(&rest body)
  `(with-current-buffer (get-buffer-create "*scratch*")
     (kill-region  (point-min) (point-max))
     ,@body
     (switch-to-buffer "*scratch*")
     (goto-char 0)))

(defun an/buffer:insert-scratch (&optional clearp)
  (interactive "p")
  (insert-buffer (get-buffer "*scratch*"))
  (if clearp
      (an/buffer:clear-scratch)))

(defun an/buffer:list ()
  (mapcar (lambda (b) (substring-no-properties (buffer-name b))) (buffer-list)))

(defun an/buffer:match (regexp)
  (remove-if-not (lambda(s) (string-match regexp s)) (an/buffer:list)))

(defun an/buffer:fetch-lines(n)
  (let ((reg-begin nil)
        (reg-end nil))
  (save-excursion
    (goto-char (line-beginning-position))
    (setf reg-begin (point))
    (forward-line n)
    (goto-char (line-end-position))
    (setf reg-end (point)))
  (forward-line n) ;; change buffer state so curser is n-lines ahead
  (let ((region (buffer-substring-no-properties reg-begin reg-end)))
    (split-string region "\n" t))))

(defun an/buffer:fetch-line()
  (car (an/buffer:fetch-lines 1)))

(defun an/buffer:num-lines()
  (count-lines (point-min) (point-max)))

(defun an/vector-list(ls)
  "Converts a list of objects to a vector of objects."
  (loop with v = (make-vector (length ls) 0)
        for l in ls
        for i = 0 then (+ i 1)
        do (aset v i l)
        finally (return v)))



(defun an/buffer:fetch-lines-as-numbers()
  (an/vector-list
   (mapcar 'string-to-number
           (split-string (an/buffer:fetch-line) " "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directory helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'find-lisp)

;; directory predicates
(defalias 'an/dir:is-directory-p 'file-directory-p)

(defalias  'an/file:find-files-recurse 'find-lisp-find-files)

(defun an/false-predicate(&rest arg) nil)

(defun an/file:find-subdir(directory regexp)
  "Get list of DIRECTORY names matching REGEXP"
 (an/list:filter 'file-directory-p (directory-files directory t regexp nil)))

(defun an/file:find-files (directory regexp)
  "Find files under DIRECTORY,  that match REGEXP."
  (let ((file-predicate      'find-lisp-default-file-predicate)
	(directory-predicate 'an/false-predicate)
	(find-lisp-regexp regexp))
    (find-lisp-find-files-internal
     directory
     file-predicate
     directory-predicate)))

(defun an/file:file-mv-extenstion(dir old-extension new-extension)
  "Reersively move files "
  (loop for file in (an/file:find-files-recurse  dir (format ".*%s" old-extension))
        do
        (let ((file-no-extension (file-name-sans-extension file)))
          (shell-command (format "mv %s %s.%s" file file-no-extension new-extension)))))

(defmacro an/file:for-each-subdir(directory regexp &rest body )
  `(loop for file in (an/file:find-subdir ,directory ,regexp)
         do
         (let ((default-directory file))
           ,@body)))

(defun an/file:map-over-file(input-file f)
  (with-temp-buffer
    (insert-file-contents input-file t)
    (goto-char 0)
    (loop with num-lines = (an/buffer:num-lines)
          for current-line in (an/buffer:fetch-lines num-lines)
          for i = 0 then (+ i 1) do
          (funcall  f i current-line))))


(cl-defmacro an/parse-over-file(in-file (line,count)  => (line-var,count-var)
                                        &key (first nil) (second nil) (rest nil))
  "Tries to make file mapping easier to read. specify variables
to bind do line and count, along with parsers for first,second,
and rest of the lines."
  `(an/file:map-over-file
    ,in-file
    (lambda (,count-var ,line-var)
      (cond
       ((= ,count-var 0) ,first)
       ,(if second
             `((= ,count-var 1) ,second))
       (t ,rest)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun an/shell:visit-buffer-dir()
  "Try to go to relevant directory in *shell*"
  (interactive)
  (let* ((pwd (shell-command-to-string "pwd"))
        (chdir-cmd (format "cd %s\r" pwd))
        (file-buffer (current-buffer))
        (shell-buffer (get-buffer "*shell*"))
        (shell-process (get-buffer-process shell-buffer))
        )
    (process-send-string shell-process chdir-cmd)
    (pop-to-buffer shell-buffer t)
    (comint-clear-buffer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun an/windows:swap()
  "Swap buffers in `selected-window' and `other-window'"
  (interactive)
  (let* ((w1 (selected-window))
        (w2 (other-window 1))
        (b1 (window-buffer w1))
        (b2 (window-buffer w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vectors - helpers for dealing with vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun an/vector:make(length constructor)
  (let ((retval (make-vector length nil)))
    (loop for i from 0 below length do
          (aset  retval i (funcall constructor i)))
    retval))

(defun an/buffer:line-to-numbers(line)
  (an/vector-list
   (mapcar 'string-to-number
           (split-string line " "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table - helpers for two dimension vector.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct table/position row column)

(defun table/make(rows cols cell-value)
  (loop repeat rows vconcat
        (vector (loop repeat cols vconcat (make-vector 1 cell-value)))))

(defun table/nrows(table)
  (length table))

(defun table/ncols(table)
  (length (aref table 0)))

(defmacro table/at (table i j)
  `(aref (aref ,table ,i) ,j))

(defmacro table/setf(m i j value)
  `(setf (aref (aref ,m ,i ) ,j)  ,value))

(defmacro table/at-position(table pos)
  `(table/at ,table  (g/position-row ,pos)
             (g/position-column ,pos)))

(defmacro an/swapf(r1 r2)
  `(let ((temp nil))
     (setf  temp ,r2)
     (setf ,r2 ,r1)
     (setf ,r1 temp)))

(defun an/vector-swap (v p1 p2)
  (an/swapf (aref v p1) (aref v p2)))

(defun table/swap-rows(table r1 r2)
  (loop for c from 0 below (g/table-ncols table) do
        (an/swapf (table/at table r1 c) (table/at table r2 c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphs - some collections of graph helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct an/graph
  (nodes nil)
  (type nil)   ;; condition on type of graph
  (matrix nil) ;; an adjacency matrix representation
  (adj-list )) ;; an adjacency list   representation

(defstruct an/graph:node
  number
  ;; (neighbours nil) use external ds for these
  (data nil)
  (component nil)
  (visited nil)
  (start-time -1)
  (finish-time -1))

(defun an/relations:decrement (relations)
  (loop for r in relations
        for v1  = (- (aref r 0) 1)
        for v2  = (- (aref r 1) 1)
        collect (vector v1 v2)))

(defun an/relations:max-member (relations)
  "Returns the maximum vertex in list of relations a list of two
element vectors."
  (loop for rel in relations
        for first-vertex = (aref rel 0)
        for second-vertex = (aref rel 1)
        maximize (max first-vertex second-vertex)))

(cl-defun an/graph:make(type num-vertices relations &key (edge-type nil))
  "Construct a graph with underlying data structure to use
sepcified by type 'matrix 'adj-list. Relations are specified as
a list of vector pairs of vertices."
  (let ((g
         (make-an/graph
          :type type
          :nodes (an/graph:make-nodes num-vertices))))
    (an/graph:init-edge g)
    (loop for rel in relations
          for first-vertex  = (aref rel 0) ;;(-  (aref rel 0) 1)
          for second-vertex = (aref rel 1) ;;(-  (aref rel 1) 1)
          do
          (if (eq edge-type 'undirected )
              (an/graph:add-undirected-edge g first-vertex second-vertex)
              (an/graph:add-directed-edge g first-vertex second-vertex)))
    g))

(defun an/graph:add-directed-edge (graph i j )
  (cond
   ((eq (an/graph-type graph) 'adj-list)
    (an/edge-graph:add-directed-edge graph i j))
   ((eq (an/graph-type graph) 'matrix )
    (matrix-graph:add-directed-edge graph i j))))

(defun an/graph:add-undirected-edge (graph i j )
  (cond
   ((eq (an/graph-type graph) 'adj-list)
    (an/edge-graph:add-undirected-edge graph i j))
   ((eq (an/graph-type graph) 'matrix )
    (matrix-graph:add-undirected-edge graph i j))))

(defun an/graph:init-edge (graph )
  (cond
   ((eq (an/graph-type graph) 'adj-list)
    (setf (an/graph-adj-list graph)
          (edge-graph/make (length (an/graph-nodes graph)))))

   ((eq (an/graph-type graph) 'matrix )
    (setf (an/graph-matrix graph )
          (matrix-graph/make (length (an/graph-nodes graph)))))))

(defun an/graph:make-nodes(num)
  "Returns a vector of sat/nodes with increasing sequence numbers "
  (an/vector:make num (lambda(i) (make-an/graph:node :number i))))

(defun an/graph-component-graph(g num-components)
  (let ((nodes (an/graph-nodes g))
        (graph (an/graph-matrix g)))
    (make-an/graph
     :type 'matrix
     :matrix (an/graph-make-component-graph nodes graph num-components)
     :nodes (an/graph-make-component-nodes nodes num-components))))

(defun an/graph-make-component-graph(nodes graph num-components)
  "A component graph is a directed acyclic graph which summarises
the original graph containing one vertex per strongly connected
component in the graph."
  (let ((component-graph (table/make num-components num-components nil)))
    (loop for i from 0 below (table/nrows graph )
          for i-cmp =  (an/graph:node-component (aref nodes i)) do
          (loop for j from 0 below (table/ncols graph)
                for j-cmp =  (an/graph:node-component (aref nodes j)) do
                ;; change component number to index
                (if (and  (table/at graph i j)   (not (eq i-cmp j-cmp)))
                    (table/setf component-graph i-cmp j-cmp t))))
    component-graph))

(defun an/graph-make-component-nodes(nodes num-components)
  (let ((component-nodes (an/graph:make-nodes num-components)))
    (loop for node across nodes
          for component-number = (an/graph:node-component node)
          for component = (aref component-nodes component-number ) do
          (push node (an/graph:node-data component)))
    component-nodes))

(defun an/graph-neighbours(graph node)
  (if (an/graph-matrix graph)
      (matrix-graph/neighbours node (an/graph-matrix graph) (an/graph-nodes graph))
    (edge-graph/neighbours node (an/graph-adj-list graph)  (an/graph-nodes graph))))

(defun an/graph-reverse (g)
  (if (an/graph-matrix g)
      (setf (an/graph-matrix g) (matrix-graph/reverse (an/graph-matrix g)))
    (setf (an/graph-adj-list g) (edge-graph/reverse (an/graph-adj-list g)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjacency Matrix Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-graph/make(size)
  "Simple two dimensional table representing adjacency matrix"
  (table/make size size nil))

(defun matrix-graph/reverse(graph )
  "Create a reverse graph such that if (i,j) is edge in G
then (j,i) is an edge in reverse(G)"
  (loop
   with size = (length graph)
   with new-graph = (table/make size size nil)
   finally (return new-graph)
   for row from 0 below (table/nrows  graph) do
   (loop for col below  (table/ncols graph)  do
         (if (table/at graph row col)
             (table/setf new-graph col row t)))))

(defun matrix-graph/neighbours(node graph nodes)
  "Retrun a vector of neighbours of node."
  (loop for neighbours-p across (aref graph (an/graph:node-number node))
        for pos = 0 then (+  pos 1)
        if neighbours-p collect (aref nodes pos)))

(defun matrix-graph:add-directed-edge (graph  i j)
  (table/setf (an/graph-matrix graph)  i j t))

(defun matrix-graph:add-undirected-edge (graph  i j)
  (matrix-graph:add-directed-edge graph i j)
  (matrix-graph:add-directed-edge graph j i))

(defun graph/make-nodes(num)
  "Returns a vector of sat/nodes with increasing sequence numbers "
  (g/make-vector num (lambda(i) (make-sat/node :number i))))

(defun an/graph:nodes-clear-visited (nodes)
  "Mark all nodes as unvisited."
  (loop for node across nodes do
        (setf (an/graph:node-visited node) nil)))

(cl-defun an/graph:dfs-visit-graph (g  &key (traverse-order nil) (post-dfs nil) (pre-visit nil) (post-visit nil))
  "Visit a complete graph using dfs. Restarting on each
exhaustion, assumes node is vector."
  (let ((nodes (an/graph-nodes g)))
    (an/graph:nodes-clear-visited nodes)
    (loop for node across
          (if traverse-order
              traverse-order  nodes)
          do
          (if (not (an/graph:node-visited node))
              (progn
                (message "Call dfs-visit :" (an/graph:node-number node))
                (an/graph:dfs-visit g nil node :pre-visit pre-visit :post-visit post-visit)
                (if post-dfs
                    (funcall post-dfs g node)))))
    (an/graph:nodes-clear-visited nodes)))


(cl-defun an/graph:dfs-visit(g parent node &key (pre-visit nil) (post-visit nil))
  "Runs dfs on a `graph' represented by and adjaceny matrix of
vectors, `nodes' is a of nodes containing auxiliary information
about graph nodes. `node' is the node to visit. `pre-visit' and
`post-visit' are optional key word callbacks called before and
after visiting `node`. "
  (let ((nodes (an/graph-nodes g)))
    (message "an/graph:dfs-visit:call %d" (an/graph:node-number node))
      (let* ((node-num (an/graph:node-number node))
         (initial-node node))
    ;; mark called-node as visiting
    (setf (an/graph:node-visited node) 'visiting)
    (if pre-visit
        (progn
          (funcall pre-visit g parent node)))
    (loop for node in (an/graph-neighbours g initial-node)
          ;;(matrix-graph/neighbours initial-node graph nodes)
          finally
          (progn
            (message "Finished Visiting : %d , %s" (an/graph:node-number initial-node) initial-node)
            (setf (an/graph:node-visited initial-node) 'visited)
            (if post-visit
                (funcall post-visit g parent initial-node)))

          if (not (an/graph:node-visited node))
          do
          (an/graph:dfs-visit g initial-node node :post-visit post-visit :pre-visit pre-visit)
          (setf (an/graph:node-visited node) 'visited)))))


(defun an/graph:dfs-post-order (g)
  "Computes the ordering of nodes, from last to finish to first to finish"
  (let ((node-finish-order '()))
    (an/graph:dfs-visit-graph g
                         :post-visit (lambda (g p node)
                                       (push node node-finish-order)))
    (an/vector-list node-finish-order)))

(defun an/graph:assign-components(g)
  "Find all the strongly connected components:
1. Perform DFS on the graph, compute the completion order of each
node.
2. Starting from first finished , Perform DFS assigning same
component numbers till each component is exhausted.
3. Returns the number of components found."
  (lexical-let* ((cur-component-number 0)
                 (dfs-post-order (an/graph:dfs-post-order g)))

    ;; Redo dfs this time going through reverse graph in node finish order
    (message "******Start Computing Component Number ********** ")
    (an/graph-reverse g)
    (an/graph:dfs-visit-graph g
     :traverse-order dfs-post-order
     :post-visit (lambda (g p nd)
                   (setf (an/graph:node-component nd) cur-component-number)
                   (message "assign-components : %d component: %d" (an/graph:node-number nd) (an/graph:node-component nd)))
     :post-dfs (lambda (g node)
                 (incf cur-component-number)))
     ;; return graph to original state.
    (an/graph-reverse g)
    cur-component-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjacency List Implementations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun edge-graph/make(size)
  "Constructs a graph represented as list of edge lists "
  (make-vector size nil))

(defun an/edge-graph:add-directed-edge (eg i j)
  (push j (aref (an/graph-adj-list eg) i)))

(defun an/edge-graph:add-undirected-edge (eg i j)
  (an/edge-graph:add-directed-edge eg i j)
  (an/edge-graph:add-directed-edge eg j i))

(defun an/edge-graph:parse-edges (relations)
  "Parse relation lists into a directed adjanceny list :
eg.  '([1 2] [2 3])."
  (let* ((max-vertex
          (loop for rel in relations
                for first-vertex = (aref rel 0)
                for second-vertex = (aref rel 1)
                maximize (max first-vertex second-vertex)))
         (num-vertex max-vertex)
         (graph      (edge-graph/make num-vertex)))
    (loop for rel in relations
          for first-vertex  = (-  (aref rel 0) 1)
          for second-vertex = (-  (aref rel 1) 1)
          do
          (an/edge-graph:add-undirected-edge graph first-vertex second-vertex))
    graph))

(defun edge-graph/neighbours (node edge-graph nodes)
  (loop  with node-number = (an/graph:node-number node)
         for neighbour in (aref edge-graph node-number)
         collect (aref nodes neighbour)))

(defun edge-graph/reverse (graph)
  (error "Not impelemnted"))

(provide 'an-lib)
