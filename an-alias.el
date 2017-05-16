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
    (setf reg-end (point))
    ;; AN - 5/14
    ;;    (if (equal (point) reg-begin)
    ;;        (setf reg-end (line-end-position))
    ;;      (setf reg-end (point))
    )
  (forward-line n) ;; change buffer state so curser is n-lines ahead
  (let ((region (buffer-substring-no-properties reg-begin reg-end)))
    (split-string region "\n" t))))

(defun an/buffer:fetch-line()
  (car (an/buffer:fetch-lines 1)))

(defun an/buffer:num-lines()
  (count-lines (point-min) (point-max)))

(defun an/vector-list(ls)
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
          for current-line in (g/fetch-lines num-lines)
          for i = 0 then (+ i 1) do
          (funcall  f i current-line))))
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
;; Adjacency Matrix Helpers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun matrix-graph/make(size)
  "Simple two dimensional table representing adjacency matrix"
  (table/make size size nil))

(defun matrix-graph/reverse(graph size)
  "Reverse a adjacency matrix for a graph "
  (loop
   with new-graph = (table/make size size nil)
   finally (return new-graph)
   for row from 0 below (table/nrows  graph) do
   (loop for col below  (table/ncols graph) do
         (if (table/at graph row col)
             (table/setf new-graph col row t)))))

(provide 'an-alias)

