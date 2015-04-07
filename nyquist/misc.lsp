;## misc.lsp -- a collection of useful support functions

;; Garbage collection "improvement" -- XLISP will GC without allocation
;; as long as it does not run out of cells. This can make it very slow
;; since GC does work proportional to the heap size. If there were
;; always at least, say, 1/3 of the heap free after GC, then allocating
;; cells would be more-or-less a constant time operation (amortized).
;;
;; So, after GC, we'll expand until we have 1/3 of the heap free.
;;
(defun ny:gc-hook (heap-size free-cells)
  (cond ((< (* free-cells 2) heap-size) ;; free cells is < 1/3 heap
         ;; expand. Each expansion unit is 2000 cons cells
         (let* ((how-many-not-free (- heap-size free-cells))
                (should-be-free (/ how-many-not-free 2))
                (how-many-more (- should-be-free free-cells))
                (expand-amount (/ how-many-more 2000)))
           (cond ((> expand-amount 0)
                  (if *gc-flag*
                      (format t
                       "[ny:gc-hook allocating ~A more cells] "
                       (* expand-amount 2000)))
                  (expand expand-amount)))))))

(setf *gc-hook* 'ny:gc-hook)


; set global if not already set
;
(defmacro init-global (symb expr)
  `(if (boundp ',symb) ,symb (setf ,symb ,expr)))

; controlling breaks and tracebacks:
; XLISP and SAL behave differently, so there are four(!) flags:
; *sal-traceback* -- print SAL traceback on error in SAL mode
;                    Typically you want this on always.
; *sal-break* -- allow break (to XLISP prompt) on error when in SAL mode
;                (overrides *sal-traceback*) Typically, you do not want
;                this unless you need to see exactly where an error happened
;                or the bug is in XLISP source code called from SAL.
; *xlisp-break* -- allow break on error when in XLISP mode
;                  Typically, you want this on.
; *xlisp-traceback* -- print XLISP traceback on error in XLISP mode
;                      Typically, you do not want this because the full
;                      stack can be long and tedious.

(setf *sal-mode* nil)

(setf *sal-traceback* t
      *sal-break* nil
      *xlisp-break* t
      *xlisp-traceback* nil)

(defun sal-tracenable (flag) (setf *sal-traceback* flag))
(defun sal-breakenable (flag)
  (setf *sal-break* flag)
  (if *sal-mode* (setf *breakenable* flag)))
(defun xlisp-breakenable (flag)
  (setf *xlisp-break* flag)
  (if (not *sal-mode*) (setf *breakenable* flag)))
(defun xlisp-tracenable (flag)
  (setf *xlisp-traceback* flag)
  (if flag (setf *xlisp-break* t))
  (cond ((not *sal-mode*)
         (if flag (setf *breakenable* t))
         (setf *tracenable* flag))))


; enable or disable breaks
(defun bkon () (xlisp-breakenable t))
(defun bkoff () (xlisp-breakenable nil))


;; (grindef 'name) - pretty print a function
;;
(defun grindef (e) (pprint (get-lambda-expression (symbol-function e))))

;; (args 'name) - print function and its formal arguments
;;
(defun args (e) 
  (pprint (cons e (second (get-lambda-expression (symbol-function e))))))

;; (incf <place>), (decf <place>) - add/sub 1 to/from variable
;;
(defmacro incf (symbol) `(setf ,symbol (1+ ,symbol)))
(defmacro decf (symbol) `(setf ,symbol (1- ,symbol)))


;; (push val <place>) - cons val to list
;;
(defmacro push (val lis) `(setf ,lis (cons ,val ,lis)))
(defmacro pop (lis) `(prog1 (car ,lis) (setf ,lis (cdr ,lis))))

;; include this to use RBD's XLISP profiling hooks
;;(load "/afs/andrew/usr/rbd/lib/xlisp/profile.lsp")

;(cond ((boundp 'application-file-name)
;       (load application-file-name)))


(defun get-input-file-name ()
  (let (fname)
    (format t "Input file name: ")
    (setf fname (read-line))
    (cond ((equal fname "") (get-input-file-name))
          (t fname))))


(defun open-output-file ()
  (let (fname)
    (format t "Output file name: ")
    (setf fname (read-line))
    (cond ((equal fname "") t)
          (t (open fname :direction :output)))))


(defmacro while (cond &rest stmts)
  `(prog () loop (if ,cond () (return)) ,@stmts (go loop)))


; when parens/quotes don't match, try this
; 
(defun file-sexprs ()
  (let ((fin (open (get-input-file-name)))
        inp)
    (while (setf inp (read fin)) (print inp))))

;; get path for currently loading file (if any)
;;
(defun current-path ()
  (let (fullpath n)
    (setf n -1)
    (cond (*loadingfiles*
           (setf fullpath (car *loadingfiles*))
           (dotimes (i (length fullpath))
             ;; search for "/" (and on windows, also "\") in path:
             (cond ((or (equal (char fullpath i) *file-separator*)
                        (equal (char fullpath i) #\/))
                    (setf n i))))
           ;; trim off filename (after last separator char in path
           (setf fullpath (subseq fullpath 0 (1+ n)))

;;         REMOVED SUPPORT FOR MAC OS-9 AND BELOW -RBD
           ;; if this is a Mac, use ':' in place of empty path
;;           (cond ((and (equal fullpath "") 
;;                       (equal *file-separator* #\:))
;;                  (setf fullpath ":")))
;;         END MAC OS-9 CODE

           ;; Here's an interesting problem: fullpath is now the path
           ;; specified to LOAD, but it may be relative to the current
           ;; directory. What if we want to load a sound file from the
           ;; current directory? It seems that S-READ gives priority to
           ;; the *DEFAULT-SF-DIR*, so it will follow fullpath STARTING
           ;; FROM *DEFAULT-SF-DIR*. To fix this, we need to make sure
           ;; that fullpath is either an absolute path or starts with
           ;; and explicit ./ which tells s-read to look in the current
           ;; directory.
           (cond ((> (length fullpath) 0)
		  (cond ((full-name-p fullpath))
			(t ; not absolute, make it explicitly relative
			 (setf fullpath (strcat "./" fullpath)))))
                 (t (setf fullpath "./"))) ; use current directory
           fullpath)
          (t nil))))
          
;; real-random -- pick a random real from a range
;;
(defun real-random (from to)
  (+ (* (rrandom) (- to from)) from))

;; power -- raise a number to some power x^y
;;
(defun power (x y)
  (exp (* (log (float x)) y)))
  
;; require-from -- load a file if a function is undefined
;;
;; fn-symbol -- the function defined when the file is loaded
;; file-name -- the name of file to load if fn-symbol is undefined
;; path -- if t, load from current-path; if a string, prepend string
;;         to file-name; if nil, ignore it
;;
(defmacro require-from (fn-symbol file-name &optional path)
  (cond ((eq path t)
         (setf file-name `(strcat (current-path) ,file-name)))
        (path
         (setf file-name `(strcat ,path ,file-name))))
  ; (display "require-from" file-name)
  `(if (fboundp (quote ,fn-symbol))
       t
       ;; search for either .lsp or .sal file
       (sal-load ,file-name)))

