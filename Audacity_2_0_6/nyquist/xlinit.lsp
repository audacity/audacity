;; xlinit.lsp -- standard definitions and setup code for XLisp
;;


(defun bt () (baktrace 6))

(defmacro setfn (a b) 
  `(setf (symbol-function ',a) (symbol-function ',b)))

(setfn co continue)
(setfn top top-level)
(setfn res clean-up)
(setfn up clean-up)

;## display -- debugging print macro
;
; call like this (display "heading" var1 var2 ...)
; and get printout like this:
;   "heading : VAR1 = <value> VAR2 = <value> ...<CR>"
;
; returns:
;   (let ()
;     (format t "~A: " ,label)
;     (format t "~A = ~A  " ',item1 ,item1)
;     (format t "~A = ~A  " ',item2 ,item2)
;     ...)
;
(defmacro display-macro (label &rest items)
  (let ($res$)
    (dolist ($item$ items)
            (setq $res$ (cons
                         `(format t "~A = ~A  " ',$item$ ,$item$)
                         $res$)))
    (append (list 'let nil `(format t "~A : " ,label))
            (reverse $res$)
            '((terpri)))))


(defun display-on () (setfn display display-macro) t)
(defun display-off () (setfn display or) nil)
(display-on)

; (objectp expr) - object predicate
;
;this is built-in: (defun objectp (x) (eq (type-of x) 'OBJ))


; (filep expr) - file predicate
;
(defun filep (x) (eq (type-of x) 'FPTR))

(load "profile.lsp" :verbose NIL)

(setq *breakenable* t)
(setq *tracenable* nil)

(defmacro defclass (name super locals class-vars)
  (if (not (boundp name))
    (if super
    `(setq ,name (send class :new ',locals ',class-vars ,super))
    `(setq ,name (send class :new ',locals ',class-vars)))))

;(cond ((boundp 'application-file-name)
;       (load application-file-name)))

(setq *gc-flag* t)

