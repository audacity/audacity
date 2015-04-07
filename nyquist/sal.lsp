;;; **********************************************************************
;;; Copyright (C) 2006 Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision: 1.2 $
;;; $Date: 2009-03-05 17:42:25 $

;; DATA STRUCTURES AND ALGORITHMS (for sal.lsp and parse.lsp)
;;
;; TOKENIZE converts source language (a string) into a list of tokens
;;    each token is represented as follows:
;;    (:TOKEN <type> <string> <start> <info> <lisp>)
;;    where <type> is one of:
;;        :id -- an identifier
;;        :lp -- left paren
;;        :rp -- right paren
;;        :+, etc. -- operators
;;        :int -- an integer
;;        :float -- a float
;;        :print, etc. -- a reserved word
;;    <string> is the source string for the token
;;    <start> is the column of the string
;;    <info> and <lisp> are ??
;; Tokenize uses a list of reserved words extracted from terminals in
;;    the grammar. Each reserved word has an associated token type, but
;;    all other identifiers are simply of type :ID.
;;
;; *** WHY REWRITE THE ORIGINAL PARSER? ***
;; Originally, the code interpreted a grammar using a recursive pattern
;; matcher, but XLISP does not have a huge stack and there were
;; stack overflow problems because even relatively small expressions
;; went through a very deep nesting of productions. E.g. 
;; "print note(between(30,odds(.5, 60, 90)))" 0 t nil))" was at recursion
;; level 46 when the stack overflowed. The stack depth is 2000 or 4000,
;; but all locals and parameters get pushed here, so since PARSE is the
;; recursive function and it has lots of parameters and locals, it appears
;; to use 80 elements in the stack per call.
;; *** END ***
;;
;; The grammar for the recursive descent parser:
;;   note: [ <x> ] means optional <x>, <x>* means 0 or more of <x>
;;
;; <number> = <int> | <float>
;; <atom> = <int> | <float> | <id> | <bool>
;; <list> = { <elt>* }
;; <elt> = <atom> | <list> | <string>
;; <aref> = <id> <lb> <pargs> <rb>
;; <ifexpr> = ? "(" <sexpr> , <sexpr> [ , <sexpr> ] ")"
;; <funcall> = <id> <funargs>
;; <funargs> = "(" [ <args> ] ")"
;; <args> =  <arg> [ , <arg> ]*
;; <arg> = <sexpr> | <key> <sexpr>
;; <op> = + | - | "*" | / | % | ^ | = | != |
;;        "<" | ">" | "<=" | ">=" | ~= | ! | & | "|"
;; <mexpr> = <term> [ <op> <term> ]*
;; <term> = <-> <term> | <!> <term> | "(" <mexpr> ")" |
;;          <ifexpr> | <funcall> | <aref> | <atom> | <list> | <string>
;; <sexpr> = <mexpr> | <object> | class
;; <top> = <command> | <block> | <conditional> | <assignment> | <loop> | <exec>
;; <exec> = exec <sexpr>
;; <command> = <define-cmd> | <file-cmd> | <output>
;; <define-cmd> = define <declaration>
;; <declaration> = <vardecl> | <fundecl>
;; <vardecl> = variable <bindings>
;; <bindings> = <bind> [ , <bind> ]*
;; <bind> = <id> [ <=> <sexpr> ]
;; <fundecl> = <function> <id> "(" [ <parms> ] ")" <statement>
;; <parms> = <parm> [ , <parm> ]*
;;  this is new: key: expression for keyword parameter
;; <parm> = <id> | <key> [ <sexpr> ] 
;; <statement> = <block> | <conditional> | <assignment> |
;;               <output-stmt> <loop-stmt> <return-from> | <exec>
;; <block> = begin [ with <bindings> [ <statement> ]* end
;; <conditional> = if <sexpr> then [ <statement> ] [ else <statement> ] |
;;                 when <sexpr> <statement> | unless <sexpr> <statement>
;; <assignment> = set <assign> [ , <assign> ]*
;; <assign> = ( <aref> | <id> ) <assigner> <sexpr>
;; <assigner> = = | += | *= | &= | @= | ^= | "<=" | ">="
;; <file-cmd> = <load-cmd> | chdir <pathref> | 
;;              system <pathref> | play <sexpr>
;; (note: system was removed)
;; <load-cmd> = load <pathref> [ , <key> <sexpr> ]* 
;; <pathref> = <string> | <id>
;; <output-stmt> = print <sexpr> [ , <sexpr> ]* |
;;                 output <sexpr>
;; <loop-stmt> = loop [ with <bindings> ] [ <stepping> ]* 
;;               [ <termination> ]* [ <statement> ]+
;;               [ finally <statement> ] end
;; <stepping> = repeat <sexpr> |
;;              for <id> = <sexpr> [ then <sexpr> ] |
;;              for <id> in <sexpr> |
;;              for <id> over <sexpr> [ by <sexpr> ] |
;;              for <id> [ from <sexpr> ]
;;                       [ ( below | to | above | downto ) <sexpr> ]
;;                       [ by <sexpr> ] |
;; <termination> = while <sexpr> | until <sexpr>
;; <return-from> = return <sexpr>

;(in-package cm)

; (progn (cd "/Lisp/sal/") (load "parse.lisp") (load "sal.lisp"))

(setfn defconstant setf)
(setfn defparameter setf)
(setfn defmethod defun)
(setfn defvar setf)
(setfn values list)
(if (not (boundp '*sal-secondary-prompt*))
    (setf *sal-secondary-prompt* t))
(if (not (boundp '*sal-xlispbreak*))
    (setf *sal-xlispbreak* nil))

(defun sal-trace-enter (fn &optional argvals argnames)
  (push (list fn *sal-line* argvals argnames) *sal-call-stack*))

(defun sal-trace-exit ()
  (setf *sal-line* (second (car *sal-call-stack*)))
  (pop *sal-call-stack*))

;; SAL-RETURN-FROM is generated by Sal compiler and
;;  performs a return as well as a sal-trace-exit()
;;
(defmacro sal-return-from (fn val)
  `(prog ((sal:return-value ,val))
     (setf *sal-line* (second (car *sal-call-stack*)))
     (pop *sal-call-stack*)
     (return-from ,fn sal:return-value)))


(setf *sal-traceback* t)


(defun sal-traceback (&optional (file t) 
                      &aux comma name names line)
  (format file "Call traceback:~%")
  (setf line *sal-line*)
  (dolist (frame *sal-call-stack*)
    (setf comma "")
    (format file "    ~A" (car frame))
    (cond ((symbolp (car frame))
           (format file "(")
           (setf names (cadddr frame))
           (dolist (arg (caddr frame))
             (setf name (car names))
             (format file "~A~%        ~A = ~A" comma name arg)
             (setf names (cdr names))
             (setf comma ","))
           (format file ") at line ~A~%" line)
           (setf line (second frame)))
          (t 
           (format file "~%")))))


'(defmacro defgrammer (sym rules &rest args)
  `(defparameter ,sym
     (make-grammer :rules ',rules ,@args)))

'(defun make-grammer (&key rules literals)
  (let ((g (list 'a-grammer rules literals)))
    (grammer-initialize g)
    g))

'(defmethod grammer-initialize (obj)
  (let (xlist)
    ;; each literal is (:name "name")
    (cond ((grammer-literals obj)
           (dolist (x (grammer-literals obj))
             (cond ((consp x)
                    (push x xlist))
                   (t
                    (push (list (string->keyword (string-upcase (string x)))
                                (string-downcase (string x)))
                          xlist)))))
          (t
           (dolist (x (grammer-rules obj))
             (cond ((terminal-rule? x)
                    (push (list (car x)
                                (string-downcase (subseq (string (car x)) 1)))
                          xlist))))))
    (set-grammer-literals obj (reverse xlist))))

'(setfn grammer-rules cadr)
'(setfn grammer-literals caddr)
'(defun set-grammer-literals (obj val)
  (setf (car (cddr obj)) val))
'(defun is-grammer (obj) (and (consp obj) (eq (car obj) 'a-grammer)))

(defun string->keyword (str)
  (intern (strcat ":" (string-upcase str))))

(defun terminal-rule? (rule)
  (or (null (cdr rule)) (not (cadr rule))))

(load "sal-parse.lsp" :verbose nil)

(defparameter *sal-print-list* t)

(defun sal-printer (x &key (stream *standard-output*) (add-space t)
                           (in-list nil))
  (let ((*print-case* ':downcase))
    (cond ((and (consp x) *sal-print-list*)
	   (write-char #\{ stream)
	   (do ((items x (cdr items)))
               ((null items))
	      (sal-printer (car items) :stream stream
                                       :add-space (cdr items) :in-list t)
	      (cond ((cdr items)
                     (cond ((not (consp (cdr items)))
                            (princ "<list not well-formed> " stream)
                            (sal-printer (cdr items) :stream stream :add-space nil)
                            (setf items nil))))))
	   (write-char #\} stream))
	  ((not x)     (princ "#f" stream) )
	  ((eq x t)    (princ "#t" stream))
          (in-list     (prin1 x stream))
	  (t           (princ x stream)))
    (if add-space (write-char #\space stream))))

(defparameter *sal-printer* #'sal-printer)

(defun sal-message (string &rest args)
  (format t "~&; ")
  (apply #'format t string args))


;; sal-print has been modified from the original SAL to print items separated
;; by spaces (no final trailing space) and followed by a newline.
(defun sal-print (&rest args)
  (do ((items args (cdr items)))
       ((null items))
     ;; add space unless we are at the last element
     (funcall *sal-printer* (car items) :add-space (cdr items)))
  (terpri)
  (values))

(defmacro keyword (sym)
  `(str-to-keyword (symbol-name ',sym)))

(defun plus (&rest nums)
  (apply #'+ nums))

(defun minus (num &rest nums)
  (apply #'- num nums))

(defun times (&rest nums)
  (apply #'* nums))

(defun divide (num &rest nums)
  (apply #'/ num nums))

;; implementation of infix "!=" operator
(defun not-eql (x y)
  (not (eql x y)))

; dir "*.*
; chdir
; load "rts.sys"

(defun sal-chdir ( dir)
  (cd (expand-path-name dir))
  (sal-message "Directory: ~A" (pwd))
  (values))

;;; sigh, not all lisps support ~/ directory components.

(defun expand-path-name (path &optional absolute?)
  (let ((dir (pathname-directory path)))
    (flet ((curdir ()
	     (truename 
	      (make-pathname :directory
			     (pathname-directory
			      *default-pathname-defaults*)))))
      (cond ((null dir)
	     (if (equal path "~") 
		 (namestring (user-homedir-pathname))
		 (if absolute? 
		     (namestring (merge-pathnames path (curdir)))
		     (namestring path))))
	    ((eql (car dir) ':absolute)
	     (namestring path))
	    (t
	     (let* ((tok (second dir))
		    (len (length tok)))
	       (if (char= (char tok 0) #\~)
		   (let ((uhd (pathname-directory (user-homedir-pathname))))
		     (if (= len 1)
			 (namestring
			  (make-pathname :directory (append uhd (cddr dir))
					 :defaults path))
			 (namestring
			  (make-pathname :directory
					 (append (butlast uhd)
						 (list (subseq tok 1))
						 (cddr dir))
					 :defaults path))))
		   (if absolute?
		       (namestring (merge-pathnames  path (curdir)))
		       (namestring path)))))))))


(defun sal-load (filename &key (verbose t) print)
  (progv '(*sal-input-file-name*) (list filename)
    (prog (file extended-name)
      ;; first try to load exact name
      (cond ((setf file (open filename))
             (close file) ;; found it: close it and load it
             (return (generic-loader filename verbose print))))
      ;; try to load name with ".sal" or ".lsp"
      (cond ((string-search "." filename) ; already has extension
             nil) ; don't try to add another extension
            ((setf file (open (strcat filename ".sal")))
             (close file)
             (return (sal-loader (strcat filename ".sal")
                                 :verbose verbose :print print)))
            ((setf file (open (strcat filename ".lsp")))
             (close file)
             (return (lisp-loader filename :verbose verbose :print print))))
      ;; search for file as is or with ".lsp" on path
      (setf fullpath (find-in-xlisp-path filename))
      (cond ((and (not fullpath) ; search for file.sal on path
                  (not (string-search "." filename))) ; no extension yet
             (setf fullpath (find-in-xlisp-path (strcat filename ".sal")))))
      (cond ((null fullpath)
             (format t "sal-load: could not find ~A~%" filename))
            (t
             (return (generic-loader fullpath verbose print)))))))


;; GENERIC-LOADER -- load a sal or lsp file based on extension
;;
;; assumes that file exists, and if no .sal extension, type is Lisp
;;
(defun generic-loader (fullpath verbose print)
  (cond ((has-extension fullpath ".sal")
         (sal-loader fullpath :verbose verbose :print print))
        (t
         (lisp-loader fullpath :verbose verbose :print print))))

#|
(defun sal-load (filename &key (verbose t) print)
  (progv '(*sal-input-file-name*) (list filename)
    (let (file extended-name)
      (cond ((has-extension filename ".sal")
             (sal-loader filename :verbose verbose :print print))
            ((has-extension filename ".lsp")
             (lisp-load filename :verbose verbose :print print))
            ;; see if we can just open the exact filename and load it
            ((setf file (open filename))
             (close file)
             (lisp-load filename :verbose verbose :print print))
            ;; if not, then try loading file.sal and file.lsp
            ((setf file (open (setf *sal-input-file-name*
                                    (strcat filename ".sal"))))
             (close file)
             (sal-loader *sal-input-file-name* :verbose verbose :print print))
            ((setf file (open (setf *sal-input-file-name* 
                                    (strcat filename ".lsp"))))
             (close file)
             (lisp-load *sal-input-file-name* :verbose verbose :print print))
            (t
             (format t "sal-load: could not find ~A~%" filename))))))
|#

(defun lisp-loader (filename &key (verbose t) print)
  (if (load filename :verbose verbose :print print)
      nil ; be quiet if things work ok
      (format t "error loading lisp file ~A~%" filename)))


(defun has-extension (filename ext)
  (let ((loc (string-search ext filename
                            :start (max 0 (- (length filename)
                                             (length ext))))))
    (not (null loc)))) ; coerce to t or nil
    

(defmacro sal-at (s x) (list 'at x s))
(defmacro sal-at-abs (s x) (list 'at-abs x s))
(defmacro sal-stretch (s x) (list 'stretch x s))
(defmacro sal-stretch-abs (s x) (list 'stretch-abs x s))

;; splice every pair of lines
(defun strcat-pairs (lines)
  (let (rslt)
    (while lines
      (push (strcat (car lines) (cadr lines)) rslt)
      (setf lines (cddr lines)))
    (reverse rslt)))


(defun strcat-list (lines)
  ;; like (apply 'strcat lines), but does not use a lot of stack
  ;; When there are too many lines, XLISP will overflow the stack
  ;; because args go on the stack.
  (let (r)
    (while (> (setf len (length lines)) 1)
      (if (oddp len) (setf lines (cons "" lines)))
      (setf lines (strcat-pairs lines)))
    ; if an empty list, return "", else list has one string: return it
    (if (null lines) "" (car lines))))


(defun sal-loader (filename &key verbose print)
  (let ((input "") (file (open filename)) line lines)
    (cond (file
           (push filename *loadingfiles*)
           (while (setf line (read-line file))
            (push line lines)
            (push "\n" lines))
           (close file)
           (setf input (strcat-list (reverse lines)))
           (sal-trace-enter (strcat "Loading " filename))
           (sal-compile input t t filename)
           (pop *loadingfiles*)
           (sal-trace-exit))
          (t
           (format t "error loading SAL file ~A~%" filename)))))


; SYSTEM command is not implemented
;(defun sal-system (sys &rest pairs)
;  (apply #'use-system sys pairs))


(defun load-sal-file (file)
  (with-open-file (f file :direction :input)
    (let ((input (make-array '(512) :element-type 'character
			     :fill-pointer 0 :adjustable t)))
      (loop with flag
	 for char = (read-char f nil ':eof)
	 until (or flag (eql char ':eof))
	 do
	   (when (char= char #\;)
	     (loop do (setq char (read-char f nil :eof))
		until (or (eql char :eof)
			  (char= char #\newline))))
	   (unless (eql char ':eof)
	     (vector-push-extend char input)))
      (sal input :pattern :command-sequence))))


(defmacro sal-play (snd)
  (if (stringp snd) `(play-file ,snd)
                    `(play ,snd)))


(if (not (boundp '*sal-compiler-debug*))
    (setf *sal-compiler-debug* nil))


(defmacro sal-simrep (variable iterations body)
  `(simrep (,variable ,iterations) ,body))


(defmacro sal-seqrep (variable iterations body)
  `(seqrep (,variable ,iterations) ,body))


;; function called in sal programs to exit the sal read-compile-run-print loop
(defun sal-exit () (setf *sal-exit* t))

(setf *sal-call-stack* nil)

;; read-eval-print loop for sal commands
(defun sal ()
  (progv '(*breakenable* *tracenable* *sal-exit* *sal-mode*)
         (list *sal-break* nil nil t)
    (let (input line)
      (setf *sal-call-stack* nil)
      (read-line) ; read the newline after the one the user 
                  ; typed to invoke this fn
      (princ "Entering SAL mode ...\n");
      (while (not *sal-exit*)
        (princ "\nSAL> ")
        (sal-trace-enter "SAL top-level command interpreter")
        ;; get input terminated by two returns
        (setf input "")
        (while (> (length (setf line (read-line))) 0)
          (if *sal-secondary-prompt* (princ " ... "))
          (setf input (strcat input "\n" line)))
        ;; input may have an extra return, remaining from previous read
        ;; if so, trim it because it affects line count in error messages
        (if (and (> (length input) 0) (char= (char input 0) #\newline))
            (setf input (subseq input 1)))
        (sal-compile input t nil "<console>")
        (sal-trace-exit))
      (princ "Returning to Lisp ...\n")))
  ;; in case *xlisp-break* or *xlisp-traceback* was set from SAL, impose
  ;; them here
  (cond ((not *sal-mode*) 
         (setf *breakenable* *xlisp-break*)
         (setf *tracenable* *xlisp-traceback*)))
  t)



(defun sal-error-output (stack)
  (if *sal-traceback* (sal-traceback))
  (setf *sal-call-stack* stack)) ;; clear the stack


;; when true, top-level return statement is legal and compiled into MAIN
(setf *audacity-top-level-return-flag* nil)

;; SAL-COMPILE-AUDACITY -- special treatment of RETURN
;;
;; This works like SAL-COMPILE, but if there is a top-level
;; return statement (not normally legal), it is compiled into
;; a function named MAIN. This is a shorthand for Audacity plug-ins
;;
(defun sal-compile-audacity (input eval-flag multiple-statements filename)
  (progv '(*audacity-top-level-return-flag*) '(t)
    (sal-compile input eval-flag multiple-statements filename)))


;; SAL-COMPILE -- translate string or token list to lisp and eval
;;
;; input is either a string or a token list
;; eval-flag tells whether to evaluate the program or return the lisp
;; multiple-statements tells whether the input can contain multiple
;;   top-level units (e.g. from a file) or just one (from command line)
;; returns:
;;   if eval-flag, then nothing is returned
;;   otherwise, returns nil if an error is encountered
;;   otherwise, returns a list (PROGN p1 p2 p3 ...) where pn are lisp
;;      expressions
;;
;; Note: replaced local variables here with "local" names to avoid
;; collisions with globals that compiled code might try to use:
;; eval uses local bindings, not global ones
;;
(defun sal-compile (sal:input sal:evflag sal:mult-stmts sal:filename)
  ;; save some globals because eval could call back recursively
  (progv '(*sal-tokens* *sal-input* *sal-input-text*) '(nil nil nil)
    (let (sal:output sal:remainder sal:rslt sal:stack)
      (setf sal:stack *sal-call-stack*)
      ;; if first input char is "(", then eval as a lisp expression:
      ;(display "sal-compile" sal:input)(setf *sal-compiler-debug* t)
      (cond ((input-starts-with-open-paren sal:input)
             ;(print "sal:input is lisp expression")
             (errset
              (print (eval (read (make-string-input-stream sal:input)))) t))
            (t ;; compile SAL expression(s):
             (loop
                (setf sal:output (sal-parse nil nil sal:input sal:mult-stmts 
                                        sal:filename))
                (cond ((first sal:output) ; successful parse
                       (setf sal:remainder *sal-tokens*)
                       (setf sal:output (second sal:output))
                       (when *sal-compiler-debug*
                         (terpri)
                         (pprint sal:output))
                       (cond (sal:evflag ;; evaluate the compiled code
                              (cond ((null (errset (eval sal:output) t))
                                     (sal-error-output sal:stack)
                                     (return)))) ;; stop on error
                             (t
                              (push sal:output sal:rslt)))
                                        ;(display "sal-compile after eval" 
                                        ;         sal:remainder *sal-tokens*)
                       ;; if there are statements left over, maybe compile again
                       (cond ((and sal:mult-stmts sal:remainder)
                              ;; move sal:remainder to sal:input and iterate
                              (setf sal:input sal:remainder))
                             ;; see if we've compiled everything
                             ((and (not sal:evflag) (not sal:remainder))
                              (return (cons 'progn (reverse sal:rslt))))
                             ;; if eval but no more sal:input, return
                             ((not sal:remainder)
                              (return))))
                      (t ; error encountered
                       (return)))))))))

;; SAL just evaluates lisp expression if it starts with open-paren,
;; but sometimes reader reads previous newline(s), so here we
;; trim off initial newlines and check if first non-newline is open-paren
(defun input-starts-with-open-paren (input)
  (let ((i 0))
    (while (and (stringp input)
                (> (length input) i)
                (eq (char input i) #\newline))
      (incf i))
    (and (stringp input)
         (> (length input) i)
         (eq (char input i) #\())))

(defun sal-equal (a b)
  (or (and (numberp a) (numberp b) (= a b)) 
      (equal a b)))

(defun not-sal-equal (a b)
  (not (sal-equal a b)))
