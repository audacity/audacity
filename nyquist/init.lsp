; init.lsp -- default Nyquist startup file

(setf *breakenable* t)
(load "nyinit.lsp" :verbose nil)

; add your customizations here:
;    e.g. (setf *default-sf-dir* "...")

; (load "test.lsp")



;; "_" (UNDERSCORE) - translation function
;;
;; Third party plug-ins are not translated by gettext in Audacity, but may include a
;; list of translations named *locale*. The format of *locale* must be:
;; (LIST (language-list) [(language-list) ...]) 
;; Each language-list is an a-list in the form:
;; ("cc" ((list "string" "translated-string") [(list "string" "translated-string") ...]))
;; where "cc" is the quoted country code.
;;
(setfn underscore _)
;;
(defun _(txt &aux newtxt)
  (when (boundp '*locale*)
    (when (not (listp *locale*))
          (error "bad argument type" *locale*))
    (let* ((cc (get '*audacity* 'language))
           (translations (second (assoc cc *locale* :test 'string-equal))))
      (if translations
          (let ((translation (second (assoc txt translations :test 'string=))))
            (if translation
                (if (stringp translation)
                    (setf newtxt translation)
                    (error "bad argument type" translation))
                (format t "No ~s translation of ~s.~%" cc txt)))
          (progn
            (setf *locale* '*unbound*)
            (format t "No ~s translations.~%" cc)))))
  (if newtxt newtxt (underscore txt)))


;;; Some helpers for parsing strings returned by (aud-do "GetInfo: ...

(defun eval-string (string)
  ;;; Evaluate a string as a LISP expression.
  ;;; If 'string' is not a valid LISP expression, the behaviour is undefined.
  (eval (read (make-string-input-stream string))))

(defmacro quote-string (string)
  ;;; Prepend a single quote to a string
  `(setf ,string (format nil "\'~a" ,string)))

(defun aud-get-info (str)
  ;;; Return "GetInfo: type=type" as Lisp list, or throw error
  ;;; Audacity 2.3.0 does not fail if type is not recognised, it 
  ;;; falls back to a default, so test for valid types.
  ;;; 'Commands+' is not supported in Audacity 2.3.0
  (let (type
        info
        (types '("Commands" "Menus" "Preferences"
                "Tracks" "Clips" "Envelopes" "Labels" "Boxes")))
    ;Case insensitive search, then set 'type' with correct case string, or  NIL.
    (setf type (first (member str types :test 'string-equal)))
    (if (not type)
        (error (format nil "bad argument '~a' in (aud-get-info ~a)" str str)))
    (setf info (aud-do (format nil "GetInfo: type=~a format=LISP" type)))
    (if (not (last info))
        (error (format nil "(aud-get-info ~a) failed.~%" str)))
    (let* ((info-string (first info))
           (sanitized ""))
      ;; Escape backslashes
      (dotimes (i (length info-string))
        (setf ch (subseq info-string i (1+ i)))
        (if (string= ch "\\")
            (string-append sanitized "\\\\")
            (string-append sanitized ch)))
      (eval-string (quote-string sanitized)))))


;;; *NYQ-PATH* is not required as path to Nyquist .lsp files
;;; is already defined (but not previously documented) as *runtime-path*
;;(setf *NYQ-PATH* (current-path))

;;; Load wrapper functions for aud-do commands.
;;; If commented out, "aud-do-support.lsp" may be loaded by a plug-in.
;;; Example: (lisp-loader (strcat *runtime-path* "aud-do-support.lsp"))
(load "aud-do-support.lsp" :verbose nil)
