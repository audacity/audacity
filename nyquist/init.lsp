; init.lsp -- default Nyquist startup file
(load "nyinit.lsp" :verbose nil)

; add your customizations here:
;    e.g. (setf *default-sf-dir* "...")

; (load "test.lsp")



;; "_" (UNDERSCORE) - translation function
;;
;; Third party plug-ins are not translated by gettext in Audacity, but may include a
;; list of translations named *local*. The format of *locale* must be:
;; (LIST (language-list) [(language-list) ...]) 
;; Each language-list is an a-list in the form:
;; ("cc" ((list "string" "translated-string") [(list "string" "translated-string") ...]))
;; where "cc" is the quoted country code.
;;
(setf underscore (function _))
;;
(defun _(txt)
  (setf translated nil)
  (when (boundp '*locale*)
    (if (not (listp *locale*))
        (format t "Warning: Invalid *locale* (not a list).~%")
        (let ((locale (get '*audacity* 'language)))
          (if (not (setf language-list (assoc locale *locale* :test 'string-equal)))
              (format t "Warning: No language-list for \"~a\" in *locale*.~%" locale)
              (if (/= (length language-list) 2)
                  (format t "Error: Invalid \"~a\" language list in *locale*.~%" locale)
                  ; Get just the list of substitution pairs
                  (let ((language-list (second language-list)))
                    (if (not (listp language-list))
                        (format t "Warning: No translations for \"~a\" in *locale*.~%" locale)
                        (let ((translation (assoc txt language-list :test 'string=)))
                          (if (not translation)
                              (format t "Warning: No ~a translations for \"~a\".~%" locale txt)
                              (if (not (and (listp translation)
                                            (= (length translation) 2)))
                                  (format t "Error: Invalid translation for ~a in *locale*.~%" txt)
                                  (setf translated (second translation))))))))))))
    (if translated
        translated
        (funcall underscore txt)))


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
    (let ((info-string (first info)))
      (eval-string (quote-string info-string)))))
