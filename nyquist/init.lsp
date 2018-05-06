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
