;; update.lsp -- script to push changes into source directories

(load "makefile.lsp")    ; just to make sure we got the latest bindings


;; UPDATE-SOURCES -- makes a script to push changes into source directories
;;
(defun update-sources ()
  (let (outf)
    (load "transfiles.lsp") ; just to make sure we're current
    (setf outf (open "script" :direction :output))
    (format outf "#
# source this script file

# Source Paths: nyqsrc, cmtsrc, xlsrc, trnsrc
")
    (format outf "
#
# XLISP SOURCES
#
") 
    (file-update outf xlfiles ".c" "xlsrc")
    (file-update outf xlfiles-h ".h" "xlsrc")
    (file-update outf xlfiles-lsp ".lsp" "xlsrc")

    (format outf "
#
# NYQUIST SOURCES
#
")
    (file-update outf nyqfiles ".c" "nyqsrc")
    (file-update outf (exceptions-filter nyqfiles) ".h" "nyqsrc")
    (file-update outf nyqfiles-h ".h" "nyqsrc")
    (file-update outf nyqfiles-lsp ".lsp" "nyqsrc")
    (file-update outf makefiles "" "nyqsrc")

    (format outf "
#
# CMT SOURCES
#
")
    (file-update outf cmtfiles ".c" "cmtsrc")
    (file-update outf (exceptions-filter cmtfiles) ".h" "cmtsrc")
    (file-update outf cmtfiles-h ".h" "cmtsrc")

; don't write back machine generated trnsrc files
;    (file-update outf transfiles ".c" "trnsrc")
;    (file-update outf transfiles ".h" "trnsrc")

    (close outf)

    (format t "DONE writing script, 'source script' to copy files from~%")
    (format t "this directory to the source directories~%")

))
            

;; EXCEPTIONS-FILTER - remove .h files from list
; the depends-exceptions tells whether a .h file exists for a .c file
;; 
(defun exceptions-filter (files)
  (let (result)
    (dolist (f files)
      (let ((ex (assoc f depends-exceptions :test #'equal)))
        (cond (ex
               (if (and (cdr ex)
                          (string-search (strcat f ".h") (cadr ex)))
                   (push f result)))
              (t (push f result)))))
    result))


;; FILE-UPDATE -- write dependency for source files
;;
(defun file-update (outf files ext dir)
  (dolist (f files)
    (let ((fname (strcat f ext)))
      (format outf "cp -p ~A ~A/~A~%" fname dir fname))))

