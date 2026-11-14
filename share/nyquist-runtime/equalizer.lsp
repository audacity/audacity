;; equalizer.lsp -- support functions for equalizer editor in jNyqIDE

#| This is modeled after envelopes.lsp, which details how envelope data is 
exchanged between Nyquist and jNyqIDE.

The jNyqIDE code needs some work to make it look like the envelope
editor (which also needs work, but that's another matter). For consistency,
both should support named envelopes and equalizers.

However, for now, we have equalizers numbered from 0 to 9. The format for
exchange will be:

get-eq-data: begin
name parameters newline
name parameters newline
...
get-eq-data: end

and when the IDE wants to save a definition, it should call
(DEFINE-EQ 'NAME 'PARAMETER-LIST)

|#

(cond ((not (boundp '*equalizers*))
       (setf *equalizers* nil)))

;; DEFINE-EQ -- save the eq data and make corresponding function
;;
(defun define-eq (name expression)
  (setf *equalizers* (remove name *equalizers* 
                            :test #'(lambda (key item) (eql key (car item)))))
  (push (list name expression) *equalizers*)
  (make-eq-function name expression)
  ; make sure equalizers are redefined when workspace is loaded
  (add-to-workspace '*equalizers*)
  (describe '*equalizers* "data for equalizers in jNyqIDE")
  (add-action-to-workspace 'make-eq-functions)
  nil)


;; MAKE-EQ-FUNCTION -- convert data to a defined function
;;
(defun make-eq-function (name parameters)
  (cond ((numberp name)
             (setf name (intern (format nil "EQ-~A" name)))))
  (if (not (boundp '*grapheq-loaded*)) (load "grapheq.lsp"))
  (setf (symbol-function name)
        (eval `(lambda (s) (nband-range s ',parameters 60 14000)))))


;; MAKE-EQ-FUNCTIONS -- convert data to defined functions
;;
(defun make-eq-functions ()
  (let (name type parameters)
    (dolist (eq *equalizers*)
       (setf name (car eq))
       (setf parameters (second parameters))
       (make-eq-function name parameters))))


;; GET-EQ-DATA -- print env data for IDE
;;
(defun get-eq-data ()
  (let (parameters)
    (princ "get-eq-data: begin\n")
    (dolist (env *equalizers*)
      (format t "~A" (car env))
      (setf parameters (second env))
      (dotimes (i (length parameters))
        (format t " ~A" (aref parameters i)))
      (format t "~%"))
    (princ "get-eq-data: end\n")
    nil))


