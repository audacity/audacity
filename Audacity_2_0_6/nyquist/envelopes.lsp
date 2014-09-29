;; envelopes.lsp -- support functions for envelope editor in jNyqIDE

#| In Nyquist, editable envelopes are saved as one entry in the workspace
named *envelopes*. The entry is an association list where each element
looks like this:

(name type parameters... )

where name is a symbol, e.g. MY-ENVELOPE-1,
      type is a function name, e.g. PWL, PWLV, PWE, etc., and
      parameters are breakpoint data, e.g. 0.1 1 0.2 0.5 1

Example of two envelopes named FOO and BAR:

((FOO PWL 0.1 1 1) (BAR PWE 0.2 1 1))

To convert envelope data into functions, call (MAKE-ENV-FUNCTIONS).
This function should be on the workspace's list of functions to call.
(See ADD-ACTION-TO-WORKSPACE in Nyquist Manual.)

When the jNyqIDE wants to get the envelope data from the workspace, it
should call (GET-ENV-DATA), which will dump formatted data to Nyquist's
standard output as follows:

get-env-data: begin
name (type parameters...) newline
name (type parameters...) newline
...
get-env-data: end

When the IDE wants to save a definition, it should call
(DEFINE-ENV 'NAME 'EXPRESSION)

To delete a definition, call:
(DELETE-ENV 'NAME)

Envelope data will be loaded when the editor window is opened and saved
whenever the user issues a "save" command. If the user switches envelopes
without saving, there is a prompt to save or ignore.

The user will also be prompted to save when the editor window is closed
or when Nyquist is exited.

Saving the workspace automatically is something that Nyquist should do
(or prompt the user to do) when it exits.

|#

;; WORKSPACE -- the workspace is just a set of variables, typically
;;  with scores as values. These are stored in the file workspace.lsp
;;  so that you can work on some data and then store it for use later.

(cond ((not (boundp '*workspace*))
       (setf *workspace* nil)))
(cond ((not (boundp '*workspace-actions*))
       (setf *workspace-actions* nil)))
;; one of the variables in the workspace is *envelopes*
(cond ((not (boundp '*envelopes*))
       (setf *envelopes* nil)))

;; DESCRIBE -- add a description to a global variable
;;
(defun describe (symbol &optional description)
  (add-to-workspace symbol)
  (cond (description
         (putprop symbol description 'description))
        (t
         (get symbol 'description))))

;; ADD-TO-WORKSPACE -- add a global symbol to workspace
;;
(defun add-to-workspace (symbol)
  (cond ((not (symbolp symbol))
         (format t "add-to-workspace expects a (quoted) symbol~%"))
        ((not (member symbol *workspace*))
         (push symbol *workspace*))))


;; ADD-ACTION-TO-WORKSPACE -- call function when workspace is loaded
;;
(defun add-action-to-workspace (symbol)
  (cond ((not (symbolp symbol))
         (format t "add-action-to-workspace expects a (quoted) symbol~%"))
        ((not (member symbol *workspace-actions*))
         (push symbol *workspace-actions*))))

;; SAVE-WORKSPACE -- write data to file
;;
(defun save-workspace ()
  (let (val (outf (open "workspace.lsp" :direction :output)))
    (dolist (sym *workspace*)
      (format outf "(add-to-workspace '~A)~%" sym)
      (cond ((get sym 'description)
             (format outf "(putprop '~A \"~A\" 'description)~%"
                          sym (get sym 'description))))
      (format outf "(setf ~A '" sym)
      (setf val (symbol-value sym))
      (cond ((listp val)
             (format outf "(~%")
             (dolist (elem val)
               (format outf "  ~A~%" elem))
             (format outf " ))~%~%"))
            (t
             (format outf "~A)~%~%" val))))
    (dolist (sym *workspace-actions*) ;; call hooks after reading data
      (format outf "(add-action-to-workspace '~A)~%" sym)
      (format outf "(if (fboundp '~A) (~A))~%" sym sym))
    (format outf "(princ \"workspace loaded\\n\")~%")
    (close outf)
    (princ "workspace saved\n")
    nil))


;; DEFINE-ENV -- save the env data and make corresponding function
;;
(defun define-env (name expression)
  (delete-env name)
  (push (cons name expression) *envelopes*)
  (make-env-function name expression)
  ; make sure envelopes are redefined when workspace is loaded
  (add-to-workspace '*envelopes*) ; so *envelopes* will be saved
  (describe '*envelopes* "data for envelope editor in jNyqIDE")
  (add-action-to-workspace 'make-env-functions)
  nil)


;; DELETE-ENV -- delete an envelope definition from workspace
;;
;; note that this will not undefine the corresponding envelope function
;;
(defun delete-env (name)
  (setf *envelopes* 
        (remove name *envelopes* 
                :test #'(lambda (key item) (eql key (car item))))))


;; MAKE-ENV-FUNCTION -- convert data to a defined function
;;
(defun make-env-function (name expression)
  (setf (symbol-function name)
        (eval (list 'lambda '() expression))))


;; MAKE-ENV-FUNCTIONS -- convert data to defined functions
;;
(defun make-env-functions ()
  (let (name type parameters)
    (dolist (env *envelopes*)
       (setf name (car env))
       (setf type (cadr env))
       (setf parameters (cddr env))
       (make-env-function name (cons type parameters)))))


;; GET-ENV-DATA -- print env data for IDE
;;
(defun get-env-data ()
    (princ "get-env-data: begin\n")
    (dolist (env *envelopes*)
      (format t "~A ~A~%" (car env) (cdr env)))
    (princ "get-env-data: end\n")
    nil)

