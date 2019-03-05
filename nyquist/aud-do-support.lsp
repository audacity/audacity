;;; A collection of helper functions and macros to make scripting Audacity commands
;;; easier and more Lisp-like.
;;;
;;; Copyright 2018 - 2019 Audacity Team
;;; Steve Daulton
;;; Released under terms of the GNU General Public License version 2:
;;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html


(defun char-remove (ch str)
  ;;; Remove all occurances of character from string.
  (do ((out "")
       (i 0 (1+ i)))
      ((= i (length str)) out)
    (if (char/= (char str i) ch)
        (setf out (format nil "~a~a" out (char str i))))))


(defmacro string-append (str &rest strs)
  ;;; Append one or more strings to 'str'
  `(setf ,str (strcat ,str ,@strs)))


(defun aud-get-command (id)
  ;;; Return command signature from id string
  (let ((all (aud-get-info "Commands")))
    (dolist (cmd all)
      (when (member (string id) (assoc 'id cmd) :test 'equal)
        (return cmd)))))


(defun aud-import-command (command-sig &optional func-name)
  ;;; Generate a LISP function from Audacity command signature.
  ;;; If supplied, the generated function name will be 'func-name', otherwise
  ;;; it will be the command id, preceeded by 'aud-'.
  (let ((id (second (assoc 'id command-sig)))
        (params (second (assoc 'params command-sig)))
        (func-def "(defun aud-")
        (func-kwargs "(&key ")
        (func-body "")
        (aud-do-params ())
        (validate-func "")
        (aud-do-command ""))
    (if func-name
        (setf func-def (format nil "(defun ~a " func-name))
        (string-append func-def id " "))
    (dolist (p params)
      (let* ((key     (second (assoc 'key p)))
             (type    (second (assoc 'type p)))
             (default (second (assoc 'default p)))
             (enums   (second (assoc 'enum p)))
             ; The kwarg value must be a valid Lisp variable name (no spaces).
             (val     (char-remove #\Space key)))
        (string-append func-kwargs val " ")
        ;; Convert list of 'enums' to a string with quoted enums so we can string compare.
        (if enums
            (let ((str-enum "(list "))
              (dolist (e enums)
                (string-append str-enum "\"" (string e) "\" "))
              (setf enums (string-append str-enum ")")))
            (setf enums ""))
        ;; Add validators for each parameter to function body.
        (string-append func-body
"  (when " val "
    (unless (validate " val " \"" type "\" " enums ")(error \"bad argument type\" " val "))
    (push  (format nil \"\\\"" key "\\\"=~s \" " val ") params))\n")))

    ;; concatenate strings to build the complete function.
    (string-append func-def func-kwargs "&aux (params ()))\n"
"  ;; Push validated 'val's onto 'params' list
  (defun validate (val type &optional enums)
    (cond
      ((string-equal type \"bool\")
        (or (= val 0)(= val 1)))
      ((string-equal type \"string\")
        (stringp val))
      ((string-equal type \"enum\")
        (member val enums :test 'string=))
      ((string-equal type \"int\")
        (integerp val))
      ((string-equal type \"float\")
        (numberp val))
      ((string-equal type \"double\")
        (numberp val))))\n"
      func-body
"
  (setf command \"" id ": \")
    (dolist (p params)
      (setf command (strcat command p)))
    ;(print command)
    (aud-do command))")
    (eval-string func-def)))


(defun aud-import-commands ()
  ;;; Import all Audacity commands as LISP functions
  ;;; Function names prefix the command id with "aup-".
  (dolist (command (aud-get-info "Commands"))
      (aud-import-command command)))
