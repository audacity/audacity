;;; A collection of helper functions and macros to make scripting Audacity commands
;;; easier and more Lisp-like.
;;;
;;; Copyright 2018 - 2020 Audacity Team
;;; Steve Daulton
;;; Released under terms of the GNU General Public License version 2:
;;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html


(defun char-remove (ch str)
  ;;; Remove all occurrences of character from string.
  (do ((out "")
       (i 0 (1+ i)))
      ((= i (length str)) out)
    (if (char/= (char str i) ch)
        (setf out (format nil "~a~a" out (char str i))))))

(defun number-string-p (str)
  ;;; like digit-char-p for strings
  (unless (stringp str)
    (return-from number-string-p nil))
  (let ((num (string-to-number str)))
    (if (numberp num)
        num
        nil)))

(defmacro string-append (str &rest strs)
  ;;; Append one or more strings to 'str'
  `(setf ,str (strcat ,str ,@strs)))

(defun aud-print-command (cmd)
  ;;; Print a quick reference for command arguments.
  (let ((help-data (first (aud-do-command "Help" :command cmd :format "LISP")))
        (out (format nil "(aud-do-command ~s [:key val ...])~%" (string-downcase cmd))))
    (cond
      ((string-equal help-data "Command not found")
          ;Debug out can be copied on all platforms.
          (format t "~a~a." out help-data)
          (format nil "~a~a." out help-data))
      (t  (setf help-data (eval-string (quote-string help-data)))
          (let ((params (second (assoc 'params help-data))))
            (dolist (p params)
              (setf out (format nil "~a  :~a (~a) default: ~s~%"
                                out
                                (string-downcase (second (assoc 'key p)))
                                (second (assoc 'type p))
                                (second (assoc 'default p))))
              (let ((enums (assoc 'enum p)))
                (when enums
                  (setf out (format nil "~a    [" out))
                  (dolist (e (second enums))
                    (setf out (format nil "~a~s " out e)))
                  (setf out (format nil "~a]~%" (string-right-trim " " out)))))))
          (format t "~a" out)
          out))))


(defun aud-do-command (id &rest params)
  ;; Translate aud-do-command, to (aud-do "command").
  ;; To avoid unnecessary overhead, only validate when debugging enabled
  ;; 'aud-import-commands' passes params as a list, so we need to unpack it.
  (when (and (= (length params) 1)
             (listp (first params)))
    (setf params (first params)))
  (when *tracenable*
    (aud-check-debug-cache)
    (let (val-allowed type enums pstr
          (id-valid (aud-verify-command-id id))
          (valid-params (aud-get-command-params id))
          (keystr ""))
      (if (not id-valid)
          ; The command may still be valid as 
          ; "GetInfo: Type=Commands" does not return all valid AUD-DO commands.
          (format t "Debug data unavailable: ~s.~%" id)
          ;; Command ID recognised, so check params.
          (dolist (p params)
            (setf pstr (format nil "~a" p))
            (cond
              ((char= (char pstr 0) #\:) ;keyword
                (setf keystr (subseq pstr 1))
                (let ((kf (dolist (vp valid-params nil)
                            (when (string-equal (second (assoc 'key vp)) keystr)
                              (return vp)))))
                  (cond
                    (kf ;keyword found
                      (setf type (second (assoc 'type kf)))
                      (setf enums (second (assoc 'enum kf)))
                      (cond
                        ((member type '("int" "float" "double") :test 'string-equal)
                          (setf val-allowed "number"))
                        ((string-equal type "enum")
                          (setf val-allowed enums)) ;a list
                        (t (setf val-allowed type)))) ;"string" "bool" or NIL
                    ;; Invalid keyword, so give some helpful hints:
                    (t (format t "Invalid key in ~s :~a~%" id keystr)
                       ;; pretty print valid keywords
                       (format t "Valid keys for ~a are:~%" id)
                       (dolist (vp valid-params)
                         (dolist (item vp)
                           (let ((itype (first item)))
                             (case itype
                              ('KEY (format t "   ~a " (second item)))
                              ('TYPE (when (string-not-equal (second item) "enum")
                                       (format t "(~a) " (second item))))
                              ('ENUM (format t "[~a]"
                                        (string-trim "()"
                                            (format nil "~a" (second item))))))))
                         (format t "~%"))))))
              (t  ;key value
                (cond
                  ((not val-allowed)
                      (format t "Too many arguments: ~s :~a~%" id keystr))
                  ((listp val-allowed)
                      (unless (member pstr enums :test 'string=) ;case sensitive
                        (format t "Invalid enum in ~s :~a - ~s~%" id keystr p)
                        (format t "Options are:~%  ~a~%" enums)))
                  ((string= val-allowed "bool")
                      (unless (or (string= pstr "0") (string= pstr "1"))
                        (format t "~s :~a value must be 0 or 1~%" id keystr)))
                  ((string= val-allowed "number")
                      (unless (or (numberp p) (number-string-p p))
                        (format t "~s :~a value must be a number: ~s~%" id keystr p)))
                  ((string= val-allowed "string")
                      (unless (stringp p)
                        (format t "~s :~a value must be a string: ~a~%" id keystr p))))
                (psetq  val-allowed nil
                        type  nil
                        enums nil)))))))
  ;; Send the command
  (let ((cmd (format nil "~a:" id)))
    (dolist (p params)
      (setf p (format nil "~a" p))
      (string-append cmd
          (cond
            ((char= (char p 0) #\:) ;keyword
              (format nil " ~a=" (subseq p 1)))
            (t  ;key value
              (format nil "~s" p)))))
    (aud-do cmd)))


(defun aud-import-commands (&aux cmd)
  ;; Generate function stubs in the form (aud-<command> [&key arg ...])
  ;; Call once to make "aud-<command>"s available.
  ;; We don't call this on load, as we don't want to delay loading Nyquist unnecessarily.
  (aud-check-debug-cache)
  (dolist (cmd (aud-get-command))
    (setf cmd (second (assoc 'id cmd)))
    (let ((symb (intern (string-upcase (format nil "aud-~a" cmd)))))
      (eval `(defun ,symb (&rest args)
              (aud-do-command ,cmd args))))))


(defun aud-check-debug-cache ()
  ;;; Load aud-do-debug-data-cache, updating if necessary.
  (let ((fqname (format nil "~a~a~a"
                       (string-right-trim (string *file-separator*) (get-temp-path))
                       *file-separator*
                       "aud-do-debug-data-cache.lsp")))
    (cond ;Update if necessary
      ((fboundp 'aud-do-version)  ;cache is loaded
        ;; Refresh cache if versions don't match.
        ;; 'aud-do-version' tests the interned version.
        ;; 'autoload-helper' tests the disk version and prevents repeating cache refresh in the initial session.
        (unless (or (string= (format nil "~a" (aud-do-version))
                             (format nil "~a" (get '*audacity* 'version)))
                    (string= (format nil "~a" (autoload-helper fqname 'aud-do-version nil))
                             (format nil "~a" (get '*audacity* 'version))))
          (aud-refresh-debug-data-cache fqname)))
      ;cache not loaded, so try loading and refresh if we can't.
      ((not (load fqname :verbose t))
        (aud-refresh-debug-data-cache fqname)))))


(defun aud-refresh-debug-data-cache (fqname)
  ;; Cache the list of command profiles as function "aud-get-command", and load it.
  (labels ((disable-plugins (typestring &aux oldval)
            ;; Disable plug-ins of type 'typestring' and return it's previous value.
            (let ((getcmd (format nil "GetPreference: Name=\"~a/Enable\"" typestring)))
              (setf oldval (first (aud-do getcmd)))
              (do-set-val typestring oldval 0) ;Disable all plug-ins
              oldval))  ;may be 0, 1 or ""
          (do-set-val (typestring oldval newval)
            ;; If plug-in type was previously enabled ('oldval = true, "1" or empty), set it to 'newval'.
            (let ((setcmd (format nil "SetPreference: Name=\"/~a/Enable\" Value=" typestring)))
              (when (and oldval (or (string= oldval "")(string= oldval "1")))
                (aud-do (format nil "~a~s" setcmd (if (= newval 0) 0 oldval))))))
          (get-usable-commands ()
            ;; Disable plug-ins, get list of remaining commands, then re-enable plug-ins if previously enabled.
            ;; Return list of commands.
            (let ((cmds '(("Nyquist" ny)("LADSPA" la)("LV2" lv)("VST" vs)("AudioUnit" au)("Vamp" va)))
                  info)
              (dolist (cmd cmds)
                (setf (nth 1 cmd) (disable-plugins (nth 0 cmd))))
              (setf info (first (aud-do "getinfo: type=Commands format=LISP"))) ;Get scriptables and built-in effects
              (dolist (cmd cmds)
                (do-set-val (nth 0 cmd) (nth 1 cmd) 1))  ;Re-enable plug-ins
              info)))
      (let ((fp (open fqname :direction :output)))
        ;; Write cache file, or return error.
        (cond
          (fp (format fp
";; Intended for internal use by aud-do-command.~%
(defun aud-do-version ()
  '~a)~%
(defun aud-verify-command-id (id)
  (second (assoc 'id (aud-get-command id))))~%
(defun aud-get-command-params (id)
  (second (assoc 'params (aud-get-command id))))~%
(defun aud-get-command (&optional id &aux cmds)
  ;; If id supplied, return command profile or nil.
  ;; Else, return full list.
  (setf cmds
  '~a)
  ;; Return all commands, or one command or nil.
  (if id
      (dolist (cmd cmds nil)
        (when (string-equal (string id) (second (assoc 'id cmd)))
          (return cmd)))
      cmds))"
                      (get '*audacity* 'version)
                      (get-usable-commands))
              (format t "Debug data cache refreshed.~%")
              (close fp)
              (unless (load fqname :verbose t) ;load the file
                (error "Unable to load" fqname))) ;assert
          (t  (format t "Error: ~a cannot be written." fqname))))))


;; Try to load AUD- command cache.
(when (get-temp-path)
  (let ((fqname (format nil "~a~a~a"
                        (string-right-trim (string *file-separator*) (get-temp-path))
                        *file-separator*
                        "aud-do-debug-data-cache.lsp")))
    (load fqname :verbose t)))
