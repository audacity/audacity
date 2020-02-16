;;; A collection of helper functions and macros to make scripting Audacity commands
;;; easier and more Lisp-like.
;;;
;;; Copyright 2018 - 2020 Audacity Team
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
  ;;; Return command signature from id string or NIL.
  (let* ((helpstr (format nil "Help: Command=~s Format=LISP" id))
         (cmd-sig (aud-do helpstr)))
    (cond
      ((not (listp cmd-sig)) (error "Unknown error in aud-do" cmd-sig))
      ((string-equal (first cmd-sig) "Command not found") nil)
      (t (setf cmd-sig (first cmd-sig))
         (eval-string (quote-string cmd-sig))))))


(defun aud-import-command (cmd &optional func-name)
  ;;; Generate a LISP function from Audacity command ID or signature.
  ;;; If supplied, the generated function name will be 'func-name', otherwise
  ;;; it will be the command id, preceeded by 'aud-'.
  (when (stringp cmd)
    ;; cmd is the id, so get the command signature
    (let ((id cmd))
      (setf cmd (aud-get-command id))
      (if cmd
          (aud-import-command cmd func-name)
          (error "in aud-import-command, invalid argument" id))))
  (let ((id (second (assoc 'id cmd)))
        (params (second (assoc 'params cmd)))
        (func-def "(defun aud-")
        (func-kwargs "(&key ")
        (func-body ""))
    (if func-name
        (setf func-def (format nil "(defun ~a " func-name))
        (string-append func-def id " "))
    (dolist (p params)
      (let* ((key     (second (assoc 'key p)))
             (type    (second (assoc 'type p)))
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
    (aud-do command))")
    (eval-string func-def)))


(defun aud-generate-command-stubs (cmd-list)
  ;; Generate one stub for each function.
  ;; Stubs check that command is actually available before
  ;; generating the Lisp function.
  ;; This function is for internal use only.
  (dolist (cmd-id cmd-list)
    (let ((func-def (format nil
"(defun aud-~a (&rest args)
(if (string-equal (first (aud-do \"Help: Command=~a\")) \"Command not found\")
(error \"Command unavailable\" ~s))
(aud-import-command ~s)
(let ((arg-string \"\") (cmd-string \"(aud-~a \"))
(dolist (arg args)
(setf arg-string (format nil \"~a ~a\" arg-string arg)))
(setf cmd-string (format nil \"~a~a)\" cmd-string arg-string))
(eval-string cmd-string)))"
cmd-id cmd-id cmd-id cmd-id cmd-id "~a" "~s" "~a" "~a")))
      (eval-string func-def))))


;; Hard coded list because "GetInfo:" is slow and we can't yet exclude
;; Nyquist plug-ins (Nyquist plug-ins can't run from Nyquist Macros).
;; TODO: Create a fast scripting command to return this list instead of relying on hard coded.
(aud-generate-command-stubs
  (list "Amplify" "AutoDuck" "BassAndTreble" "ChangePitch" "ChangeSpeed"
        "ChangeTempo" "Chirp" "ClickRemoval" "Compressor" "DtmfTones"
        "Distortion" "Echo" "FadeIn" "FadeOut" "FilterCurve" "FindClipping"
        "GraphicEq" "Invert" "LoudnessNormalization" "Noise" "Normalize"
        "Paulstretch" "Phaser" "Repeat" "Repair" "Reverb" "Reverse"
        "Silence" "SlidingStretch" "Tone" "TruncateSilence" "Wahwah"
        ;; Scriptable Commands
        "CompareAudio" "Demo" "Export2" "GetInfo" "GetPreference" "Help"
        "Import2" "Message" "OpenProject2" "SaveProject2" "Screenshot"
        "SelectFrequencies" "SelectTime" "SelectTracks" "Select" "SetClip"
        "SetEnvelope" "SetLabel" "SetPreference" "SetProject" "SetTrackAudio"
        "SetTrackStatus" "SetTrackVisuals" "SetTrack"))
