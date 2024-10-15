;; system.lsp -- system-dependent lisp code

; local definition for play
;  this one is for Mac OS-X:

(if (not (boundp '*default-sf-format*))
    (setf *default-sf-format* snd-head-wave))

(if (not (boundp '*default-sound-file*))
    (compute-default-sound-file))

(if (not (boundp '*default-sf-dir*))
    (setf *default-sf-dir* "/tmp"))
    
(format t "*default-sf-dir* (default sound file directory) is ~A~%" 
        *default-sf-dir*)

(if (not (boundp '*default-sf-mode*))
    (setf *default-sf-mode* snd-mode-pcm))

(if (not (boundp '*default-sf-bits*))
    (setf *default-sf-bits* 16))

(if (not (boundp '*default-plot-file*))
    (setf *default-plot-file* (strcat (get-user) "-points.dat")))


; FULL-NAME-P -- test if file name is a full path or relative path
;
; (otherwise the *default-sf-dir* will be prepended
;
(defun full-name-p (filename)
  (or (eq (char filename 0) #\/)
      (eq (char filename 0) #\.)))

; RELATIVE-PATH-P -- test if filename or path is a relative path
;
(defun relative-path-p (filename)
  (not (eq (char filename 0) #\/)))

(setf *file-separator* #\/)


;; PLAY-FILE - play a sound file
;;
(defun play-file (name)
  (s-save (s-read name) NY:ALL "" :play t))


;; R - replay last file written with PLAY
(defun r () (play-file *default-sound-file*))

;;;; use this old version if you want to use sndplay to play
;;;; the result file rather than play the samples as they
;;;; are computed. This version does not autonormalize.
;; PLAY - write value of an expression to file and play it
;;
;(defmacro play (expr)
;  `(prog (specs)
;	 (setf specs (s-save (force-srate *sound-srate* ,expr) 
;			   1000000000 *default-sound-file*))
;	 (r)))
;;;;

; local definition for play
(defmacro play (expr)
  `(s-save-autonorm ,expr NY:ALL *default-sound-file* :play *soundenable*))

;; use standard s-plot
