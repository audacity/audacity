; machine.lsp -- machine/system-dependent definitions
; 	SGI/IRIX

(if (not (boundp '*default-sf-format*))
    (setf *default-sf-format* snd-head-AIFF))

(if (not (boundp '*default-sound-file*))
    (compute-default-sound-file))

(if (not (boundp '*default-sf-dir*))
    (setf *default-sf-dir* "/tmp/"))

(if (not (boundp '*default-sf-mode*))
    (setf *default-sf-mode* snd-mode-pcm))

(if (not (boundp '*default-sf-bits*))
    (setf *default-sf-bits* 16))

(if (not (boundp '*default-plot-file*))
    (setf *default-plot-file* "points.dat"))

(defmacro play (expr)
  `(let ()
     (s-save-autonorm  ,expr NY:ALL *default-sound-file* :play *soundenable*)
     (r)))

(defun r ()
  (play-file *default-sound-file*))

; PLAY-FILE -- play a file
(defun play-file (name)
  (system (strcat "sfplay " (soundfilename name))))


; FULL-NAME-P -- test if file name is a full path or relative path
;
; (otherwise the *default-sf-dir* will be prepended
;
(defun full-name-p (filename)
  (or (eq (char filename 0) #\/)
      (eq (char filename 0) #\.)))

(setf *file-separator* #\/)

(sound-off) ; sgi implementation does not support real-time audio output yet

