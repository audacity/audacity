; machine.lsp -- machine/system-dependent definitions
; 	sparc

(setf *default-sound-srate* 8192)

(if (not (boundp '*default-sf-format*))
    (setf *default-sf-format* snd-head-NeXT))

(if (not (boundp '*default-sound-file*))
    (compute-default-sound-file))

(if (not (boundp '*default-sf-dir*))
    (setf *default-sf-dir* "/tmp"))

(if (not (boundp '*default-sf-mode*))
    (setf *default-sf-mode* snd-mode-pcm))

(if (not (boundp '*default-sf-bits*))
    (setf *default-sf-bits* 16))

; local definition for play

(defun r ()
  (system (format nil
                  "play ~A"
                  *default-sound-file*)))

(defmacro play (expr)
  `(prog (specs)
         (setf specs (s-save ,expr NY:ALL *default-sound-file*))
         (r)))


