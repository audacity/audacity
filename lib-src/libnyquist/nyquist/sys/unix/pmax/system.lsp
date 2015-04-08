; machine.lsp -- machine/system-dependent definitions
; 	rs6000

(if (not (boundp '*default-sf-format*))
    (setf *default-sf-format* snd-head-none))

(if (not (boundp '*default-sound-file*))
    (compute-default-sound-file))

(if (not (boundp '*default-sf-dir*))
    (setf *default-sf-dir* "/tmp/"))

(if (not (boundp '*default-sf-mode*))
    (setf *default-sf-mode* snd-mode-pcm))

(if (not (boundp '*default-sf-bits*))
    (setf *default-sf-bits* 16))

; local definition for play


(defun p6k ()
  (system (format nil
           "/usr/itc/projects/depot/tactus/bin/acpaplay ~A/~A < playparms"
           *default-sf-dir*
           *default-sound-file*)))


(defmacro play (expr)
  `(prog (specs playparms)
         (setf specs (s-save ',expr 1000000000 *default-sound-file*))
         (setf playparms (open "playparms" :direction :output))
         (format playparms "~A~%16~%2~%~A~%" (car specs) (cadr specs))
         (close playparms)
         (p6k)))


