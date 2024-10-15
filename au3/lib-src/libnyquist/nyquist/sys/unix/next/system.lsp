; local definition for play
;  this one is for NeXT:

(if (not (boundp '*default-sf-format*))
    (setf *default-sf-format* snd-head-NeXT))

(if (not (boundp '*default-sound-file*))
    (compute-default-sound-file))

(if (not (boundp '*default-sf-dir*))
    (setf *default-sf-dir* "./"))

(if (not (boundp '*default-sf-mode*))
    (setf *default-sf-mode* snd-head-mode-pcm))

(if (not (boundp '*default-sf-bits*))
    (setf *default-sf-bits* 16))

(if (not (boundp '*default-plot-file*))
    (setf *default-plot-file* "points.dat"))

;; PLAY-FILE - play a sound file
;;
(defun play-file (name)
  (system (strcat "sndplay " (soundfilename name))))

;; R - replay last file written with PLAY
(defun r () (play-file *default-sound-file*))

;; PLAY - write value of an expression to file and play it
;;
(defmacro play (expr)
  `(prog (specs)
         (setf specs (s-save (force-srate *sound-srate* ,expr) 
                             1000000000 *default-sound-file*))
         (r)))

