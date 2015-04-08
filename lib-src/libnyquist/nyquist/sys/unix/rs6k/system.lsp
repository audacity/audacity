; machine.lsp -- machine/system-dependent definitions
; 	rs6000

(if (not (boundp '*default-sf-format*))
    (setf *default-sf-format* snd-head-NeXT))

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

; turn off switch to play sound as it is computed
(setf *soundenable* nil)

; local definition for play

(defmacro play (expr)
  `(let ()
     (s-save-autonorm ,expr NY:ALL *default-sound-file* :play *soundenable*)
     (r)))


(defun r ()
  (play-file *default-sound-file*))


; PLAY-FILE -- play a file
(defun play-file (name)
  (system (format nil "acpaplay ~A" (soundfilename name))))


; FULL-NAME-P -- test if file name is a full path or relative path
;
; (otherwise the *default-sf-dir* will be prepended
;
(defun full-name-p (filename)
  (or (eq (char filename 0) #\/)
      (eq (char filename 0) #\.)))

(setf *file-separator* #\/)


; save the standard function to write points to a file
;
(setfn s-plot-points s-plot)



; S-PLOT - plot a small number of points
;
(defun s-plot (&rest args)
  (let ((n (soundfilename *default-plot-file*)))
    (apply #'s-plot-points args)
    (cond ((boundp '*plotscript-file*))
          (t
           (format t "*plotscript-file* is unbound, setting it to: \n")
           (format t "    sys/unix/rs6k/plotscript\n")
           (format t "You may need to set it to a full path\n")
           (setf *plotscript-file* "sys/unix/rs6k/plotscript")))
    (system (format nil "xterm -t -e ~A ~A" *plotscript-file* n))))


; S-EDIT - run the audio editor on a sound
;
(defmacro s-edit (&optional expr)
  `(prog ()
         (if ,expr (s-save ,expr 1000000000 *default-sound-file*))
         (system (format nil "audio_editor ~A &" 
                         (soundfilename *default-sound-file*)))))

