; system.lsp -- machine/system-dependent definitions
; 	Macintosh

(setf ny:bigendianp t)

;; note that *default-sf-format* is used below by 
;;    compute-default-sound-file
(if (not (boundp '*default-sf-format*))
    (setf *default-sf-format* snd-head-AIFF))

;; note that compute-default-sound-file uses *default-sf-format*,
;;    so be sure to set *default-sf-format* first (this was just done)
(if (not (boundp '*default-sound-file*))
    (compute-default-sound-file))

 (if (not (boundp '*default-sf-dir*))
    (setf *default-sf-dir* ""))

(if (not (boundp '*default-sf-mode*))
    (setf *default-sf-mode* snd-mode-pcm))

(if (not (boundp '*default-sf-bits*))
    (setf *default-sf-bits* 16))

(if (not (boundp '*default-plot-file*))
    (setf *default-plot-file* "points.dat"))

; turn off switch to play sound as it is computed
(setf *soundenable* T)

; local definition for play
(defmacro play (expr)
  `(s-save-autonorm ,expr NY:ALL *default-sound-file* :play *soundenable*))

(defun r ()
  (s-save (s-read *default-sound-file*) NY:ALL "" :play t)
)

; PLAY-FILE -- play a file
(defun play-file (name)
  (s-save (s-read name) NY:ALL "" :play t))

; FULL-NAME-P -- test if file name is a full path or relative path
;
; (otherwise the *default-sf-dir* will be prepended
;
(defun full-name-p (filename)
  (eq (char filename 0) #\:))

(setf *file-separator* #\:)

; save the standard function to write points to a file
;
;(setfn s-plot-points s-plot)

(defun array-max-abs (points)
  (let ((m 0.0))
    (dotimes (i (length points))
      (setf m (max m (abs (aref points i)))))
    m))

(setf graph-width 800)
(setf graph-height 220)


(defun s-plot (snd &optional (n 800))
  (show-graphics)
  (clear-graphics)
  (cond ((soundp snd)
           (s-plot-2 snd n (/ graph-height 2) graph-height nil))
          (t
           (let ((gh (/ graph-height (length snd)))
             hs)
         (dotimes (i (length snd))
           (setf hs (s-plot-2 (aref snd i) n (+ (/ gh 2) (* i gh)) gh hs)))))))
  

(defun s-plot-2 (snd n y-offset graph-height horizontal-scale)
  (prog ((points (snd-samples snd n))
           maxpoint horizontal-scale vertical-scale)
    (setf maxpoint (array-max-abs points))
    (moveto 0 y-offset)
    (lineto graph-width y-offset)
    (moveto 0 y-offset)
    (cond ((null horizontal-scale)
           (setf horizontal-scale (/ (float graph-width) (length points)))))
    (setf vertical-scale (- (/ (float graph-height) 2 maxpoint)))
    (dotimes (i (length points))
      (lineto (truncate (* horizontal-scale i))
          (+ y-offset (truncate (* vertical-scale (aref points i))))))
    (format t "X Axis: ~A to ~A (seconds)\n" (snd-t0 snd) (/ (length points) (snd-srate snd)))
    (format t "Y Axis: ~A to ~A\n" (- maxpoint) maxpoint)
    (format t "~A samples plotted.\n" (length points))
    (return horizontal-scale)
    ))




; S-EDIT - run the audio editor on a sound
;
;(defmacro s-edit (&optional expr)
;  `(prog ()
;		 (if ,expr (s-save ,expr 1000000000 *default-sound-file*))
;		 (system (format nil "audio_editor ~A &" 
;						 (soundfilename *default-sound-file*)))))

