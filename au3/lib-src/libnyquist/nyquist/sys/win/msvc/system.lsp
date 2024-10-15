; machine.lsp -- machine/system-dependent definitions
;       Windows

;; default behavior is to call SETUP-CONSOLE to get large white typescript
;;
;; set *setup-console* to nil in your personal init.lsp to override this behavior 
;; (this may be necessary to work with emacs)
;;
(if (not (boundp '*setup-console*)) (setf *setup-console* t))
(if *setup-console* (setup-console))

(if (not (boundp '*default-sf-format*))
    (setf *default-sf-format* snd-head-Wave))

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

;(if (not (boundp '*plotscript-file*))
;    (setf *plotscript-file* "sys/unix/rs6k/plotscript"))

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
  (or (eq (char filename 0) #\\)
      (eq (char filename 0) #\/)
      (eq (char filename 0) #\.)
      (and (> (length filename) 2)
           (both-case-p (char filename 0))
           (equal (char filename 1) #\:))))

; RELATIVE-PATH-P -- test if filename or path is a relative path
;
; note that properly converting a Windows path from relative to
;  absolute is complicated by paths like: E:MYFILE.LSP
;  Nyquist assumes that if there is a drive letter, the path is
;  absolute, e.g. E:\TMP\MYFILE.LSP and if there is no drive,
;  the path is relative, e.g. you cannot have \TMP\MYFILE.LSP
;
(defun relative-path-p (filename)
  (or (< (length filename) 2)
      (not (both-case-p (char filename 0)))
      (not (equal (char filename 1) #\:))))


(setf *file-separator* #\\)

(defun ny:load-file () (load "*.*"))
(defun ny:reload-file () (load "*"))


; save the standard function to write points to a file
;
;(setfn s-plot-points s-plot)

;(defun array-max-abs (points)
;  (let ((m 0.0))
;        (dotimes (i (length points))
;          (setf m (max m (abs (aref points i)))))
;        m))

;(setf graph-width 600)
;(setf graph-height 220)

;(defun s-plot (snd &optional (n 600))
;  (show-graphics)
;  (clear-graphics)
;  (cond ((soundp snd)
;               (s-plot-2 snd n (/ graph-height 2) graph-height))
;              (t
;               (let ((gh (/ graph-height (length snd)))
;                     hs)
;                 (dotimes (i (length snd))
;                   (setf hs (s-plot-2 (aref snd i) n (+ (/ gh 2) (* i gh)) gh hs)))))))
;
;
;(defun s-plot-2 (snd n y-offset graph-height horizontal-scale)
;  (prog ((points (snd-samples snd n))
;                   maxpoint horizontal-scale vertical-scale)
;    (setf maxpoint (array-max-abs points))
;    (moveto 0 y-offset)
;    (lineto graph-width y-offset)
;    (moveto 0 y-offset)
;    (cond ((null horizontal-scale)
;               (setf horizontal-scale (/ (float graph-width) (length points)))))
;    (setf vertical-scale (- (/ (float graph-height) 2 maxpoint)))
;    (dotimes (i (length points))
;      (lineto (truncate (* horizontal-scale i))
;              (+ y-offset (truncate (* vertical-scale (aref points i))))))
;    (format t "X Axis: ~A to ~A (seconds)\n" (snd-t0 snd) (/ (length points) (snd-srate snd)))
;    (format t "Y Axis: ~A to ~A\n" (- maxpoint) maxpoint)
;    (format t "~A samples plotted.\n" (length points))
;    (return horizontal-scale)
;    ))
;
; S-EDIT - run the audio editor on a sound
;
;(defmacro s-edit (&optional expr)
;  `(prog ()
;         (if ,expr (s-save ,expr 1000000000 *default-sound-file*))
;         (system (format nil "audio_editor ~A &" 
;                         (soundfilename *default-sound-file*)))))

