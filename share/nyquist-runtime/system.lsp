;; system.lsp -- system-dependent lisp code

; local definition for play
;  this one is for Linux:

(if (not (boundp '*default-sf-format*))
    (setf *default-sf-format* snd-head-wave))

(if (not (boundp '*default-sound-file*))
    (compute-default-sound-file))

(if (not (boundp '*default-sf-dir*))
    (setf *default-sf-dir* "./"))

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
;;
;; WARNING: if you invoke an external program to play files, 
;; but Nyquist uses internal (portaudio) interface to
;; play synthesized sound, Nyquist may fail to open the
;; sound device while it is playing a sound file and then
;; refuse to play anything. -RBD dec05
;;  (system (strcat "sndplay " (soundfilename name))))
;;  (system (strcat "play " (soundfilename name) )))
;;
  (play (s-read (soundfilename name))))

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

(setf *runtime-path* (current-path))
(display "system.lsp" *runtime-path*)

;; for Linux, modify s-plot (defined in nyquist.lsp) by saving s-plot
;; in standard-s-plot, then call gnuplot to display the points.
;;
;; we also need to save the location of this file so we can find
;; nyquist-plot.txt, the command file for gnuplot
;;
;; This code is broken in the following ways:
;;    it tries to run gnuplot even when plotting can be done by NyquistIDE
;;    it plots "points.dat", but "points.dat" may not be correct 
;;       (see *default-plot-file*)
;;    it assumes the plot file is in the current directory, but it
;;       by default goes to the sound file directory
;;
;; Fix this code or complain if you want to plot with gnuplot while
;; running ny (or even NyquistIDE (jny) if you want). Otherwise, use
;; NyquistIDE to get s-plot to work.
;;
;(setf *runtime-path* (current-path))
;(display "system.lsp" *runtime-path*)
;
;(setfn standard-s-plot s-plot)
;
;(defun s-plot (s &optional (dur 2.0) (n 1000))
;  (let (plot-file)
;    (standard-s-plot s dur n) ;; this calculates the data points
;    (setf plot-file (strcat *runtime-path* "nyquist-plot.txt"))
;    (system (strcat "gnuplot -persist " plot-file))))

