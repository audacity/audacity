$nyquist plug-in
$version 4
$type process
$name (_ "Crossfade Tracks")
$debugbutton disabled
$preview selection
$author (_ "Steve Daulton")
$release 3.0.4-2
$copyright (_ "GNU General Public License v2.0 or later")

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

$control TYPE (_ "Fade TYPE") choice (
    ("ConstantGain" (_ "Constant Gain"))
    ("ConstantPower1" (_ "Constant Power 1"))
    ("ConstantPower2" (_ "Constant Power 2"))
    ("CustomCurve" (_ "Custom Curve"))) 0
$control CURVE (_ "Custom curve") real "" 0 0 1
$control DIRECTION (_ "Fade direction") choice (
    (_ "Automatic")
    ("OutIn" (_ "Alternating Out / In"))
    ("InOut" (_ "Alternating In / Out"))) 0


(defun crossfade ()
  (setf fade-out
    (case DIRECTION
      (0 (equal (guessdirection) 'OUT))   ; auto
      (1 (oddp (get '*track* 'index)))    ; fade out odd
      (T (evenp (get '*track* 'index))))) ; fade out even
  ; Set control rate to sound rate to ensure length is exact.
  (setf *control-srate* *sound-srate*)
  (mult *track*
    (cond
      (fade-out
        (case TYPE
          (0 (pwlv 1 1 0))
          (1 (osc (hz-to-step (/ (get-duration 4))) 1 *sine-table* 90))
          (2 (s-sqrt (pwlv 1 1 0)))
          (T (custom 0))))
      (T  ; else fade in.
        (case TYPE
          (0 (pwlv 0 1 1))
          (1 (osc (hz-to-step (/ (get-duration 4))) 1))
          (2 (s-sqrt (pwlv 0 1 1)))
          (T (custom 1)))))))

(defun custom (inout)
  ;; 'epsilon' defines the curvature of a logarithmc curve.
  ;; To avoid log 0 or /0 it must be > 0 and < 1.
  (let* ((ccurve (+ 0.99 (* -0.98 CURVE)))
         ; magic number 2.7 gives approx 'constant power' curve at 50% setting.
         (epsilon (power ccurve 2.7)))
    (if (= inout 0)
        (setf logcurve (pwev epsilon 1 1))
        (setf logcurve (pwev 1 1 epsilon)))
    ; Scale and invert curve for 0 to unity gain.
    (sum 1
         (mult (/ -1 (- 1 epsilon))
               (diff logcurve epsilon)))))

(defun guessdirection ()
  ;;; If the selection is closer to the start of the
  ;;; audio clip, fade in, otherwise fade out.
  ;;; Use `inclips`, i.e., the clip boundaries before the stretch-rendering pre-processing step.
  (let* ((start (get '*selection* 'start))
         (end (get '*selection* 'end))
         (clips (get '*track* 'inclips))
         (in-dist end)
         (out-dist end))
    (if (arrayp clips)
        (setf clips (append (aref clips 0)(aref clips 1))))
    (dotimes (i (length clips))
      (setf in-dist (min in-dist (abs (- start (first (nth i clips))))))
      (setf out-dist (min out-dist (abs (- end (second (nth i clips)))))))
    (if (< in-dist out-dist) 'in 'out)))


(if (< (length (get '*selection* 'tracks)) 2)
    (format nil (_ "Error.~%Select 2 (or more) tracks to crossfade."))
    (crossfade))
