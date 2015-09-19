;nyquist plug-in
;version 4
;type process
;name "Crossfade Tracks..."
;action "Crossfading..."
;preview selection
;author "Steve Daulton"
;copyright "Released under terms of the GNU General Public License version 2"

;; crossfadetracks.ny by Steve Daulton Nov 2014.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .

;control type "Fade type" choice "Constant Gain,Constant Power 1,Constant Power 2,Custom Curve" 0
;control curve "Custom curve" real "" 0 0 1

(defun crossfade (type)
  (mult *track*
    (cond
      ((= (get '*track* 'index) 1)  ; fade out.
        (case type
          (0 (pwlv 1 1 0))
          (1 (osc (hz-to-step (/ (get-duration 4))) 1 *sine-table* 90))
          (2 (s-sqrt (pwlv 1 1 0)))
          (T (custom curve 0))))
      (T  ; else fade in.
        (setf *control-srate* *sound-srate*)
        (case type
          (0 (pwlv 0 1 1))
          (1 (osc (hz-to-step (/ (get-duration 4))) 1))
          (2 (s-sqrt (pwlv 0 1 1)))
          (T (custom curve 1)))))))

(defun custom (curve inout)
  ;; 'epsilon' defines the curvature of a logarithmc curve.
  ;; To avoid log 0 or /0 it must be > 0 and < 1.
  (let* ((curve (+ 0.99 (* -0.98 (min 1 (max 0 curve)))))
         ; magic number 2.7 gives approx 'constant power' curve at 50% setting.
         (epsilon (power curve 2.7)))
    (if (= inout 0)
        (setf logcurve (pwev epsilon 1 1))
        (setf logcurve (pwev 1 1 epsilon)))
    (sum 1
         (mult (/ -1 (- 1 epsilon))
               (diff logcurve epsilon)))))

(let ((tracks (length (get '*selection* 'tracks))))
  (case tracks
    (1 "Only 1 track selected.\n'Crossfade Tracks' requires 2 tracks.")
    (2 (crossfade type))
    (T "Too many tracks selected.\n'Crossfade Tracks' requires 2 tracks.")))
