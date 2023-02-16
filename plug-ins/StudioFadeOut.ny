$nyquist plug-in
$version 4
$type process
$name (_ "Studio Fade Out")
$author (_ "Steve Daulton")
$release 3.0.4-1
$copyright (_ "GNU General Public License v2.0 or later")

;; Produce a smooth and musical sounding fade out.
;; Applies a sinusoidal fade out with a progressive low-pass
;; filter from full spectrum at start to 100 Hz at end.

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


;;; sweeping low pass filter 
 (defun filter (sig dur)
  (abs-env
    ;; cross-fade the filter
    (let* ((nyq-hz (/ *sound-srate* 2))
           (f-out (r-cos (min (/ dur 2.0) 0.5)))
           (f-in (diff (snd-const 1 0 *sound-srate* dur) f-out)))
      (sim
        (mult f-out sig)
        (mult f-in (lp sig (pwlv nyq-hz dur 100)))))))

;;; raised cosine
(defun r-cos (dur)
  (abs-env
    (mult 0.5 
      (sum 1 
        (osc (hz-to-step (/ (* dur 2))) dur *table* 90)))))

(let ((dur (get-duration 1)))
  (cond
    ((< len 3) (format nil (_ "Selection too short.~%It must be more than 2 samples.")))
    ((< dur 0.2) (mult *track* (r-cos dur)))
    (t (mult (filter *track* dur)(r-cos dur)))))
  
