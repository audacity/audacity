$nyquist plug-in
$version 4
$type process
$preview linear
$name (_ "Tremolo")
$debugbutton disabled
$author (_ "Steve Daulton")
$release 2.4.0
$copyright (_ "GNU General Public License v2.0 or later")

;; tremolo.ny by Steve Daulton (www.easyspacepro.com) July 2012.
;; Based on Tremolo by Dominic Mazzoni and David R. Sky."

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control WAVE (_ "Waveform type") choice ((_ "Sine")
                                          (_ "Triangle" "waveform")
                                          (_ "Sawtooth")
                                          ("InverseSawtooth" (_ "Inverse Sawtooth"))
                                          (_ "Square")) 0

$control PHASE (_ "Starting phase (degrees)") int "" 0 -180 180
$control WET (_ "Wet level (percent)") int "" 40 1 100
$control LFO (_ "Frequency (Hz)") float-text "" 4 0.001 1000


; set tremolo waveform 
(setf waveform
  (abs-env
    (case WAVE
      (0 *sine-table*)
      (1 *tri-table*)
      ; sawtooth
      (2 (maketable (pwlv -1 0.995 1 1 -1)))
      ; inverse sawtooth
      (3 (maketable (pwlv -1 0.005 1 1 -1)))
      ; square
      (4 (maketable (pwlv -1 0.005 1 0.5 1 0.505 -1 1 -1))))))


;;; Generate modulation wave
(defun mod-wave (level)
  ; *sine-table* is 90 degrees rotated compared to other tables.
  (if (= WAVE 0)
      (setf phase-shift (- PHASE 90))
      (setf phase-shift PHASE))
  (sum (- 1 level) 
       (mult level 
             (osc (hz-to-step LFO) 1.0 waveform phase-shift))))


(mult *track* (mod-wave (/ WET 200.0)))
