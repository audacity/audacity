$nyquist plug-in
$version 3
$type process
$preview linear
$name (_ "Tremolo")
$manpage "Tremolo"
$debugbutton disabled
$action (_ "Applying Tremolo...")
$author (_ "Steve Daulton")
$release 2.3.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; tremolo.ny by Steve Daulton (www.easyspacepro.com) July 2012.
;; Based on Tremolo by Dominic Mazzoni and David R. Sky."

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

$control wave (_ "Waveform type") choice (
   (_ "Sine")
   (_ "Triangle")
   (_ "Sawtooth")
   ("InverseSawtooth" (_ "Inverse Sawtooth"))
   (_ "Square")
) 0
$control phase (_ "Starting phase (degrees)") int "" 0 -180 180
$control wet (_ "Wet level (percent)") int "" 40 1 100
$control lfo (_ "Frequency (Hz)") float-text "" 4 0.001 1000

; Convert % to linear
(setq wet (/ wet 200.0))

; set tremolo waveform 
(setq waveform
  (abs-env
    (case wave
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
  (when (= wave 0)
    (setq phase (- phase 90)))
  (sum (- 1 level) 
    (mult level 
      (osc (hz-to-step lfo) 1.0 waveform phase))))

(mult s (mod-wave wet))
