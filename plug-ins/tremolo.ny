;nyquist plug-in
;version 3
;type process
;preview linear
;categories "http://lv2plug.in/ns/lv2core#ModulatorPlugin"
;name "Tremolo..."
;action "Applying Tremolo..."
;author "Steve Daulton"
;copyright "Released under terms of the GNU General Public License version 2"

;; tremolo.ny by Steve Daulton (www.easyspacepro.com) July 2012.
;; Based on Tremolo by Dominic Mazzoni and David R. Sky."
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; http://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;control wave "Waveform type" choice "sine,triangle,sawtooth,inverse sawtooth,square" 0
;control phase "Starting phase (degrees)" int "" 0 -180 180
;control wet "Wet level (percent)" int "" 40 0 100
;control lfo "Frequency (Hz)" real "" 4 0 10

; Limit to sensible range
(setq lfo (min 1000 (max lfo (/ (get-duration 1)))))

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
