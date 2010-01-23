;nyquist plug-in
;version 3
;type process
;categories "http://lv2plug.in/ns/lv2core#ModulatorPlugin"
;name "Tremolo..."
;action "Applying Tremolo..."
;info "by Dominic Mazzoni, modified by David R. Sky\nReleased under terms of the GNU General Public License Version 2\n'Starting phase' sets where to start tremolo in the waveform cycle.\n'Wetness level' sets depth of tremolo - 0 percent is no tremolo,\n100 percent sweeps between zero and maximum volume.\n'Frequency' controls the speed of the oscillation - use higher\nfrequencies for faster oscillation."

;control wave "Waveform type" choice "sine,triangle,sawtooth,inverse sawtooth,square" 0
;control phase "      Starting phase [degrees]" int "" 0 -180 180
;control amount "      Wetness level [percent]" int "" 40 0 100
;control lfo "      Frequency [Hz]" real "" 4.0 0.1 10.0

; set tremolo *waveform* 
(setq *waveform* (cond
((= wave 0) ; sine
*sine-table*)
((= wave 1) ; triangle
*tri-table*)
((= wave 2) ; sawtooth
(abs-env (list (pwl 0 -1 .995  1 1 -1 1) (hz-to-step 1.0) t)))
((= wave 3) ; inverse sawtooth
(abs-env (list (pwl 0 1 .995  -1 1 1 1) (hz-to-step 1.0) t)))
(t ; square
(abs-env (list (pwl 0 1 .495 1 .5 -1 .995 -1 1 1 1) (hz-to-step 1.0) t)))))


; check for negative [invalid] frequency value
(cond ((<= lfo 0)
(format nil 
"Error\n\nYou have entered an invalid frequency of ~a Hz.   
Please enter a frequency above 0 Hz." lfo))
;
(t
; apply tremolo
(mult (sum (const (- 1.0 (/ amount 200.0))) (scale (/ amount 200.0) 
(osc (hz-to-step lfo) 1.0 *waveform* phase))) s)))

; from previous commit
; arch-tag: 0ee3925a-8016-44db-91e8-8c4b7a9f3992

