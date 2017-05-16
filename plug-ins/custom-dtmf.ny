;nyquist plug-in
;version 1
;type generate
;name "Custom DTMF Generator"
;action "Generating DTMF..."
;info "Generate a DTMF tone using the parameters below. \nContributed by W. Robertson"

;control tl "Tone duration (milliseconds)" int "" 100 1 10000
;control sl "Silence duration after tone (milliseconds)" int "" 100 0 10000
;control high "Frequency 1 (Hz)" int "" 425 1 10000
;control low "Frequency 2 (Hz)" int "" 25 1 10000
;control volume "Volume (percent)" int "" 80 1 100

; DTMF (Dual Tone Multi-Frequency) generator
; This version allows the user to specify the two frequencies in the DTMF
; tone, which can be useful for generating other telephony tones, or
; SELCAL tones used by aircraft on HF radio communications. 
; Based on the original telephone DTMF generator created by:
;   David R. Sky, Dominic Mazzoni, Roger B. Dannenberg, W. Borgert
;
; Released under terms of the GNU General Public License version 2
; http://www.gnu.org/copyleft/gpl.html


; convert volume percent to flonum
; DTMF uses two tones so we cut volume by further one-half
(setf volume (/ volume 200.0))

; convert milliseconds to seconds
(setf tl (/ tl 1000.0))
(setf sl (/ sl 1000.0))

(defun dtmf (volume tl sl high low)
  (seq
    (mult volume
      (pwl 0.002 1 (- tl 0.002) 1 tl)
      (sim
        (osc (hz-to-step high) tl)
        (osc (hz-to-step low) tl)
      )
    )
    (s-rest sl)
  )
)

(dtmf volume tl sl high low)
