;nyquist plug-in
;version 3
;type process
;categories "http://lv2plug.in/ns/lv2core#LowpassPlugin"
;name "Low Pass Filter..."
;action "Performing Low Pass Filter..."

;; lowpass.ny by Dominic Mazzoni
;; Modified by David R. Sky
;; Updated by Steve Daulton June 2012
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .

;; To enable the Q control, remove one semicolon from the start of lines 18 and 32

;control rolloff "Rolloff (dB per octave)" choice "  6 dB,12 dB,24 dB,36 dB,48 dB" 0
;;control q "Filter quality (Q) for 12 dB rolloff" real "" 0.7071 .1 20
;control frequency "Cutoff frequency (Hz)" real "" 1000 1 20000

(cond 
  ((> frequency (/ *sound-srate* 2))
    (format nil 
           "Cutoff frequency is set at ~a Hz but must not~%~
           be greater than ~a Hz (half of the track sample rate)."
           frequency
           (truncate (/ *sound-srate* 2.0))))
  ((< frequency 1)
    (format nil
           "Cutoff frequency is set at ~a Hz~%but must be at least 1 Hz."
           frequency))
; ((= rolloff 1)(lowpass2 s frequency (max (min q 20) 0.1)))
  (T 
    (funcall 
      (nth rolloff '(lp lowpass2 lowpass4 lowpass6 lowpass8))
      s frequency)))
