;nyquist plug-in
;version 4
;type process
;preview linear
;name "Low Pass Filter..."
;action "Performing Low Pass Filter..."
;author "Dominic Mazzoni"
;copyright "Released under terms of the GNU General Public License version 2"

;; lowpass.ny by Dominic Mazzoni
;; Last updated August 2015
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .


;control frequency "Frequency (Hz)" float-text "" 1000 0 nil
;control rolloff "Rolloff (dB per octave)" choice "  6 dB,12 dB,24 dB,36 dB,48 dB" 0


(cond
  ((< frequency 0.1) "Frequency must be at least 0.1 Hz.")
  ((>= frequency (/ *sound-srate* 2.0))
      (format nil "Error:~%~%Frequency (~a Hz) is too high for track sample rate.~%~%~
                   Track sample rate is ~a Hz~%~
                   Frequency must be less than ~a Hz."
              frequency
              *sound-srate*
              (/ *sound-srate* 2.0)))
  (T  (funcall (nth rolloff '(lp lowpass2 lowpass4 lowpass6 lowpass8))
               *track* frequency)))
