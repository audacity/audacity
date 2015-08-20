;nyquist plug-in
;version 4
;type process
;preview linear
;name "High Pass Filter..."
;action "Performing High Pass Filter..."
;author "Dominic Mazzoni"
;copyright "Released under terms of the GNU General Public License version 2"

;; highpass.ny by Dominic Mazzoni
;; Last updated August 2015
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .

;control range "Frequency range" choice "Hz,kHz" 0
;control frequency "Frequency" real "" 1000 1 1000
;control rolloff "Rolloff (dB per octave)" choice "  6 dB,12 dB,24 dB,36 dB,48 dB" 0


(case range
  (0 (setf f0 (format nil "~a Hz" frequency))
     (setf srate (format nil "~a Hz" *sound-srate*))
     (setf fmax (format nil "~a Hz" (/ *sound-srate* 2))))
  (T (setf f0 (format nil "~a kHz" frequency))
     (setf frequency (* frequency 1000))
     (setf srate (format nil "~a kHz" (/ *sound-srate* 1000)))
     (setf fmax (format nil "~a kHz" (/ *sound-srate* 2000)))))


(if (>= frequency (/ *sound-srate* 2.0))
    (format nil "Error:\nFrequency (~a) too high for track sample rate.~%~%~
                 Track sample rate is ~a~%~
                 Frequency must be less than ~a."
            f0
            srate
            fmax) 
    (funcall 
      (nth rolloff '(hp highpass2 highpass4 highpass6 highpass8))
      *track* frequency))
