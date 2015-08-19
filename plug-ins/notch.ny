;nyquist plug-in
;version 4
;type process
;preview linear
;name "Notch Filter..."
;action "Applying Notch Filter..."
;author "Steve Daulton and Bill Wharrie"
;copyright "Released under terms of the GNU General Public License version 2"

;; notch.ny by Steve Daulton and Bill Wharrie, September 2010.
;; Last updated August 2015.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .

;control range "Frequency range" choice "Hz,kHz" 0
;control frequency "Frequency" real "" 60 1 1000
;control q "Q (higher value reduces width)" real "" 1 0.1 20


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
    (notch2 *track* frequency q))
