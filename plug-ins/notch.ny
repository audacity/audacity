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


;control frequency "Frequency (Hz)" float-text "" 60 0 nil
;control q "Q (higher value reduces width)" float-text "" 1 0.1 nil


(cond
  ((< frequency 0.1) "Frequency must be at least 0.1 Hz.")
  ((>= frequency (/ *sound-srate* 2.0))
      (format nil "Error:~%~%Frequency (~a Hz) is too high for track sample rate.~%~%~
                   Track sample rate is ~a Hz~%~
                   Frequency must be less than ~a Hz."
              frequency
              *sound-srate*
              (/ *sound-srate* 2.0)))
  ((< q 0.1) "Q must be at least 0.1.")
  (T  (notch2 *track* frequency q)))
