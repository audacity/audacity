$nyquist plug-in
$version 4
$type process spectral
$preview linear
$name (_ "Spectral edit parametric EQ")
$manpage "Spectral_edit_parametric_EQ"
$action (_ "Filtering...")
$author (_ "Paul Licameli")
$release 2.3.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; SpectralEditParametricEQ.ny by Paul Licameli, November 2014.
;; Updated by Steve Daulton 2014 / 2015.

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

$control control-gain (_ "Gain (dB)") real "" 0 -24 24

(defun wet (sig gain fc bw)
  (eq-band sig fc gain (/ bw 2)))

(defun result (sig)
  (let*
      ((f0 (get '*selection* 'low-hz))
       (f1 (get '*selection* 'high-hz))
       (fc (get '*selection* 'center-hz))
       (bw (get '*selection* 'bandwidth))
       (tn (truncate len))
       (rate (snd-srate sig))
       (transition (truncate (* 0.01 rate)))  ; 10 ms
       (t1 (min transition (/ tn 2)))         ; fade in length (samples)
       (t2 (max (- tn transition) (/ tn 2)))  ; length before fade out (samples)
       (breakpoints (list t1 1.0 t2 1.0 tn))
       (env (snd-pwl 0.0 rate breakpoints)))
    (cond
      ((not (or f0 f1)) ; This should never happen for a 'spectral' effect.
          (throw 'error-message (format nil (_ "~aPlease select frequencies.") p-err)))
      ((not f0)
          (throw 'error-message (format nil (_ "~aLow frequency is undefined.") p-err)))
      ((not f1)
          (throw 'error-message (format nil (_ "~aHigh frequency is undefined.") p-err)))
      ((and fc (= fc 0))
          (throw 'error-message (format nil (_ "~aCenter frequency must be above 0 Hz.") p-err)))
      ((and f1 (> f1 (/ *sound-srate* 2)))
          (throw 'error-message
            (format nil (_ "~aFrequency selection is too high for track sample rate.~%~
                        For the current track, the high frequency setting cannot~%~
                        be greater than ~a Hz")
                    p-err (/ *sound-srate* 2))))
      ((and bw (= bw 0))
          (throw 'error-message
            (format nil (_ "~aBandwidth is zero (the upper and lower~%~
                         frequencies are both ~a Hz).~%~
                         Please select a frequency range.")
                    p-err f0)))
      ;; If centre frequency band is above Nyquist, do nothing.
      ((and fc (>= fc (/ *sound-srate* 2.0)))
          nil)
      (t  (sum (prod env (wet sig control-gain fc bw))
               (prod (diff 1.0 env) sig))))))

(catch 'error-message
  (setf p-err (format nil (_ "Error.~%")))
  (if (= control-gain 0)
      nil ; Do nothing
      (multichan-expand #'result *track*)))
