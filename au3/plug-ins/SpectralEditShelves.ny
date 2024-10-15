$nyquist plug-in
$version 4
$type process spectral
$preview linear
$name (_ "Spectral Edit Shelves")
$debugbutton false
$author (_ "Paul Licameli")
$release 2.3.0-2
$copyright (_ "GNU General Public License v2.0 or later")

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;; SpectralEditShelves.ny by Paul Licameli, November 2014.
;; Updated by Steve Daulton 2014 / 2015.


$control CONTROL-GAIN (_ "Gain (dB)") real "" 0 -24 24

(defmacro validate (hz)
"If frequency is above Nyquist, don't use it"
  `(if (or (>= ,hz (/ *sound-srate* 2.0))
           (<= ,hz 0))
        (setf ,hz nil)))

(defun mid-shelf (sig lf hf gain)
  "Combines high shelf and low shelf filters"
  (let ((invg (- gain)))
    (scale (db-to-linear gain)
           (eq-highshelf (eq-lowshelf sig lf invg)
                         hf invg))))

(defun wet (sig gain f0 f1)
  "Apply appropriate filter"
  (cond
    ((not f0) (eq-lowshelf sig f1 gain))
    ((not f1) (eq-highshelf sig f0 gain))
    (t (mid-shelf sig f0 f1 gain))))

(defun result (sig)
  (let*
      ((f0 (get '*selection* 'low-hz))
       (f1 (get '*selection* 'high-hz))
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
      ((and f0 (>= f0 (/ *sound-srate* 2.0)))
          ; Shelf is above Nyquist frequency so do nothing.
          nil)
      ((and f0 f1 (= f0 f1))
          (throw 'error-message
            (format nil (_ "~aBandwidth is zero (the upper and lower~%~
                         frequencies are both ~a Hz).~%~
                         Please select a frequency range.")
                    p-err f0)))
      (T  (if f0 (validate f0))
          (if f1 (validate f1))
          (if (not (or f0 f1))  ; 'validate' may return nil
              nil               ; Do nothing
              (sum (prod env (wet sig CONTROL-GAIN f0 f1))
                  (prod (diff 1.0 env) sig)))))))

(catch 'error-message
  (setf p-err (format nil (_ "Error.~%")))
  (if (= CONTROL-GAIN 0)
      "" ; No-op
      (multichan-expand #'result *track*)))
