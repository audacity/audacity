;nyquist plug-in
;version 4
;type process
;name "Spectral edit multi tool"
;action "Filtering..."
;author "Paul Licameli"
;copyright "Released under terms of the GNU General Public License version 2"

;; SpectralEditMulti.ny by Paul Licameli, November 2014.
;; Updated to version 4 by Steve Daulton November 2014.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html

(defun wet (sig f0 f1 fc)
  (cond
    ((not f0) (highpass2 sig f1))
    ((not f1) (lowpass2 sig f0))
    (T  (let ((q (/ fc (- f1 f0))))
          (notch2 sig fc q)))))

(defun result (sig)
  (let* ((f0 (get '*selection* 'low-hz))
        (f1 (get '*selection* 'high-hz))
        (fc (get '*selection* 'center-hz))
        (tn (truncate len))
        (rate (snd-srate sig))
        (transition (truncate (* 0.01 rate))) ; 10 ms
        (t1 (min transition (/ tn 2)))        ; fade in length (samples)
        (t2 (max (- tn transition) (/ tn 2))) ; length before fade out (samples)
        (breakpoints (list t1 1.0 t2 1.0 tn))
        (env (snd-pwl 0.0 rate breakpoints)))
    (cond
      ((not (or f0 f1)) (throw 'error-message "Please select frequencies."))
      ((and f0 f1 (= f0 f1)) (throw 'error-message
                      "Bandwidth is zero.\nPlease select a frequency range."))
      ;; low pass frequency is above Nyquist so do nothing
      ((and (not f1) (>= f0 (/ *sound-srate* 2.0)))
          nil)
      ;; notch frequency is above Nyquist so do nothing
      ((and (and f0 f1) (>= fc (/ *sound-srate* 2.0)))
          nil)
      ;; high-pass above Nyquist so fade to silence
      ((and (not f0)  (>= f1 (/ *sound-srate* 2.0)))
          (mult sig (diff 1.0 env)))
      (T  (sum (prod env (wet sig f0 f1 fc))
               (prod (diff 1.0 env) sig))))))

(if (string-not-equal (get '*TRACK* 'VIEW) "spectral"  :end1 8 :end2 8)
    "Use this effect in the 'Spectral Selection'\nor 'Spectral Selection log(f)' view."
    (catch 'error-message
      (multichan-expand #'result *track*)))
