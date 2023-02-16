$nyquist plug-in
$version 4
$type process spectral
$name (_ "Spectral Edit Multi Tool")
$author (_ "Paul Licameli")
$release 2.3.0-1
$copyright (_ "GNU General Public License v2.0")

;; License: GPL v2
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;; SpectralEditMulti.ny by Paul Licameli, November 2014.
;; Updated by Steve Daulton 2014 / 2015.


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
        (bw (get '*selection* 'bandwidth))
        (tn (truncate len))
        (rate (snd-srate sig))
        (transition (truncate (* 0.01 rate))) ; 10 ms
        (t1 (min transition (/ tn 2)))        ; fade in length (samples)
        (t2 (max (- tn transition) (/ tn 2))) ; length before fade out (samples)
        (breakpoints (list t1 1.0 t2 1.0 tn))
        (env (snd-pwl 0.0 rate breakpoints)))
    (cond
      ((not (or f0 f1)) ; This should never happen for a 'spectral' effect.
        (throw 'error-message
            (format nil (_ "~aPlease select frequencies.") p-err)))
      ((and f0 f1 (= f0 f1))
        (throw 'error-message
          (format nil (_ "~aBandwidth is zero (the upper and lower~%~
                       frequencies are both ~a Hz).~%~
                       Please select a frequency range.")
                  p-err f0)))
      ;; Biqud filter fails if centre frequency is very low and bandwidth very high.
      ;; 'Magic numbers' 10 Hz and 10 octaves are experimental.
      ((and f0 (< f0 10) bw (> bw 10))
        (throw 'error-message
          (format nil (_ "~aNotch filter parameters cannot be applied.~%~
                      Try increasing the low frequency bound~%~
                      or reduce the filter 'Width'.")
                  p-err)))
      ;; low pass frequency is above Nyquist so do nothing
      ((and (not f1) (>= f0 (/ *sound-srate* 2.0)))
          nil)
      ;; notch frequency is above Nyquist so do nothing
      ((and f0 f1 (>= fc (/ *sound-srate* 2.0)))
          nil)
      ;; high-pass above Nyquist so fade to silence
      ((and (not f0)  (>= f1 (/ *sound-srate* 2.0)))
          (mult sig (diff 1.0 env)))
      (T  (sum (prod env (wet sig f0 f1 fc))
               (prod (diff 1.0 env) sig))))))

(catch 'error-message
  (setf p-err (format nil (_ "Error.~%")))
  (multichan-expand #'result *track*))
