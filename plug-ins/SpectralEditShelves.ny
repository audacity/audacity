;nyquist plug-in
;version 4
;type process
;preview linear
;name "Spectral edit shelves..."
;action "Filtering..."
;author "Paul Licameli"
;copyright "Released under terms of the GNU General Public License version 2"


;; SpectralEditShelves.ny by Paul Licameli, November 2014.
;; Updated to version 4 by Steve Daulton November 2014.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html

;control control-gain "Gain (dB)" real "" 0 -24 24

(setf control-gain (min 24 (max -24 control-gain))) ; excessive settings may crash

(defmacro validate (hz)
"Ensure frequency is below Nyquist"
  `(setf ,hz (max 0 (min (/ *sound-srate* 2.0) ,hz))))

(defun mid-shelf (sig lf hf gain)
  "Combines high shelf and low shelf filters"
  (let* ((invg (- gain)))
    (scale (db-to-linear gain)
           (eq-highshelf (eq-lowshelf sig lf invg)
                         hf invg))))

(defun wet (sig gain f0 f1)
  (cond
   ((not f0) (eq-lowshelf sig (validate f1) gain))
   ((not f1) (eq-highshelf sig (validate f0) gain))
   (t
    (validate f0)
    (validate f1)
    (when (= f0 f1)
      (throw 'error-message "Please select a frequency range."))
    (mid-shelf sig f0 f1 gain))))

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
      ((not (or f0 f1))
          (throw 'error-message (format nil "~aPlease select frequencies." p-err)))
      ; shelf is above Nyquist frequency so do nothing
      ((and f0 (>= f0 (/ *sound-srate* 2.0))) nil)
      (T (sum (prod env (wet sig control-gain f0 f1))
              (prod (diff 1.0 env) sig))))))

;; Frequency selection must be between 0 Hz and Nyquist.
(if (and (get '*selection* 'low-hz)
         (<= (get '*selection* 'low-hz) 0))
    (remprop '*selection* 'low-hz))
(if (and (get '*selection* 'high-hz)
         (>= (get '*selection* 'high-hz)(/ *sound-srate* 2)))
    (remprop '*selection* 'high-hz))

(catch 'error-message
  (setf p-err "")
  (if (= control-gain 0)		; Allow dry preview
      "Gain is zero. Nothing to do."
    (multichan-expand #'result *track*)))

