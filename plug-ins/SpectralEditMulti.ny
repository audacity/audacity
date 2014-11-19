;nyquist plug-in
;version 3
;type process
;name "Spectral edit multi tool"
;action "Calculating..."
;author "Paul Licameli"
;copyright "Unknown"

(defun wet (sig)
  (cond
   ((not (or *f0* *f1*)) (throw 'error-message "Please select frequencies"))
   ((not *f0*) (highpass2 sig *f1*))
   ((not *f1*) (lowpass2 sig *f0*))
   (t (if (= *f0* *f1*)
          (throw 'error-message "Band width is undefined")
          (let*
              ((fc (sqrt (* *f0* *f1*)))
               (width (abs (- *f1* *f0*)))
               (q (/ fc width)))
            (notch2 sig fc q))))))

(defun result (sig)
  (let*
      ((tn (truncate len))
       (rate (snd-srate sig))
       (transition (truncate (* 0.01 rate)))
       (t1 (min transition (/ tn 2)))
       (t2 (max (- tn transition) (/ tn 2)))
       (breakpoints (list t1 1.0 t2 1.0 tn))
       (env (snd-pwl 0.0 rate breakpoints)))
    (sum (prod env (wet sig)) (prod (diff 1.0 env) sig))))

(catch 'error-message
  (multichan-expand #'result s))

