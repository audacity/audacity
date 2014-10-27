;nyquist plug-in
;version 3
;type process
;name "Spectral edit parametric EQ..."
;action "Calculating..."

;control control-gain "Gain (dB)" real "" 0 -24 24

(defun wet (sig gain)
  (cond
   ((not (or *f0* *f1*)) (throw 'debug-message "Please select frequencies"))
   ((not *f0*) (throw 'debug-message "Bottom frequency is undefined"))
   ((not *f1*) (throw 'debug-message "Top frequency is undefined"))
   (t (let*
          ((fc (sqrt (* *f0* *f1*)))
           (width-octaves (/ (s-log (/ *f1* *f0*)) (s-log 2.0))))
        (eq-band sig fc gain (/ width-octaves 2))))))

(defun result (sig)
  (let*
      ((tn (truncate len))
       (rate (snd-srate sig))
       (transition (truncate (* 0.01 rate)))
       (t1 (min transition (/ tn 2)))
       (t2 (max (- tn transition) (/ tn 2)))
       (breakpoints (list t1 1.0 t2 1.0 tn))
       (env (snd-pwl 0.0 rate breakpoints)))
    (sum (prod env (wet sig control-gain)) (prod (diff 1.0 env) sig))))

(catch 'debug-message
  (multichan-expand #'result s))
