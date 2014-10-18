;nyquist plug-in
;version 3
;type process
;name "Spectral edit shelves"
;action "Calculating..."

;control control-gain "Gain (dB)" real "" 0 -24 24

(defun mid-shelf (sig lf hf gain)
  "Combines high shelf and low shelf filters"
  (let* ((invg (- gain)))
    (scale (db-to-linear gain)
           (eq-highshelf (eq-lowshelf sig lf invg)
                         hf invg))))

(defun wet (sig gain)
  (cond
   ((not (or *f0* *f1*)) (throw 'debug-message "Please select frequencies"))
   ((not *f0*) (eq-lowshelf sig *f1* gain))
   ((not *f1*) (eq-highshelf sig *f0* gain))
   (t (mid-shelf sig *f0* *f1* gain))))

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

