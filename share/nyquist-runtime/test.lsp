
(defun ss () (osc c5))

(defun tt () (stretch 2 (snd-tapv (ss) 1.1 (scale *d* (lfo 10)) 2.2)))
(setf *d* .01)

(defun g () (play (tt)))

;(set-sound-srate 10)
;(set-control-srate 10)
(defun rr () (stretch 10 (ramp)))
(defun ll () (stretch 10 (lfo .5)))
(defun xx () (snd-tapv (rr) 1.1 (ll) 2.2))
(defun h () (snd-samples (xx) 150))

(defun chorus (sound maxdepth depth rate saturation)
  (let ((modulation (prod depth (stretch-abs 10000.0 (general-lfo rate))))
        (offset (/ maxdepth 2.0))
        chor)
    (setf chor (snd-tapv sound offset modulation maxdepth))
    (sum (prod chor saturation) (prod (seq (s-rest offset) sound)
                                          (sum 1.0 (prod -1.0 saturation))))))


(set-sound-srate 22050.0)

(defun f ()
 (chorus (s-read "runtime\\ah.wav") .1 .1 1 .5))

(defun e ()
 (seq (s-rest .05) (chorus (s-read "rpd.wav") .07 .07 .7 .5)))

(defun d () (sum (e) (f)))

(defun rou () (s-read "round.wav" :time-offset 1.18 :dur (- 8.378 1.18)))

(defun rou4 () (sim (rou)
                    (at *rd* (rou)) 
                    (at (* *rd* 2) (rou)) 
                    (at (* *rd* 3) (rou))))



