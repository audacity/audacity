;; dspprims.lsp -- interface to dsp primitives

;; ARESON - notch filter
;; 
(defun areson (s c b &optional (n 0))
  (multichan-expand "ARESON" #'nyq:areson
    '(((SOUND) nil) ((NUMBER SOUND) "center")
      ((NUMBER SOUND) "bandwidth") ((INTEGER) nil))
    s c b n))

(setf areson-implementations
      (vector #'snd-areson #'snd-aresonvc #'snd-aresoncv #'snd-aresonvv))

;; NYQ:ARESON - notch filter, single channel
;;
(defun nyq:areson (signal center bandwidth normalize)
  (select-implementation-1-2 "ARESON" areson-implementations 
   signal center bandwidth normalize))


;; hp - highpass filter
;; 
(defun hp (s c)
  (multichan-expand "HP" #'nyq:hp
    '(((SOUND) "snd") ((NUMBER SOUND) "cutoff")) s c))

(setf hp-implementations
      (vector #'snd-atone #'snd-atonev))

;; NYQ:hp - highpass filter, single channel
;;
(defun nyq:hp (s c)
  (select-implementation-1-1 "HP" hp-implementations s c))


;; comb-delay-from-hz -- compute the delay argument
;;
(defun comb-delay-from-hz (hz)
  (recip hz))

;; comb-feedback -- compute the feedback argument
;;
(defun comb-feedback (decay delay)
  (s-exp (mult -6.9087 delay (recip decay))))

;; COMB - comb filter
;; 
;; this is just a feedback-delay with different arguments
;;
(defun comb (snd decay hz)
  (multichan-expand "COMB" #'nyq:comb
    '(((SOUND) "snd") ((NUMBER SOUND) "decay") ((POSITIVE) "hz"))
    snd decay hz))


(defun nyq:comb (snd decay hz)
  (let (delay feedback len d)
    ; convert decay to feedback
    (setf delay (/ (float hz)))
    (setf feedback (comb-feedback decay delay))
    (nyq:feedback-delay snd delay feedback "COMB")))

;; ALPASS - all-pass filter
;; 
(defun alpass (snd decay hz &optional min-hz)
  (multichan-expand "ALPASS" #'nyq:alpass
    '(((SOUND) "snd") ((NUMBER SOUND) "decay")
      ((POSITIVE SOUND) "hz") ((POSITIVE-OR-NULL) "min-hz"))
    snd decay hz min-hz))
  
(defun nyq:alpass (snd decay hz min-hz)
  (let (delay feedback len d)
    ; convert decay to feedback, iterate over array if necessary
    (setf delay (comb-delay-from-hz hz))
    (setf feedback (comb-feedback decay delay))
    (nyq:alpass1 snd delay feedback min-hz)))


;; CONST -- a constant at control-srate
;;
(defun const (value &optional (dur 1.0))
  (ny:typecheck (not (numberp value))
    (ny:error "CONST" 1 '((NUMBER) "value") value))
  (ny:typecheck (not (numberp dur))
    (ny:error "CONST" 2 '((NUMBER) "dur") dur))
  (let ((d (get-duration dur)))
    (snd-const value *rslt* *CONTROL-SRATE* d)))


;; CONVOLVE - fast convolution
;; 
(defun convolve (s r)
  (multichan-expand "CONVOLVE" #'nyq:convolve
    '(((SOUND) nil) ((SOUND) nil)) s r))

(defun nyq:convolve (s r)
  (snd-convolve s (force-srate (snd-srate s) r)))


;; FEEDBACK-DELAY -- (delay is quantized to sample period)
;;
(defun feedback-delay (snd delay feedback)
  (multichan-expand "FEEDBACK-DELAY" #'nyq:feedback-delay 
    '(((SOUND) "snd") ((NUMBER) "delay") ((NUMBER SOUND) "feedback"))
    snd delay feedback))
  

;; SND-DELAY-ERROR -- report type error
;;
(defun snd-delay-error (snd delay feedback)
  (error "FEEDBACK-DELAY with variable delay is not implemented"))


(setf feedback-delay-implementations
      (vector #'snd-delay #'snd-delay-error #'snd-delaycv #'snd-delay-error))


;; NYQ:FEEDBACK-DELAY -- single channel delay
;;
(defun nyq:feedback-delay (snd delay feedback &optional (src "FEEDBACK-DELAY"))
  (select-implementation-1-2 src feedback-delay-implementations 
                             snd delay feedback))


;; SND-ALPASS-ERROR -- report type error
;;
(defun snd-alpass-error (snd delay feedback)
  (error "ALPASS with constant decay and variable hz is not implemented"))


(if (not (fboundp 'snd-alpasscv))
    (defun snd-alpasscv (snd delay feedback min-hz)
      (error "snd-alpasscv (ALPASS with variable decay) is not implemented")))
(if (not (fboundp 'snd-alpassvv))
    (defun snd-alpassvv (snd delay feedback min-hz)
      (error "snd-alpassvv (ALPASS with variable decay and feedback) is not implemented")))
      

(defun nyq:alpassvv (the-snd delay feedback min-hz)
    (let (max-delay)
      (ny:typecheck (or (not (numberp min-hz)) (<= min-hz 0))
        (ny:error "ALPASS" 4 '((POSITIVE) "min-hz") min-hz))
      (setf max-delay (/ (float min-hz)))
      ; make sure delay is between 0 and max-delay
      ; use clip function, which is symmetric, with an offset
      (setf delay (snd-offset (clip (snd-offset delay (* max-delay -0.5))
                                    (* max-delay 0.5))
                              (* max-delay 0.5)))
      ; now delay is between 0 and max-delay, so we won't crash nyquist when
      ; we call snd-alpassvv, which doesn't test for out-of-range data
      (snd-alpassvv the-snd delay feedback max-delay)))


;; NYQ:SND-ALPASS -- ignores min-hz argument and calls snd-alpass
;;
(defun nyq:snd-alpass (snd delay feedback min-hz)
  (snd-alpass snd delay feedback))

;; NYQ:SND-ALPASSCV -- ignores min-hz argument and calls snd-alpasscv
;;
(defun nyq:snd-alpasscv (snd delay feedback min-hz)
  (snd-alpasscv snd delay feedback))

(setf alpass-implementations
      (vector #'nyq:snd-alpass #'snd-alpass-error
              #'nyq:snd-alpasscv #'nyq:alpassvv))


;; NYQ:ALPASS1 -- single channel alpass
;;
(defun nyq:alpass1 (snd delay feedback min-hz)
  (select-implementation-1-2 "ALPASS" alpass-implementations
                              snd delay feedback min-hz))

;; CONGEN -- contour generator, patterned after gated analog env gen
;;
(defun congen (gate rise fall)
  (multichan-expand "CONGEN" #'snd-congen
    '(((SOUND) "gate") ((NONNEGATIVE) "rise") ((NONNEGATIVE) "fall"))
    gate rise fall))


;; S-EXP -- exponentiate a sound
;;
(defun s-exp (s)
  (multichan-expand "S-EXP" #'nyq:exp
    '(((NUMBER SOUND) nil)) s))


;; NYQ:EXP -- exponentiate number or sound
;;
(defun nyq:exp (s) (if (soundp s) (snd-exp s) (exp s)))

;; S-ABS -- absolute value of a sound
;;
(defun s-abs (s)
  (multichan-expand "S-ABS" #'nyq:abs
    '(((NUMBER SOUND) nil)) s))

;; NYQ:ABS -- absolute value of number or sound
;;
(defun nyq:abs (s)
  (if (soundp s) (snd-abs s) (abs s)))

;; S-AVG -- moving average or peak computation
;;
(defun s-avg (s blocksize stepsize operation)
  (multichan-expand "S-AVG" #'snd-avg
    '(((SOUND) nil) ((INTEGER) "blocksize") ((INTEGER) "stepsize")
      ((INTEGER) "operation"))
    s blocksize stepsize operation))

;; S-SQRT -- square root of a sound
;;
(defun s-sqrt (s)
  (multichan-expand "S-SQRT" #'nyq:sqrt
    '(((NUMBER SOUND) nil)) s))


;; NYQ:SQRT -- square root of a number or sound
;;
(defun nyq:sqrt (s)
  (if (soundp s) (snd-sqrt s) (sqrt s)))


;; INTEGRATE -- integration
;;
(defun integrate (s)
  (multichan-expand "INTEGRATE" #'snd-integrate
    '(((SOUND) nil)) s))


;; S-LOG -- natural log of a sound
;;
(defun s-log (s)
  (multichan-expand "S-LOG" #'nyq:log
    '(((NUMBER SOUND) nil)) s))


;; NYQ:LOG -- log of a number or sound
;;
(defun nyq:log (s)
  (if (soundp s) (snd-log s) (log s)))


;; NOISE -- white noise
;;
(defun noise (&optional (dur 1.0))
  (ny:typecheck (not (numberp dur))
    (ny:error "NOISE" 1 number-anon dur))
  (let ((d (get-duration dur)))
    (snd-white *rslt* *SOUND-SRATE* d)))


(defun noise-gate (snd &optional (lookahead 0.5) (risetime 0.02) (falltime 0.5)
                       (floor 0.01) (threshold 0.01) &key (rms nil) (link t))
  (let ((sense (if rms (rms snd 100.0 nil "NOISE-GATE") (s-abs snd))))
    (cond (link
           (mult snd (gate sense lookahead risetime falltime floor
                           threshold "NOISE-GATE")))
          (t
           (mult snd (multichan-expand "NOISE-GATE" #'gate
                      '(((SOUND) "sound") ((NUMBER) "lookahead")
                        ((NUMBER) "risetime") ((NUMBER) "falltime")
                        ((NUMBER) "floor") ((NUMBER) "threshold")
                        ((STRING) "source"))
                      sense lookahead risetime falltime
                      floor threshold "NOISE-GATE"))))))


;; QUANTIZE -- quantize a sound
;;
(defun quantize (s f)
  (multichan-expand "QUANTIZE" #'snd-quantize
    '(((SOUND) nil) ((POSITIVE) nil)) s f))


;; RECIP -- reciprocal of a sound
;;
(defun recip (s)
  (multichan-expand "RECIP" #'nyq:recip
    '(((NUMBER SOUND) nil)) s))


;; NYQ:RECIP -- reciprocal of a number or sound
;;
(defun nyq:recip (s)
  (if (soundp s) (snd-recip s) (/ (float s))))



;; RMS -- compute the RMS of a sound
;;
(defun rms (s &optional (rate 100.0) window-size (source "RMS"))
  (multichan-expand "RMS" #'ny:rms
    '(((SOUND) nil) ((POSITIVE) "rate") ((POSITIVE-OR-NULL) "window-size")
      ((STRING) "source"))
    s rate window-size source))


;; NY:RMS -- single channel RMS
;;
(defun ny:rms (s &optional (rate 100.0) window-size source)
  (let (rslt step-size)
    (ny:typecheck (not (or (soundp s) (multichannel-soundp s)))
      (ny:error source 1 '((SOUND) NIL) s t))
    (ny:typecheck (not (numberp rate))
      (ny:error source 2 '((NUMBER) "rate") rate))
    (setf step-size (round (/ (snd-srate s) rate)))
    (cond ((null window-size)
           (setf window-size step-size))
          ((not (integerp window-size))
           (ny:error source 3 '((INTEGER) "window-size" window-size))))
    (setf s (prod s s))
    (setf result (snd-avg s window-size step-size OP-AVERAGE))
    ;; compute square root of average
    (s-exp (scale 0.5 (s-log result)))))


;; RESON - bandpass filter
;; 
(defun reson (s c b &optional (n 0))
  (multichan-expand "RESON" #'nyq:reson
    '(((SOUND) "snd") ((NUMBER SOUND) "center")
      ((NUMBER SOUND) "bandwidth") ((INTEGER) "n"))
    s c b n))


(setf reson-implementations
      (vector #'snd-reson #'snd-resonvc #'snd-resoncv #'snd-resonvv))

;; NYQ:RESON - bandpass filter, single channel
;;
(defun nyq:reson (signal center bandwidth normalize)
  (select-implementation-1-2 "RESON" reson-implementations 
   signal center bandwidth normalize))


;; SHAPE -- waveshaper
;;
(defun shape (snd shape origin)
  (multichan-expand "SHAPE" #'snd-shape
    '(((SOUND) "snd") ((SOUND) "shape") ((NUMBER) "origin"))
    snd shape origin))


;; SLOPE -- calculate the first derivative of a signal
;;
(defun slope (s)
  (multichan-expand "SLOPE" #'nyq:slope
    '(((SOUND) nil)) s))


;; NYQ:SLOPE -- first derivative of single channel
;;
(defun nyq:slope (s)
  (let* ((sr (snd-srate s))
         (sr-inverse (/ sr)))
    (snd-xform (snd-slope s) sr 0 sr-inverse MAX-STOP-TIME 1.0)))


;; lp - lowpass filter
;; 
(defun lp (s c)
  (multichan-expand "LP" #'nyq:lp
    '(((SOUND) "snd") ((NUMBER SOUND) "cutoff")) s c))

(setf lp-implementations
      (vector #'snd-tone #'snd-tonev))

;; NYQ:lp - lowpass filter, single channel
;;
(defun nyq:lp (s c)
  (select-implementation-1-1 "LP" lp-implementations s c))



;;; fixed-parameter filters based on snd-biquad
;;; note: snd-biquad is implemented in biquadfilt.[ch],
;;; while BiQuad.{cpp,h} is part of STK

(setf Pi 3.14159265358979)

(defun square (x) (* x x))
(defun sinh (x) (* 0.5 (- (exp x) (exp (- x)))))


; remember that snd-biquad uses the opposite sign convention for a_i's 
; than Matlab does.
; 
; Stability: Based on courses.cs.washington.edu/courses/cse490s/11au/
; Readings/Digital_Sound_Generation_2.pdf, the stable region is 
;   (a2 < 1) and ((a2 + 1) > |a1|)
; It doesn't look to me like our a0, a1, a2 match the paper's a0, a1, a2,
; and I'm not convinced the paper's derivation is correct, but at least
; the predicted region of stability is correct if we swap signs on a1 and
; a2 (but due to the |a1| term, only the sign of a2 matters). This was
; tested manually at a number of points inside and outside the stable
; triangle. Previously, the stability test was (>= a0 1.0) which seems
; generally wrong. The old test has been removed.

; convenient biquad: normalize a0, and use zero initial conditions.
(defun nyq:biquad (x b0 b1 b2 a0 a1 a2)
  (ny:typecheck (<= a0 0.0)
    (error (format nil "In BIQUAD, a0 < 0 (unstable parameter a0 = ~A)" a0)))
  (let ((a0r (/ (float a0))))
    (setf a1 (* a0r a1) 
          a2 (* a0r a2))
    (ny:typecheck (or (<= a2 -1.0) (<= (- 1.0 a2) (abs a1)))
        (error (format nil 
         "In BIQUAD, (a2 <= -1) or (1 - a2 <= |a1|) (~A a1 = ~A, a2 = ~A)"
         "unstable parameters" a1 a2)))
    (snd-biquad x (* a0r b0) (* a0r b1) (* a0r b2) 
                  a1 a2 0 0)))


(defun biquad (x b0 b1 b2 a0 a1 a2 &optional (source "BIQUAD"))
  (multichan-expand "BIQUAD" #'nyq:biquad
    '(((SOUND) "snd") ((NUMBER) "b0") ((NUMBER) "b1")
      ((NUMBER) "b2") ((NUMBER) "a0") ((NUMBER) "a1")
      ((NUMBER) "a2"))
    x b0 b1 b2 a0 a1 a2))


; biquad with Matlab sign conventions for a_i's.
(defun biquad-m (x b0 b1 b2 a0 a1 a2)
  (multichan-expand "BIQUAD-M" #'nyq:biquad-m
    '(((SOUND) "snd") ((NUMBER) "b0") ((NUMBER) "b1")
      ((NUMBER) "b2") ((NUMBER) "a0") ((NUMBER) "a1")
      ((NUMBER) "a2"))
    x b0 b1 b2 a0 a1 a2))

(defun nyq:biquad-m (x b0 b1 b2 a0 a1 a2 &optional (source "BIQUAD-M"))
  (nyq:biquad x b0 b1 b2 a0 (- a1) (- a2)))

; two-pole lowpass
(defun lowpass2 (x hz &optional (q 0.7071) (source "LOWPASS2"))
  (multichan-expand source #'nyq:lowpass2
    '(((SOUND) "snd") ((POSITIVE) "hz") ((POSITIVE) "q") ((STRING) "source"))
    x hz q source))

;; NYQ:LOWPASS2 -- operates on single channel
(defun nyq:lowpass2 (x hz q source)
  (if (or (> hz (* 0.5 (snd-srate x)))
          (< hz 0))
      (error "cutoff frequency out of range" hz))
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (alpha (* sw (sinh (/ 0.5 q))))
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cw))
         (a2 (- 1.0 alpha))
         (b1 (- 1.0 cw))
         (b0 (* 0.5 b1))
         (b2 b0))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 source)))

; two-pole highpass
(defun highpass2 (x hz &optional (q 0.7071) (source "HIGHPASS2"))
  (multichan-expand source #'nyq:highpass2
    '(((SOUND) "snd") ((POSITIVE) "hz") ((POSITIVE) "q") ((STRING) "source"))
    x hz q source))

(defun nyq:highpass2 (x hz q source)
  (if (or (> hz (* 0.5 (snd-srate x)))
          (< hz 0))
      (error "cutoff frequency out of range" hz))
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (alpha (* sw (sinh (/ 0.5 q))))
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cw))
         (a2 (- 1.0 alpha))
         (b1 (- -1.0 cw))
         (b0 (* -0.5 b1))
         (b2 b0))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 source)))

; two-pole bandpass.  max gain is unity.
(defun bandpass2 (x hz q)
  (multichan-expand "BANDPASS2" #'nyq:bandpass2
    '(((SOUND) "snd") ((POSITIVE) "hz") ((POSITIVE) "q"))
    x hz q))

(defun nyq:bandpass2 (x hz q)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (alpha (* sw (sinh (/ 0.5 q))))
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cw))
         (a2 (- 1.0 alpha))
         (b0 alpha)
         (b1 0.0)
         (b2 (- alpha)))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 "BANDPASS2")))

; two-pole notch.
(defun notch2 (x hz q)
  (multichan-expand "NOTCH2" #'nyq:notch2
    '(((SOUND) "snd") ((POSITIVE) "hz") ((POSITIVE) "q"))
    x hz q))

(defun nyq:notch2 (x hz q)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (alpha (* sw (sinh (/ 0.5 q))))
         (a0 (+ 1.0 alpha))
         (a1 (* -2.0 cw))
         (a2 (- 1.0 alpha))
         (b0 1.0)
         (b1 (* -2.0 cw))
         (b2 1.0))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 "NOTCH2")))


; two-pole allpass.
(defun allpass2 (x hz q)
  (multichan-expand "ALLPASS2" #'nyq:allpass
    '(((SOUND) "snd") ((POSITIVE) "hz") ((POSITIVE) "q"))
    x hz q))

(defun nyq:allpass (x hz q)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (k (exp (* -0.5 w (/ (float q)))))
         (a0 1.0)
         (a1 (* -2.0 cw k))
         (a2 (* k k))
         (b0 a2)
         (b1 a1)
         (b2 1.0))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2 "ALLPASS2")))


; bass shelving EQ.  gain in dB; Fc is halfway point.
; response becomes peaky at slope > 1.
(defun eq-lowshelf (x hz gain &optional (slope 1.0))
  (multichan-expand "EQ-LOWSHELF" #'nyq:eq-lowshelf
    '(((SOUND) "snd") ((POSITIVE) "hz") ((NUMBER) "gain") ((NUMBER) "slope"))
    x hz gain slope))


(defun nyq:eq-lowshelf (x hz gain slope)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (sw (sin w))
         (cw (cos w))
         (A (expt 10.0 (/ gain (* 2.0 20.0))))
         (b (sqrt (- (/ (+ 1.0 (square A)) slope) (square (- A 1.0)))))
         (apc (* cw (+ A 1.0)))
         (amc (* cw (- A 1.0)))
         (bs (* b sw))

         (b0 (*      A (+ A  1.0 (- amc)    bs  )))
         (b1 (*  2.0 A (+ A -1.0 (- apc)        )))
         (b2 (*      A (+ A  1.0 (- amc) (- bs) )))
         (a0           (+ A  1.0    amc     bs  ))
         (a1 (* -2.0   (+ A -1.0    apc         )))
         (a2           (+ A  1.0    amc  (- bs) )))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2)))


; treble shelving EQ.  gain in dB; Fc is halfway point.
; response becomes peaky at slope > 1.
(defun eq-highshelf (x hz gain &optional (slope 1.0))
  (multichan-expand "EQ-HIGHSHELF" #'nyq:eq-highshelf
    '(((SOUND) "snd") ((POSITIVE) "hz") ((NUMBER) "gain") ((NUMBER) "slope"))
    x hz gain slope))

(defun nyq:eq-highshelf (x hz gain slope)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (sw (sin w))
         (cw (cos w))
         (A (expt 10.0 (/ gain (* 2.0 20.0))))
         (b (sqrt (- (/ (+ 1.0 (square A)) slope) (square (- A 1.0)))))
         (apc (* cw (+ A 1.0)))
         (amc (* cw (- A 1.0)))
         (bs (* b sw))

         (b0 (*      A (+ A  1.0    amc     bs  )))
         (b1 (* -2.0 A (+ A -1.0    apc         )))
         (b2 (*      A (+ A  1.0    amc  (- bs) )))
         (a0           (+ A  1.0 (- amc)    bs  ))
         (a1 (*  2.0   (+ A -1.0 (- apc)        )))
         (a2           (+ A  1.0 (- amc) (- bs) )))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2)))
    
(defun nyq:eq-band (x hz gain width)
  (cond ((and (numberp hz) (numberp gain) (numberp width))
         (eq-band-ccc x hz gain width))
        ((and (soundp hz) (soundp gain) (soundp width))
         (snd-eqbandvvv x hz (db-to-linear gain) width))
        (t (error
            (strcat
             "In EQ-BAND, hz, gain, and width must be all numbers"
             " or all sounds (if any parameter is an array, there"
             " is a problem with at least one channel), hz is "
             (param-to-string hz) ", gain is " (param-to-string gain)
             ", width is " (param-to-string width)) )) ))

; midrange EQ.  gain in dB, width in octaves (half-gain width).
(defun eq-band (x hz gain width)
  (multichan-expand "EQ-BAND" #'nyq:eq-band
    '(((SOUND) "snd") ((POSITIVE SOUND) "hz")
      ((NUMBER SOUND) "gain") ((POSITIVE SOUND) "width"))
    x hz gain width))
  
  
(defun eq-band-ccc (x hz gain width)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (sw (sin w))
         (cw (cos w))
         (J (sqrt (expt 10.0 (/ gain 20.0))))
         ;(dummy (display "eq-band-ccc" gain J))
         (g (* sw (sinh (* 0.5 (log 2.0) width (/ w sw)))))
         ;(dummy2 (display "eq-band-ccc" width w sw g))
         (b0 (+ 1.0 (* g J)))
         (b1 (* -2.0 cw))
         (b2 (- 1.0 (* g J)))
         (a0 (+ 1.0 (/ g J)))
         (a1 (- b1))
         (a2 (- (/ g J) 1.0)))
    (biquad x b0 b1 b2 a0 a1 a2)))

; see failed attempt in eub-reject.lsp to do these with higher-order fns:

; four-pole Butterworth lowpass
(defun lowpass4 (x hz)
  (lowpass2 (lowpass2 x hz 0.60492333 "LOWPASS4") 
                        hz 1.33722126 "LOWPASS4"))

; six-pole Butterworth lowpass
(defun lowpass6 (x hz)
  (lowpass2 (lowpass2 (lowpass2 x hz 0.58338080 "LOWPASS6") 
                                  hz 0.75932572 "LOWPASS6") 
                                  hz 1.95302407 "LOWPASS6"))

; eight-pole Butterworth lowpass
(defun lowpass8 (x hz)
  (lowpass2 (lowpass2 (lowpass2 (lowpass2 x hz 0.57622191 "LOWPASS8")
                                            hz 0.66045510 "LOWPASS8") 
                                            hz 0.94276399 "LOWPASS8")
                                            hz 2.57900101 "LOWPASS8"))

; four-pole Butterworth highpass
(defun highpass4 (x hz)
  (highpass2 (highpass2 x hz 0.60492333 "HIGHPASS4") 
                          hz 1.33722126 "HIGHPASS4"))

; six-pole Butterworth highpass
(defun highpass6 (x hz)
  (highpass2 (highpass2 (highpass2 x hz 0.58338080 "HIGHPASS6") 
                                     hz 0.75932572 "HIGHPASS6")
                                     hz 1.95302407 "HIGHPASS6"))

; eight-pole Butterworth highpass
(defun highpass8 (x hz)
  (highpass2 (highpass2 (highpass2 (highpass2 x hz 0.57622191 "HIGHPASS8")
                                                hz 0.66045510 "HIGHPASS8")
                                                hz 0.94276399 "HIGHPASS8")
                                                hz 2.57900101 "HIGHPASS8"))

; YIN
; maybe this should handle multiple channels, etc.
(defun yin (sound minstep maxstep stepsize)
  (ny:typecheck (not (soundp sound))
    (ny:error "YIN" 1 '((SOUND) "sound") sound))
  (ny:typecheck (not (numberp minstep))
    (ny:error "YIN" 2 '((NUMBER) "minstep") minstep))
  (ny:typecheck (not (numberp maxstep))
    (ny:error "YIN" 3 '((NUMBER) "maxstep") maxstep))
  (ny:typecheck (not (integerp stepsize))
    (ny:error "YIN" 4 '((INTEGER) "stepsize") stepsize))
  (snd-yin sound minstep maxstep stepsize))


; FOLLOW
(defun follow (sound floor risetime falltime lookahead)
  (ny:typecheck (not (soundp sound))
    (ny:error "FOLLOW" 1 '((SOUND) "sound") sound))
  (ny:typecheck (not (numberp floor))
    (ny:error "FOLLOW" 2 '((NUMBER) "floor") floor))
  (ny:typecheck (not (numberp risetime))
    (ny:error "FOLLOW" 3 '((NUMBER) "risetime") risetime))
  (ny:typecheck (not (numberp falltime))
    (ny:error "FOLLOW" 4 '((NUMBER) "stepsize") falltime))
  (ny:typecheck (not (numberp lookahead))
    (ny:error "FOLLOW" 5 '((NUMBER) "lookahead") lookahead))
  ;; use 10000s as "infinite" -- that's about 2^30 samples at 96K
  (setf lookahead (round (* lookahead (snd-srate sound))))
  (extract (/ lookahead (snd-srate sound)) 10000
           (snd-follow sound floor risetime falltime lookahead)))


;; PHASE VOCODER
(defun phasevocoder (s map &optional (fftsize -1) (hopsize -1) (mode 0))
  (multichan-expand "PHASEVOCODER" #'snd-phasevocoder
    '(((SOUND) nil) ((SOUND) "map") ((INTEGER) "fftsize")
      ((INTEGER) "hopsize") ((INTEGER) "mode"))
    s map fftsize hopsize mode))


;; PV-TIME-PITCH
;; PV-TIME-PITCH -- control time stretch and transposition 
;;
;; stretchfn maps from input time to output time
;; pitchfn maps from input time to transposition factor (2 means octave up)
(defun pv-time-pitch (input stretchfn pitchfn dur &optional
                      (fftsize 2048) (hopsize nil) (mode 0))
  (multichan-expand "PV-TIME-PITCH" #'nyq:pv-time-pitch
    '(((SOUND) "input") ((SOUND) "stretchfn") ((SOUND) "pitchfn")
      ((NUMBER) "dur") ((INTEGER) "fftsize") ((INT-OR-NULL) "hopsize")
      ((INTEGER) "mode"))
    input stretchfn pitchfn dur fftsize hopsize mode))

(defun nyq:pv-time-pitch (input stretchfn pitchfn dur fftsize hopsize mode)
  (let (wrate u v w vinv)
    (if (null hopsize) (setf hopsize (/ fftsize 8)))
    (setf wrate (/ 3000  dur))
    (setf vinv (integrate (prod stretchfn  pitchfn)))
    (setf v (snd-inverse vinv (local-to-global 0) wrate))
    (setf w (integrate (snd-recip (snd-compose pitchfn v))))
    (sound-warp w (phasevocoder input v fftsize hopsize mode) wrate)))

