;; dspprims.lsp -- interface to dsp primitives

;; ARESON - notch filter
;; 
(defun areson (s c b &optional (n 0))
  (multichan-expand #'nyq:areson s c b n))

(setf areson-implementations
      (vector #'snd-areson #'snd-aresonvc #'snd-aresoncv #'snd-aresonvv))

;; NYQ:ARESON - notch filter, single channel
;;
(defun nyq:areson (signal center bandwidth normalize)
  (select-implementation-1-2 areson-implementations 
   signal center bandwidth normalize))


;; hp - highpass filter
;; 
(defun hp (s c)
  (multichan-expand #'nyq:hp s c))

(setf hp-implementations
      (vector #'snd-atone #'snd-atonev))

;; NYQ:hp - highpass filter, single channel
;;
(defun nyq:hp (s c)
  (select-implementation-1-1 hp-implementations s c))


;; comb-delay-from-hz -- compute the delay argument
;;
(defun comb-delay-from-hz (hz caller)
  (recip hz))

;; comb-feedback-from-decay -- compute the feedback argument
;;
(defun comb-feedback (decay delay)
  (s-exp (mult -6.9087 delay (recip decay))))

;; COMB - comb filter
;; 
;; this is just a feedback-delay with different arguments
;;
(defun comb (snd decay hz)
  (multichan-expand #'nyq:comb snd decay hz))

(defun nyq:comb (snd decay hz)
  (let (delay feedback len d)
    ; convert decay to feedback, iterate over array if necessary
    (setf delay (comb-delay-from-hz hz "comb"))
    (setf feedback (comb-feedback decay delay))
    (nyq:feedback-delay snd delay feedback)))

;; ALPASS - all-pass filter
;; 
(defun alpass (snd decay hz &optional min-hz)
  (multichan-expand #'nyq:alpass snd decay hz min-hz))
  


(defun nyq:alpass (snd decay hz min-hz)
  (let (delay feedback len d)
    ; convert decay to feedback, iterate over array if necessary
    (setf delay (comb-delay-from-hz hz "alpass"))
    (setf feedback (comb-feedback decay delay))
    (nyq:alpass1 snd delay feedback min-hz)))


;; CONST -- a constant at control-srate
;;
(defun const (value &optional (dur 1.0))
  (let ((d (get-duration dur)))
    (snd-const value *rslt* *CONTROL-SRATE* d)))


;; CONVOLVE - slow convolution
;; 
(defun convolve (s r)
  (multichan-expand #'snd-convolve s r))


;; FEEDBACK-DELAY -- (delay is quantized to sample period)
;;
(defun feedback-delay (snd delay feedback)
  (multichan-expand #'nyq:feedback-delay snd delay feedback))
  

;; SND-DELAY-ERROR -- report type error
;;
(defun snd-delay-error (snd delay feedback)
  (error "feedback-delay with variable delay is not implemented"))


(setf feedback-delay-implementations
      (vector #'snd-delay #'snd-delay-error #'snd-delaycv #'snd-delay-error))


;; NYQ:FEEDBACK-DELAY -- single channel delay
;;
(defun nyq:feedback-delay (snd delay feedback)
  (select-implementation-1-2 feedback-delay-implementations 
                             snd delay feedback))


;; SND-ALPASS-ERROR -- report type error
;;
(defun snd-alpass-error (snd delay feedback)
  (error "alpass with constant decay and variable hz is not implemented"))


(if (not (fboundp 'snd-alpasscv))
    (defun snd-alpasscv (snd delay feedback min-hz)
      (error "snd-alpasscv (ALPASS with variable decay) is not implemented")))
(if (not (fboundp 'snd-alpassvv))
    (defun snd-alpassvv (snd delay feedback min-hz)
      (error "snd-alpassvv (ALPASS with variable decay and feedback) is not implemented")))
      

(defun nyq:alpassvv (the-snd delay feedback min-hz)
    (let (max-delay)
      (cond ((or (not (numberp min-hz))
                 (<= min-hz 0))
             (error "alpass needs numeric (>0) 4th parameter (min-hz) when delay is variable")))
      (setf max-delay (/ 1.0 min-hz))
      ; make sure delay is between 0 and max-delay
      ; use clip function, which is symetric, with an offset
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
  (select-implementation-1-2 alpass-implementations
                             snd delay feedback min-hz))

;; CONGEN -- contour generator, patterned after gated analog env gen
;;
(defun congen (gate rise fall) (multichan-expand #'snd-congen gate rise fall))


;; S-EXP -- exponentiate a sound
;;
(defun s-exp (s) (multichan-expand #'nyq:exp s))


;; NYQ:EXP -- exponentiate number or sound
;;
(defun nyq:exp (s) (if (soundp s) (snd-exp s) (exp s)))

;; S-ABS -- absolute value of a sound
;;
(defun s-abs (s) (multichan-expand #'nyq:abs s))

;; NYQ:ABS -- absolute value of number or sound
;;
(defun nyq:abs (s) (if (soundp s) (snd-abs s) (abs s)))

;; S-SQRT -- square root of a sound
;;
(defun s-sqrt (s) (multichan-expand #'nyq:sqrt s))

;; NYQ:SQRT -- square root of a number or sound
;;
(defun nyq:sqrt (s) (if (soundp s) (snd-sqrt s) (sqrt s)))


;; INTEGRATE -- integration
;;
(defun integrate (s) (multichan-expand #'snd-integrate s))


;; S-LOG -- natural log of a sound
;;
(defun s-log (s) (multichan-expand #'nyq:log s))


;; NYQ:LOG -- log of a number or sound
;;
(defun nyq:log (s) (if (soundp s) (snd-log s) (log s)))


;; NOISE -- white noise
;;
(defun noise (&optional (dur 1.0))
  (let ((d (get-duration dur)))
    (snd-white *rslt* *SOUND-SRATE* d)))


(defun noise-gate (snd &optional (lookahead 0.5) (risetime 0.02) (falltime 0.5)
                                                 (floor 0.01) (threshold 0.01))
  (let ((rms (lp (mult snd snd) (/ *control-srate* 10.0))))
    (setf threshold (* threshold threshold))
    (mult snd (gate rms floor risetime falltime lookahead threshold))))


;; QUANTIZE -- quantize a sound
;;
(defun quantize (s f) (multichan-expand #'snd-quantize s f))


;; RECIP -- reciprocal of a sound
;;
(defun recip (s) (multichan-expand #'nyq:recip s))


;; NYQ:RECIP -- reciprocal of a number or sound
;;
(defun nyq:recip (s) (if (soundp s) (snd-recip s) (/ (float s))))

;; RMS -- compute the RMS of a sound
;;
(defun rms (s &optional (rate 100.0) window-size)
  (let (rslt step-size)
    (cond ((not (eq (type-of s) 'SOUND))
	   (break "in RMS, first parameter must be a monophonic SOUND")))
    (setf step-size (round (/ (snd-srate s) rate)))
    (cond ((null window-size)
               (setf window-size step-size)))
    (setf s (prod s s))
    (setf result (snd-avg s window-size step-size OP-AVERAGE))
        ;; compute square root of average
        (s-exp (scale 0.5 (s-log result)))))


;; RESON - bandpass filter
;; 
(defun reson (s c b &optional (n 0))
  (multichan-expand #'nyq:reson s c b n))

(setf reson-implementations
      (vector #'snd-reson #'snd-resonvc #'snd-resoncv #'snd-resonvv))

;; NYQ:RESON - bandpass filter, single channel
;;
(defun nyq:reson (signal center bandwidth normalize)
  (select-implementation-1-2 reson-implementations 
   signal center bandwidth normalize))


;; SHAPE -- waveshaper
;;
(defun shape (snd shape origin)
  (multichan-expand #'snd-shape snd shape origin))


;; SLOPE -- calculate the first derivative of a signal
;;
(defun slope (s) (multichan-expand #'nyq:slope s))


;; NYQ:SLOPE -- first derivative of single channel
;;
(defun nyq:slope (s)
  (let* ((sr (snd-srate s))
         (sr-inverse (/ sr)))
    (snd-xform (snd-slope s) sr 0 sr-inverse MAX-STOP-TIME 1.0)))


;; lp - lowpass filter
;; 
(defun lp (s c)
  (multichan-expand #'nyq:lp s c))

(setf lp-implementations
      (vector #'snd-tone #'snd-tonev))

;; NYQ:lp - lowpass filter, single channel
;;
(defun nyq:lp (s c)
  (select-implementation-1-1 lp-implementations s c))



;;; fixed-parameter filters based on snd-biquad
;;; note: snd-biquad is implemented in biquadfilt.[ch],
;;; while BiQuad.{cpp,h} is part of STK

(setf Pi 3.14159265358979)

(defun square (x) (* x x))
(defun sinh (x) (* 0.5 (- (exp x) (exp (- x)))))


; remember that snd-biquad uses the opposite sign convention for a_i's 
; than Matlab does.

; convenient biquad: normalize a0, and use zero initial conditions.
; convenient biquad: normalize a0, and use zero initial conditions.
(defun nyq:biquad (x b0 b1 b2 a0 a1 a2)
  (if (<= a0 0.0)
      (error (format nil "a0 < 0 (unstable parameter a0 = ~A) in biquad~%" a0)))
  (let ((a0r (/ 1.0 a0)))
    (setf a1 (* a0r a1)
          a2 (* a0r a2))
    (if (or (<= a2 -1.0) (<= (- 1.0 a2) (abs a1)))
        (error (format nil
         "(a2 <= -1) or (1 - a2 <= |a1|) (~A a1 = ~A, a2 = ~A) in biquad~%"
         "unstable parameters" a1 a2)))
    (snd-biquad x (* a0r b0) (* a0r b1) (* a0r b2)
                  a1 a2 0 0)))


(defun biquad (x b0 b1 b2 a0 a1 a2)
  (multichan-expand #'nyq:biquad x b0 b1 b2 a0 a1 a2))


; biquad with Matlab sign conventions for a_i's.
(defun biquad-m (x b0 b1 b2 a0 a1 a2)
  (multichan-expand #'nyq:biquad-m x b0 b1 b2 a0 a1 a2))

(defun nyq:biquad-m (x b0 b1 b2 a0 a1 a2)
  (nyq:biquad x b0 b1 b2 a0 (- a1) (- a2)))

; two-pole lowpass
(defun lowpass2 (x hz &optional (q 0.7071))
  (multichan-expand #'nyq:lowpass2 x hz q))

;; NYQ:LOWPASS2 -- operates on single channel
(defun nyq:lowpass2 (x hz q)
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
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2)))

; two-pole highpass
(defun highpass2 (x hz &optional (q 0.7071))
  (multichan-expand #'nyq:highpass2 x hz q))

(defun nyq:highpass2 (x hz q)
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
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2)))

; two-pole bandpass.  max gain is unity.
(defun bandpass2 (x hz q)
  (multichan-expand #'nyq:bandpass2 x hz q))

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
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2)))

; two-pole notch.
(defun notch2 (x hz q)
  (multichan-expand #'nyq:notch2 x hz q))

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
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2)))


; two-pole allpass.
(defun allpass2 (x hz q)
  (multichan-expand #'nyq:allpass x hz q))

(defun nyq:allpass (x hz q)
  (let* ((w (* 2.0 Pi (/ hz (snd-srate x))))
         (cw (cos w))
         (sw (sin w))
         (k (exp (* -0.5 w (/ 1.0 q))))
         (a0 1.0)
         (a1 (* -2.0 cw k))
         (a2 (* k k))
         (b0 a2)
         (b1 a1)
         (b2 1.0))
    (nyq:biquad-m x b0 b1 b2 a0 a1 a2)))


; bass shelving EQ.  gain in dB; Fc is halfway point.
; response becomes peaky at slope > 1.
(defun eq-lowshelf (x hz gain &optional (slope 1.0))
  (multichan-expand #'nyq:eq-lowshelf x hz gain slope))

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
  (multichan-expand #'nyq:eq-highshelf x hz gain slope))

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
        (t
         (error "eq-band hz, gain, and width must be all numbers or all sounds"))))

; midrange EQ.  gain in dB, width in octaves (half-gain width).
(defun eq-band (x hz gain width)
  (multichan-expand #'nyq:eq-band x hz gain width))
  
  
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
  (lowpass2 (lowpass2 x hz 0.60492333) hz 1.33722126))

; six-pole Butterworth lowpass
(defun lowpass6 (x hz)
  (lowpass2 (lowpass2 (lowpass2 x hz 0.58338080) 
                                  hz 0.75932572) 
                                  hz 1.95302407))

; eight-pole Butterworth lowpass
(defun lowpass8 (x hz)
  (lowpass2 (lowpass2 (lowpass2 (lowpass2 x hz 0.57622191)
                                            hz 0.66045510) 
                                            hz 0.94276399)
                                            hz 2.57900101))

; four-pole Butterworth highpass
(defun highpass4 (x hz)
  (highpass2 (highpass2 x hz 0.60492333) hz 1.33722126))

; six-pole Butterworth highpass
(defun highpass6 (x hz)
  (highpass2 (highpass2 (highpass2 x hz 0.58338080) 
                                     hz 0.75932572) 
                                     hz 1.95302407))

; eight-pole Butterworth highpass
(defun highpass8 (x hz)
  (highpass2 (highpass2 (highpass2 (highpass2 x hz 0.57622191)
                                                hz 0.66045510) 
                                                hz 0.94276399)
                                                hz 2.57900101))

; YIN
; maybe this should handle multiple channels, etc.
(setfn yin snd-yin)


; FOLLOW
(defun follow (sound floor risetime falltime lookahead)
  ;; use 10000s as "infinite" -- that's about 2^30 samples at 96K
  (setf lookahead (round (* lookahead (snd-srate sound))))
  (extract (/ lookahead (snd-srate sound)) 10000
           (snd-follow sound floor risetime falltime lookahead)))

; Note: gate implementation moved to nyquist.lsp
;(defun gate (sound floor risetime falltime lookahead threshold)
;  (setf lookahead (round (* lookahead (snd-srate sound))))
;  (setf lookahead (/ lookahead (snd-srate sound)))
;  (extract lookahead 10000
;           (snd-gate sound lookahead risetime falltime floor threshold)))
