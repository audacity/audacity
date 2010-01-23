;(set-control-srate 100)
;(set-sound-srate 100)

;(setf xx (pwl 0 1  1 0 1.1 1 1.8 0 2 1 3 0 5))
;(setf xx (pwl 0 1  1 .2 1.1 1 1.8 .2 2 1 3 0 5))

;(setf yy (snd-follow xx 0.1 0.25 1.0 30))

;(setf db-factor (/ 1.0 (log 0.00001)))


; COMPRESS-MAP -- constructs a map for the compress function
;
; The map consists of two parts: a compression part and an expansion part.
; The intended use is to compress everything above compress-threshold by
; compress-ratio, and to downward expand everything below expand-ratio
; by expand-ratio.  Thresholds are in dB and ratios are dB-per-dB.
; 0dB corresponds to an amplitude of 1.0
; If the input goes above 0dB, the output can optionally be limited
; by seting limit-flag to T. This effectively changes the compression
; ratio to infinity at 0dB.  If limit-flag is NIL, then the compression-ratio
; continues to apply above 0dB.
; It is assumed that expand-threshold <= compress-threshold <= 0
; The gain is unity at 0dB so if compression-ratio > 1, then gain
; will be greater than unity below 0dB

;(defun compress-map (compress-ratio compress-threshold expand-ratio 
;		     expand-threshold limit-flag)
;  (let ()
;    (
;; I'm not sure if the rest of this function was lost due to version
;; problems, or it never existed. Email to rbd@cs.cmu.edu if you would
;; like some help with dynamics compression.
;;
;; Also, I had a really great 2-stage compressor for speech -- it did
;; something like a noise gate with a short time constant, and an automatic
;; gain control with a long time constant. Each one varied the gain by
;; about 12 dB -- any more would cause really ugly noise pumping, but
;; without the combined actions of both, there was not enough control.
;; Again, email me if you are interested.  Lately, I've been using
;; more sophisticated multiple band noise reduction in Cool Edit. They
;; obviously put a lot of work into that, and I don't plan to redo the
;; work for Nyquist. -RBD


(defun compress (input map rise-time fall-time)
  ; take the square of the input to get power
  (let ((in-squared (mult input input)))
    ; compute the time-average (sort of a low-pass) of the square
    (setf avg (snd-avg in-squared 1000 500 OP-AVERAGE))
    ; use follower to anticipate rise and trail off smoothly
    (setf env (snd-follow avg 0.001 0.2 1.0 20))
    ; take logarithm to get dB instead of linear
    (setf logenv (snd-log env))
    ; tricky part: map converts dB of input to desired gain in dB
    ; this defines the character of the compressor
    (setf shaped-env (shape logenv map 1.0))
    ; go back to linear
    (setf gain (snd-exp shaped-env))
    ; return the scaled input sound,
    ; another trick: avg signal will be delayed. Also, snd-follow
    ; has a delayed response because it's looking ahead in sound
    ; 20 = the number of samples of lookahead from snd-follow
    ; 88.2 = 44,100 (sample rate) / 500 (the step-size in avg)
    ; in other words, 44100/500 is the sample rate of the control
    ; signal looked at by follow
    ; "44100" should be replace by the signal's sample rate
    ; = (snd-srate input)
    (mult (seq (s-rest (/ 20.0 88.2)) (cue input)) gain)))

