;;;
;;;   ###########################################################
;;;   ### NYQUIST-- A Language for Composition and Synthesis. ###
;;;   ###                                                     ###
;;;   ### Copyright (c) 1994-2006 by Roger B. Dannenberg      ###
;;;   ###########################################################
;;;
(load "fileio.lsp" :verbose NIL)

(prog ()
   (setq lppp -12.0) (setq lpp -9.0)  (setq lp -6.0)    (setq lmp -3.0)
   (setq lfff 12.0) (setq lff 9.0)  (setq lf 6.0)    (setq lmf 3.0)
   (setq dB0 1.00)  (setq dB1 1.122) (setq dB10 3.1623)

   (setq s 0.25) (setq sd 0.375) (setq st (/ 0.5 3.0))
   (setq i 0.5)  (setq id 0.75)  (setq it (* st 2.0))
   (setq q 1.0)  (setq qd 1.5)   (setq qt (* st 4.0))
   (setq h 2.0)  (setq hd 3.0)   (setq ht (* st 8.0))
   (setq w 4.0)  (setq wd 6.0)   (setq wt (* st 16.0))
)

(init-global *A4-Hertz* 440.0)

; next pitch, for initializations below
; 
(defun np () (incf nyq:next-pitch))

(defun set-pitch-names ()
   (setq no-pitch 116.0)
   ; note: 58.0 is A4 - (C0 - 1) = 69 - (12 - 1)
   (setf nyq:next-pitch (- (hz-to-step *A4-Hertz*) 58.0))

   (setf nyq:pitch-names
    '(c0 (cs0 df0) d0 (ds0 ef0) e0 f0 (fs0 gf0) g0 (gs0 af0) a0
      (as0 bf0) b0
      c1 (cs1 df1) d1 (ds1 ef1) e1 f1 (fs1 gf1) g1 (gs1 af1) a1
      (as1 bf1) b1
      c2 (cs2 df2) d2 (ds2 ef2) e2 f2 (fs2 gf2) g2 (gs2 af2) a2
      (as2 bf2) b2
      c3 (cs3 df3) d3 (ds3 ef3) e3 f3 (fs3 gf3) g3 (gs3 af3) a3
      (as3 bf3) b3
      c4 (cs4 df4) d4 (ds4 ef4) e4 f4 (fs4 gf4) g4 (gs4 af4) a4
      (as4 bf4) b4
      c5 (cs5 df5) d5 (ds5 ef5) e5 f5 (fs5 gf5) g5 (gs5 af5) a5
      (as5 bf5) b5
      c6 (cs6 df6) d6 (ds6 ef6) e6 f6 (fs6 gf6) g6 (gs6 af6) a6
      (as6 bf6) b6
      c7 (cs7 df7) d7 (ds7 ef7) e7 f7 (fs7 gf7) g7 (gs7 af7) a7
      (as7 bf7) b7))

   (dolist (p nyq:pitch-names)
     (cond ((atom p) (set p (np)))
       (t (let ((pitch (np)))
        (dolist (s p) (set s pitch)))))))


(set-pitch-names)

(init-global *default-sound-srate* 44100.0)
(init-global *default-control-srate* 2205.0)

(setf *environment-variables*
      '(*WARP* *SUSTAIN* *START* *LOUD* *TRANSPOSE* 
    *STOP* *CONTROL-SRATE* *SOUND-SRATE*))

(setfn environment-time car)
(setfn environment-stretch cadr)

; ENVIRONMENT-MAP - map virtual time using an environment
;
;(defun environment-map (env tim)
;  (+ (environment-time env)
;     (* (environment-stretch env) tim)))


(defun nyq:the-environment () (mapcar 'eval *environment-variables*))


;; GLOBAL ENVIRONMENT VARIABLES and their startup values:
(defun nyq:environment-init ()
  (setq *WARP*		'(0.0 1.0 nil))
  (setq *LOUD*	0.0)   ; now in dB
  (setq *TRANSPOSE*	0.0)
  (setq *SUSTAIN*	        1.0)
  (setq *START*       MIN-START-TIME)
  (setq *STOP*        MAX-STOP-TIME)
  (setq *CONTROL-SRATE*  *DEFAULT-CONTROL-SRATE*)
  (setq *SOUND-SRATE* *DEFAULT-SOUND-SRATE*)
  t)				; return nothing in particular

(nyq:environment-init)

(defun get-duration (dur)
  (let ((duration 
         (- (local-to-global (* (get-sustain) dur))
            (setf *rslt* (local-to-global 0)))))
     (cond ((minusp duration)
            (error
"duration is less than zero: perhaps a warp or stretch
is ill-formed. Nyquist cannot continue because synthesis
functions assume durations are always positive.")))
     duration))


(defun get-loud ()
  (cond ((numberp *loud*) *loud*)
    ((soundp *loud*)
     (sref *loud* 0))
    (t
     (error (format t "*LOUD* should be a number or sound: ~A" *LOUD*)))))


(defun get-sustain ()
  (cond ((numberp *SUSTAIN*) *SUSTAIN*)
    ((soundp *SUSTAIN*)
     ;(display "get-sustain: lookup " (local-to-global 0) 0))
     (sref *SUSTAIN* 0))
    (t
     (error (format t "*SUSTAIN* should be a number or sound: ~A" *SUSTAIN*)))))


(defun get-tempo ()
  (slope (snd-inverse (get-warp) (local-to-global 0)
              *control-srate*)))

(defun get-transpose ()
  (cond ((numberp *TRANSPOSE*) *TRANSPOSE*)
    ((soundp *TRANSPOSE*)
     ; (display "get-transpose: lookup " 0)
     ; (format t "samples: ~A~%" (snd-samples *TRANSPOSE* 100))
     (sref *TRANSPOSE* 0))
    (t
     (error (format t "*TRANSPOSE* should be a number or sound: ~A" *TRANSPOSE*)))))


(defun get-warp ()
  (let ((f (warp-function *WARP*)))
    (cond ((null f) (error "Null warp function"))
    (t
     (shift-time (scale-srate f (/ (warp-stretch *WARP*)))
             (- (warp-time *WARP*)))))))


;;;;;;;;;;;;;;;;;;;;;;
;; OSCILATORS
;;;;;;;;;;;;;;;;;;;;;;

(defun build-harmonic (n table-size) (snd-sine 0 n table-size 1))

(setf *SINE-TABLE* (list (build-harmonic 1 2048)
             (hz-to-step 1.0)
             T))
(setf *TABLE* *SINE-TABLE*)


(defun calculate-hz (pitch what)
  (let ((hz (step-to-hz (+ pitch (get-transpose))))
        (octaves 0))
    (cond ((> hz (/ *SOUND-SRATE* 2))
       (format t "Warning: ~A frequency (~A hz) will alias at current sample rate (~A hz).\n"
	       what hz *SOUND-SRATE*)
       (while (> hz *SOUND-SRATE*)
         (setf octaves (1+ octaves)
               hz (* hz 0.5)))
       (cond ((> octaves 0)
              (format t "Warning: ~A frequency reduced by ~A octaves to ~A hz (which will still alias).\n" 
		      what octaves hz)))))
    hz))


;; AMOSC
;;
(defun amosc (pitch modulation &optional (sound *table*) (phase 0.0))
  (let ((modulation-srate (snd-srate modulation))
        (hz (calculate-hz pitch "amosc")))
    (scale-db (get-loud)
      (snd-amosc
        (car sound)     ; samples for table
        (cadr sound)    ; step represented by table
        *SOUND-SRATE*   ; output sample rate
        hz              ;  output hz
        (local-to-global 0)	; starting time
        modulation      ; modulation
        phase))))       ; phase


;; FMOSC
;;
;; modulation rate must be less than or equal to sound-srate, so
;; force resampling and issue a warning if necessary. snd-fmosc can
;; handle upsampling cases internally.
;;
(defun fmosc (pitch modulation &optional (sound *table*) (phase 0.0))
  (let ((modulation-srate (snd-srate modulation))
        (hz (calculate-hz pitch "fmosc")))
    (scale-db (get-loud)
      (snd-fmosc 
        (car sound)         ; samples for table
        (cadr sound)        ; step represented by table
        *SOUND-SRATE*       ; output sample rate
        hz                  ;  output hz
        (local-to-global 0) ; starting time
        modulation          ; modulation
        phase))))           ; phase


;; FMFB
;;
;; this code is based on FMOSC above
;;
(defun fmfb (pitch index &optional dur)
 (let ((hz (calculate-hz pitch "fmfb")))
   (setf dur (get-duration dur))
   (cond ((soundp index) (ny:fmfbv hz index))
          (t
           (scale-db (get-loud)
                     (snd-fmfb (local-to-global 0) 
                               hz *SOUND-SRATE* index dur))))))

;; private variable index version of fmfb
(defun ny:fmfbv (hz index)
  (let ((modulation-srate (snd-srate index)))
    (cond ((< *SOUND-SRATE* modulation-srate)
           (format t "Warning: down-sampling FM modulation in fmfb~%")
           (setf index (snd-down *SOUND-SRATE* index))))
    (scale-db (get-loud)
              (snd-fmfbv (local-to-global 0) hz *SOUND-SRATE* index))))


;; BUZZ
;;
;; (ARGUMENTS ("long" "n") ("rate_type" "sr") ("double" "hz")
;;            ("time_type" "t0") ("sound_type" "s_fm"))
;; 
(defun buzz (n pitch modulation)
  (let ((modulation-srate (snd-srate modulation))
	(hz (calculate-hz pitch "buzz nominal")))
    (cond ((< *SOUND-SRATE* modulation-srate)
           (format t "Warning: down-sampling modulation in buzz~%")
           (setf modulation (snd-down *SOUND-SRATE* modulation))))
    (setf n (max n 1)) ; avoid divide by zero problem
    (scale-db (get-loud)
              (snd-buzz n                   ; number of harmonics
                        *SOUND-SRATE*       ; output sample rate
                        hz                  ; output hz
                        (local-to-global 0) ; starting time
                        modulation))))      ; freq. modulation
                        

;; (HZOSC hz [table [phase]])
;;
;; similar to FMOSC, but without "carrier" frequency parameter
;; also, hz may be a scalar or a sound
;;
(defun hzosc (hz &optional (sound *table*) (phase 0.0))
  (let (hz-srate)
    (cond ((numberp hz)
           (osc (hz-to-step hz) 1.0 sound phase))
          (t
           (setf hz-srate (snd-srate hz))
           (cond ((< *SOUND-SRATE* hz-srate)
                  (format t "Warning: down-sampling hz in hzosc~%")
                  (setf hz (snd-down *SOUND-SRATE* hz))))
           (scale-db (get-loud)
                     (snd-fmosc (car sound) ; samples for table
                                (cadr sound) ; step repr. by table
                                *SOUND-SRATE* ; output sample rate
                                0.0 ; dummy carrier
                                (local-to-global 0) ; starting time
                                hz phase))))))


;; (SIOSC-BREAKPOINTS tab0 t1 tab1 ... tn tabn)
;;   converts times to sample numbers
;; NOTE: time-warping the spectral envelope seems
;; like the wrong thing to do (wouldn't it be better
;; to warp the parameters that control the spectra,
;; or don't warp at all?). Nominally, a note should
;; have a "score" or local time duration equal to the
;; SUSTAIN environment variable. (When sustain is 1.0
;; and no time-warping is in effect, the duration is 1).
;; So, scale all times by
;;		(local-to-global (get-sustain))
;; so that if the final time tn = 1.0, we get a nominal
;; length note.

(defun siosc-breakpoints (breakpoints)
  (display "siosc-breakpoints" breakpoints)
  (prog (sample-count result (last-count 0) time-factor)
    (setf time-factor
      (- (local-to-global (get-sustain))
         (local-to-global 0.0)))
    (setf time-factor (* time-factor *SOUND-SRATE*))
    (cond ((and (listp breakpoints)
        (cdr breakpoints)
        (cddr breakpoints)))
      (t (error "SIOSC table list must have at least 3 elements")))
loop
    (cond ((and (listp breakpoints)
           (soundp (car breakpoints)))
       (push (car breakpoints) result)
       (setf breakpoints (cdr breakpoints)))
      (t
       (error "SIOSC expecting SOUND in table list")))
    (cond ((and breakpoints
        (listp breakpoints)
        (numberp (car breakpoints)))
       (setf sample-count (truncate
        (+ 0.5 (* time-factor (car breakpoints)))))
       (cond ((< sample-count last-count)
          (setf sample-count (1+ last-count))))
       (push sample-count result)
       (setf last-count sample-count)
       (setf breakpoints (cdr breakpoints))
       (cond (breakpoints
          (go loop))))
      (breakpoints
       (error "SIOSC expecting number (time) in table list")))
    (setf result (reverse result))
    (display "siosc-breakpoints" result)
    (return result)))

;; SIOSC -- spectral interpolation oscillator
;;
;; modulation rate must be less than or equal to sound-srate, so
;; force resampling and issue a warning if necessary. snd-fmosc can
;; handle upsampling cases internally.
;;
(defun siosc (pitch modulation breakpoints)
  (let ((modulation-srate (snd-srate modulation))
	(hz (calculate-hz pitch "siosc nominal")))
    (cond ((< *SOUND-SRATE* modulation-srate)
       (format t "Warning: down-sampling FM modulation in siosc~%")
       (setf modulation (snd-down *SOUND-SRATE* modulation))))
    (scale-db (get-loud)
	      (snd-siosc (siosc-breakpoints breakpoints) ; tables
			 *SOUND-SRATE*		; output sample rate
			 hz			;  output hz
			 (local-to-global 0)	; starting time
			 modulation))))		; modulation


;; LFO -- freq &optional duration sound phase)
;;
;; Default duration is 1.0 sec, default sound is *TABLE*, 
;; default phase is 0.0.
;;
(defun lfo (freq &optional (duration 1.0)
         (sound *SINE-TABLE*) (phase 0.0))
  (let ((d (get-duration duration)))
    (if (minusp d) (setf d 0))
    (cond ((> freq (/ *CONTROL-SRATE* 2))
           (format t "Warning: lfo frequency (~A hz) will alias at current control rate (~A hz).\n"
                     freq *CONTROL-SRATE*)))
    (set-logical-stop
      (snd-osc
        (car sound)		; samples for table
        (cadr sound)		; step represented by table
        *CONTROL-SRATE*		; output sample rate
        freq			; output hz
        *rslt*			; starting time
        d			; duration
        phase)		        ; phase
      duration)))


;; FMLFO -- like LFO but uses frequency modulation
;;
(defun fmlfo (freq &optional (sound *SINE-TABLE*) (phase 0.0))
  (let ()
    (cond ((numberp freq)
           (lfo freq 1.0 sound phase))
          ((soundp freq)
           (cond ((> (snd-srate freq) *CONTROL-SRATE*)
                  (setf freq (force-srate *CONTROL-SRATE* freq))))
           (snd-fmosc (car sound) (cadr sound) *CONTROL-SRATE* 0.0 
                      (local-to-global 0) freq phase))
          (t
           (error "frequency must be a number or sound")))))


;; OSC - table lookup oscillator
;;
(defun osc (pitch &optional (duration 1.0) 
            (sound *TABLE*) (phase 0.0))
  (let ((d  (get-duration duration))
	(hz (calculate-hz pitch "osc")))
    (set-logical-stop
      (scale-db (get-loud)
        (snd-osc 
          (car sound)		; samples for table
          (cadr sound)		; step represented by table
          *SOUND-SRATE*		; output sample rate
          hz			;  output hz
          *rslt*		; starting time
          d			; duration
          phase))               ; phase
      duration)))


;; PARTIAL -- sine osc with built-in envelope scaling
;;
(defun partial (steps env)
  (let ((hz (calculate-hz steps "partial")))
    (scale-db (get-loud)
      (snd-partial *sound-srate* hz
                   (force-srate *sound-srate* env)))))


;; SAMPLER -- simple attack + sustain sampler
;;
(defun sampler (pitch modulation 
                &optional (sample *table*) (npoints 2))
  (let ((samp (car sample))
        (samp-pitch (cadr sample))
        (samp-loop-start (caddr sample))
        (hz (calculate-hz pitch "sampler nominal")))
    ; make a waveform table look like a sample with no attack:
    (cond ((not (numberp samp-loop-start))
           (setf samp-loop-start 0.0)))
    (scale-db (get-loud)
       (snd-sampler 
        samp		; samples for table
        samp-pitch	; step represented by table
        samp-loop-start ; time to start loop
        *SOUND-SRATE*	; output sample rate
        hz		;  output hz
        (local-to-global 0)	; starting time
        modulation	; modulation
        npoints))))    	; number of interpolation points


;; SINE -- simple sine oscillator
;;
(defun sine (steps &optional (duration 1.0))
  (let ((hz (calculate-hz steps "sine"))
        (d (get-duration duration)))
    (set-logical-stop
      (scale-db (get-loud)
        (snd-sine *rslt* hz *sound-srate* d))
      duration)))


;; PLUCK
;;
;; (ARGUMENTS ("double" "sr") ("double" "hz") ("time_type" "t0") 
;;            ("time_type" "d") ("double" "final_amp"))
;;
(defun pluck (steps &optional (duration 1.0) (final-amp 0.001))
  (let ((hz (calculate-hz steps "pluck"))
        (d (get-duration duration)))
    (set-logical-stop
      (scale-db (get-loud)
        (snd-pluck *SOUND-SRATE* hz *rslt* d final-amp))
      duration)))


;; abs-env -- restore the standard environment
;;
(defmacro abs-env (s)
  `(progv '(*WARP* *LOUD* *TRANSPOSE* *SUSTAIN* 
            *START* *STOP*
            *CONTROL-SRATE* *SOUND-SRATE*)
          (list '(0.0 1.0 NIL) 0.0 0.0 1.0
           MIN-START-TIME MAX-STOP-TIME
           *DEFAULT-CONTROL-SRATE* *DEFAULT-SOUND-SRATE*)
     ,s))


; nyq:add2 - add two arguments
; 
(defun nyq:add2 (s1 s2)
  (cond ((and (arrayp s1) (not (arrayp s2)))
         (setf s2 (vector s2)))
        ((and (arrayp s2) (not (arrayp s1)))
         (setf s1 (vector s1))))
  (cond ((arrayp s1)
         (sum-of-arrays s1 s2))
        (t
         (nyq:add-2-sounds s1 s2))))


; (NYQ:ADD-2-SOUNDS S1 S2) - add two sound (or number) arguments
; 
(defun nyq:add-2-sounds (s1 s2)
  (cond ((numberp s1)
         (cond ((numberp s2)
        (+ s1 s2))
          (t
           (snd-offset s2 s1))))
    ((numberp s2)
     (snd-offset s1 s2))
    (t
     (let ((s1sr (snd-srate s1))
           (s2sr (snd-srate s2)))
;    (display "nyq:add-2-sounds" s1sr s2sr)
       (cond ((> s1sr s2sr)
              (snd-add s1 (snd-up s1sr s2)))
             ((< s1sr s2sr)
              (snd-add (snd-up s2sr s1) s2))
             (t
              (snd-add s1 s2)))))))


(defmacro at (x s)
 `(progv '(*WARP*) (list (list (+ (warp-time *WARP*) 
                  (* (warp-stretch *WARP*) ,x))
                   (warp-stretch *WARP*)
                   (warp-function *WARP*)))
      ,s))


;; (AT-ABS t behavior) evaluate behavior at global time t
;;
;; *WARP* is the triple (d s f) denoting the function f(st+d),
;; a mapping from local to global time.
;; We want (d' s f) such that f(s*0 + d') = t
;; (Note that we keep the same s and f, and only change the offset.
;; To eliminate the warp and stretch use "(abs-env (at t behavior))")
;; Applying the inverse of f, d' = f-1(t), or (sref (snd-inverse f ...) t)
;; Rather than invert the entire function just to evaluate at one point,
;; we use SREF-INVERSE to find d'.
;;
(defmacro at-abs (x s)
 `(progv '(*WARP*)
         (if (warp-function *WARP*)
             (list (list (sref-inverse (warp-function *WARP*) ,x)
                         (warp-stretch *WARP*)
                         (warp-function *WARP*)))
             (list (list ,x (warp-stretch *WARP*) NIL)))
    ;; issue warning if sound starts in the past
    (check-t0 ,s ',s)))

(defun check-t0 (s src)
  (let (flag t0 (now (local-to-global 0)))
    (cond ((arrayp s)
           (dotimes (i (length s))
             (setf t0 (snd-t0 (aref s i))))
             (if (< t0 now) (setf flag t0)))
          (t
           (setf t0 (snd-t0 s))
           (if (< t0 now) (setf flag t0))))
    (if flag
        (format t "Warning: cannot go back in time to ~A, sound came from ~A~%"
                  flag src))
    ; (display "check-t0" t0 now src)
    ; return s whether or not warning was reported
    s))

;; (CLIP S1 VALUE) - clip maximum amplitude to value
;
(defun clip (x v)
  (cond ((numberp x)
     (max (min x v) (- v)))
    ((arrayp x)
     (let* ((len (length x))
        (result (make-array len)))
        (dotimes (i len)
        (setf (aref result i) 
              (snd-clip (aref x i) v)))
        result))
    (t
     (snd-clip x v))))


;; (NYQ:COERCE-TO S1 S2) - expand sound s1 to type of s2
; 
(defun nyq:coerce-to (s1 s2)
  (cond ((or (soundp s1) (numberp s1))
         (cond ((arrayp s2)
                (nyq:sound-to-array s1 (length s2)))
               (t s1)))
         (t s1)))


(defmacro continuous-control-warp (beh)
  `(snd-compose (warp-abs nil ,beh)
        (snd-inverse (get-warp)
         (local-to-global 0) *control-srate*)))

(defmacro continuous-sound-warp (beh)
  `(snd-compose (warp-abs nil ,beh)
        (snd-inverse (get-warp)
         (local-to-global 0) *sound-srate*)))


(defmacro control-srate-abs (r s)
  `(progv '(*CONTROL-SRATE*) (list ,r)
      ,s))

; db = 20log(ratio)
; db = 20 ln(ratio)/ln(10)
; db/20 = ln(ratio)/ln(10)
; db ln(10)/20 = ln(ratio)
; e^(db ln(10)/20) = ratio
;
(setf ln10over20 (/ (log 10.0) 20))

(defun db-to-linear (x) 
  (cond ((numberp x)
     (exp (* ln10over20 x)))
    ((arrayp x)
     (let* ((len (length x))
        (result (make-array len)))
        (dotimes (i len)
        (setf (aref result i) 
              (snd-exp (snd-scale ln10over20 (aref x i)))))
        result))
    (t
     (snd-exp (snd-scale ln10over20 x)))))


(defun linear-to-db (x) 
  (cond ((numberp x)
     (/ (log (float x)) ln10over20))
    ((arrayp x)
     (let* ((len (length x))
        (result (make-array len)))
        (dotimes (i len)
        (setf (aref result i) 
              (snd-scale (/ 1.0 ln10over20) (snd-log (aref x i)))))
        result))
    (t
     (snd-scale (/ 1.0 ln10over20) (snd-log x)))))


(cond ((not (fboundp 'scalar-step-to-hz))
       (setfn scalar-step-to-hz step-to-hz)
       (setfn scalar-hz-to-step hz-to-step)))


(defun step-to-hz (x)
  (cond ((numberp x)
         (scalar-step-to-hz x))
        ((arrayp x)
         (let* ((len (length x))
                (result (make-array len)))
           (dotimes (i len)
             (setf (aref result i) (step-to-hz (aref x i))))
           result))
        (t
         (s-exp (snd-offset (snd-scale 0.0577622650466621 x) 
                            2.1011784386926213)))))

(defun hz-to-step (x)
  (cond ((numberp x)
         (scalar-hz-to-step x))
        ((arrayp x)
         (let* ((len (length x))
                (result (make-array len)))
           (dotimes (i len)
             (setf (aref result i) (hz-to-step (aref x i))))
           result))
        (t
         (snd-scale 17.312340490667565
                    (snd-offset (s-log x) -2.1011784386926213))))) 


; sref - access a sound at a given time point
;    note that the time is transformed to global
(defun sref (sound point)
  (snd-sref sound (local-to-global point)))


; extract - start is stretched and shifted as is stop
;  result is shifted to start at local time zero
(defun extract (start stop sound)
  (extract-abs (local-to-global start) (local-to-global stop) sound
               (local-to-global 0)))

; extract-abs - return sound between start and stop
;  start-time is optional (to aid the implementation of
;  extract) and gives the start time of the result, normally 0.
;  There is a problem if sound t0 is not equal to start-time.
;  E.g. if sound was created with AT, its t0 might be
;  in the future, but snd-xform works by first shifting
;  t0 to local time zero, so we need to be very careful.
;  The solution is that if t0 > start_time, subtract the difference
;  from start and stop to shift them appropriately.
(defun extract-abs (start stop sound &optional (start-time 0))
  (let ((t0 (snd-t0 sound)) offset)
    (cond ((/= t0 start-time)
           (setf offset (- t0 start-time))
           (setf start (- start offset))
           (setf stop (- stop offset))))
    (snd-xform sound (snd-srate sound) start-time start stop 1.0)))
     

(defun local-to-global (local-time)
  (let ((d (warp-time *WARP*))
    (s (warp-stretch *WARP*))
    (w (warp-function *WARP*))
    global-time)
    (setf global-time (+ (* s local-time) d))
    (if w (snd-sref w global-time) global-time)))


(defmacro loud (x s)
 `(progv '(*LOUD*) (list (sum *LOUD* ,x))
     ,s))


(defmacro loud-abs (x s)
 `(progv '(*LOUD*) (list ,x)
     ,s))

(defun must-be-sound (x)
 (cond ((soundp x) x)
       (t
    (error "SOUND type expected" x))))

;; SCALE-DB -- same as scale, but argument is in db
;;
(defun scale-db (factor sound)
  (scale (db-to-linear factor) sound))

(defun set-control-srate (rate)
  (setf *default-control-srate* (float rate))
  (nyq:environment-init))

(defun set-sound-srate (rate) 
  (setf *default-sound-srate* (float rate))
  (nyq:environment-init))


; s-plot -- compute and write n data points for plotting
;
; dur is how many seconds of sound to plot. If necessary, cut the
;     sample rate to allow plotting dur seconds
; n is the number of points to plot. If there are more than n points,
;     cut the sample rate. If there are fewer than n samples, just
;     plot the points that exist.
;
(defun s-plot (snd &optional (dur 2.0) (n 1000))
  (prog* ((sr (snd-srate snd))
          (t0 (snd-t0 snd))
          (filename (soundfilename *default-plot-file*))
          (s snd) ;; s is either snd or resampled copy of snd
          (outf (open filename :direction :output)) ;; for plot data
          (maximum -1000000.0) ;; maximum amplitude
          (minimum  1000000.0) ;; minimum amplitude
          actual-dur ;; is the actual-duration of snd
          sample-count ;; is how many samples to get from s
          period  ;; is the period of samples to be plotted
          truncation-flag     ;; true if we didn't get whole sound
          points) ;; is array of samples
     ;; If we need more than n samples to get dur seconds, resample
     (cond ((< n (* dur sr))
            (setf s (force-srate (/ (float n) dur) snd))))
     ;; Get samples from the signal
     (setf points (snd-samples s (1+ n)))
     ;; If we got fewer than n points, we can at least estimate the
     ;; actual duration (we might not know exactly if we use a lowered
     ;; sample rate). If the actual sample rate was lowered to avoid
     ;; getting more than n samples, we can now raise the sample rate
     ;; based on our estimate of the actual sample duration.
     (display "test" (length points) n)
     (cond ((< (length points) n)
            ;; sound is shorter than dur, estimate actual length
            (setf actual-dur (/ (length points) (snd-srate s)))
            (setf sample-count (round (min n (* actual-dur sr))))
            (cond ((< n (* actual-dur sr))
                   (setf s (force-srate (/ (float n) actual-dur) snd)))
                  (t ;; we can use original signal
                   (setf s snd)))
            (setf points (snd-samples s sample-count))
            ;; due to rounding, need to recalculate exact count
            (setf sample-count (length points)))
           ((= (length points) n)
            (setf actual-dur dur)
            (setf sample-count n))
           (t ;; greater than n points, so we must have truncated sound
            (setf actual-dur dur)
            (setf sample-count n)
            (setf truncation-flag t)))
     ;; actual-dur is the duration of the plot
     ;; sample-count is how many samples we have
     (setf period (/ 1.0 (snd-srate s)))
     (cond ((null outf)
            (format t "s-plot: could not open ~A!~%" filename)
            (return nil)))
    (format t "s-plot: writing ~A ... ~%" filename)
    (cond (truncation-flag
           (format t "        !!TRUNCATING SOUND TO ~As\n" actual-dur)))
    (cond ((/= (snd-srate s) (snd-srate snd))
           (format t "        !!RESAMPLING SOUND FROM ~A to ~Ahz\n"
                   (snd-srate snd) (snd-srate s))))
    (cond (truncation-flag
           (format t "        Plotting ~As, actual sound duration is greater\n"
                     actual-dur))
          (t
           (format t "        Sound duration is ~As~%" actual-dur)))
    (dotimes (i sample-count)
      (setf maximum (max maximum (aref points i)))
      (setf minimum (min minimum (aref points i)))
      (format outf "~A ~A~%" (+ t0 (* i period)) (aref points i)))
    (close outf)
    (format t "        Wrote ~A points from ~As to ~As~%" 
              sample-count t0 (+ t0 actual-dur))
    (format t "        Range of values ~A to ~A\n" minimum maximum)
    (cond ((or (< minimum -1) (> maximum 1))
           (format t "        !!SIGNAL EXCEEDS +/-1~%")))))


; run something like this to plot the points:
; graph < points.dat | plot -Ttek


(defmacro sound-srate-abs (r s)
  `(progv '(*SOUND-SRATE*) (list ,r)
      ,s))


(defmacro stretch (x s)
 `(progv '(*WARP*) (list (list (warp-time *WARP*) 
                   (* (warp-stretch *WARP*) ,x)
                   (warp-function *WARP*)))
     (if (minusp (warp-stretch *WARP*))
         (break "Negative stretch factor is not allowed"))
             ,s))

         
(defmacro stretch-abs (x s)
 `(progv '(*WARP*) (list (list (local-to-global 0)
                   ,x
                   nil))
     (if (minusp (warp-stretch *WARP*))
         (break "Negative stretch factor is not allowed"))
             ,s))


(defmacro sustain (x s)
 `(progv '(*SUSTAIN*) (list (prod *SUSTAIN* ,x))
      ,s))


(defmacro sustain-abs (x s)
 `(progv '(*SUSTAIN*) (list ,x)
      ,s))


;; (WARP-FUNCTION *WARP*) - extracts function field of warp triple
;;
(setfn warp-function caddr)


;; (WARP-STRETCH *WARP*) - extracts stretch field of warp triple
;;
(setfn warp-stretch cadr)


;; (WARP-TIME *WARP*) - extracts time field of warp triple
;;
(setfn warp-time car)


(defmacro transpose (x s)
 `(progv '(*TRANSPOSE*) (list (sum *TRANSPOSE* ,x))
      ,s))


(defmacro transpose-abs (x s)
 `(progv '(*TRANSPOSE*) (list ,x)
      ,s))


;; COMPUTE-DEFAULT-SOUND-FILE -- construct and set *default-sound-file*
;;
;; (this is harder than it might seem because the default place for
;;  sound files is in /tmp, which is shared by users, so we'd like to
;;  use a user-specific name to avoid collisions)
;;
(defun compute-default-sound-file () 
  (let (inf user extension)
      ; the reason for the user name is that if UserA creates a temp file,
      ; then UserB will not be able to overwrite it. The user name is a
      ; way to give each user a unique temp file name. Note that we don't
      ; want each session to generate a unique name because Nyquist doesn't
      ; delete the sound file at the end of the session.
   (setf user (get-user))
#|
   (cond ((null user)           
       (format t 
"Please type your user-id so that I can construct a default 
sound-file name.  To avoid this message in the future, add
this to your .login file:
    setenv USER <your id here>
or add this to your init.lsp file:
    (setf *default-sound-file* \"<your filename here>\")
    (setf *default-sf-dir* \"<full pathname of desired directory here>\")

Your id please: ")
       (setf user (read))))
|#
    ; now compute the extension based on *default-sf-format*
    (cond ((= *default-sf-format* snd-head-AIFF)
           (setf extension ".aif"))
          ((= *default-sf-format* snd-head-Wave)
           (setf extension ".wav"))
          (t
           (setf extension ".snd")))
    (setf *default-sound-file* 
      (strcat (string-downcase user) "-temp" extension))
    (format t "Default sound file is ~A.~%" *default-sound-file*)))


;; CONTROL-WARP -- apply a warp function to a control function
;; 
(defun control-warp (warp-fn control &optional wrate)
  (cond (wrate
     (snd-resamplev control *control-srate*
            (snd-inverse warp-fn (local-to-global 0) wrate)))
    (t
     (snd-compose control
              (snd-inverse warp-fn (local-to-global 0) *control-srate*)))))


;; (cue sound)
;;    Cues the given sound; that is, it applies the current *WARP*, *LOUD*,
;; *START*, and *STOP* values to the argument.  The logical start time is at
;; local time 0.
(defun cue (sound)
  (cond ((arrayp sound)
     (let* ((len (length sound))
        (result (make-array len)))
        (dotimes (i len)
        (setf (aref result i)
              (cue-sound (aref sound i))))
        result))
    (t
     (cue-sound sound))))

(defun cue-sound (sound)
  (snd-xform sound
         (snd-srate sound)
         (local-to-global 0) *START* *STOP* (db-to-linear (get-loud))))

;; (sound sound)
;;    Same as (cue sound), except also warps the sound.
;; Note that the *WARP* can change the pitch of the
;; sound as a result of resampling.
;; Here's the derivation for the warping code:
;; *WARP* is a triple: (d s f) which denotes that the warp from local to
;; global time is: f(st+d)
;; We need to compose sound with the inverse of this to get a function
;; of global time
;; Let f-1 be the inverse of f.  Then the inverse of f(st+d) is 
;; (f-1(t) - d)/s
;; The composition gives us: (snd-compose sound (f-1(t) - d)/s)
;; Eliminate the 1/s term by changing the sample rate of sound:
;;  = (snd-compose (snd-scale-srate sound s) (f-1(t) - d))
;; Eliminate the -d term by shifting f before taking the inverse:
;;  = (snd-compose (scale-srate sound s) ((inverse f) - d))
;;  = (snd-compose (scale-srate sound s) (inverse f(t + d)))
;;  = (snd-compose (scale-srate sound s) (inverse (shift f -d)))
;; snd-inverse takes a time and sample rate.  For time, use zero.
;; The sample rate of inverse determines the final sample rate of
;; this function, so use *SOUND-SRATE*:
;;  = (snd-compose (scale-srate sound s) (snd-inverse (shift-time f (- d))
;;                                              0 *SOUND-SRATE*))
;;
(defun nyq:sound (sound)
   (cond ((null (warp-function *WARP*))
      (snd-xform sound (/ (snd-srate sound) (warp-stretch *WARP*))
             (local-to-global 0)
             *START* *STOP* (db-to-linear (get-loud))))
     (t
      (snd-compose (scale-srate sound (warp-stretch *WARP*))
               (snd-inverse (shift-time (warp-function *WARP*)
                        (- (warp-time *WARP*)))
                    0 *SOUND-SRATE*)))))

(defun nyq:sound-of-array (sound)
  (let* ((n (length sound))
         (s (make-array n)))
    (dotimes (i n)
      (setf (aref s i) (nyq:sound (aref sound i))))
    s))


(defun sound (sound)
  (cond ((arrayp sound)
     (nyq:sound-of-array sound))
    (t
     (nyq:sound sound))))


;; (SCALE-SRATE SOUND SCALE)
;; multiplies the sample rate by scale
(defun scale-srate (sound scale)
  (let ((new-srate (* scale (snd-srate sound))))
    (snd-xform sound new-srate (snd-time sound) 
           MIN-START-TIME MAX-STOP-TIME 1.0)))


;; (SHIFT-TIME SOUND SHIFT)
;; shift the time of a function by SHIFT, i.e. if SOUND is f(t),
;; then (shift-time SOUND SHIFT) is f(t - SHIFT).  Note that if
;; you look at plots, the shifted sound will move *right* when SHIFT
;; is positive.  
(defun shift-time (sound shift)
  (snd-xform sound (snd-srate sound) (+ (snd-t0 sound) shift)
         MIN-START-TIME MAX-STOP-TIME 1.0))


;; (NYQ:SOUND-TO-ARRAY SOUND N) - duplicate SOUND to N channels
;;
(defun nyq:sound-to-array (sound n)
  (let ((result (make-array n)))
    (dotimes (i n)
      (setf (aref result i) sound))
    result))


;; (control sound)
;;    Same as (sound sound), except this is used for control signals.  
;;    This code is identical to sound.
(setfn control sound)


;; (cue-file string)
;;    Loads a sound file with the given name, returning a sound which is
;; transformed to the current environment.
(defun cue-file (name)
    (cue (force-srate *SOUND-SRATE* (s-read name))))


;; (env t1 t2 t4 l1 l2 l3 &optional duration)
;; Creates a 4-phase envelope.
;;	tN is the duration of phase N, and lN is the final level of
;;	phase N.  t3 is implied by the duration, and l4 is 0.0.
;;	If dur is not supplied, then 1.0 is assumed.  The envelope
;;	duration is the product of dur, *STRETCH*, and *SUSTAIN*.  If 
;;	t1 + t2 + 2ms + t4 > duration, then a two-phase envelope is
;;	substituted that has an attack/release time ratio = t1/t4.
;;	The sample rate of the returned sound is *CONTROL-SRATE*.
;;
;; Time transformation: the envelope is not warped; the start time and
;; stop times are warped to global time.  Then the value of *SUSTAIN* at
;; the begining of the envelope is used to determing absolute duration.
;; Since PWL is ultimately called to create the envelope, we must use
;; ABS-ENV to prevent any further transforms inside PWL.  We use
;; (AT global-start ...) inside ABS-ENV so that the final result has 
;; the proper starting time.
;;
(defun env (t1 t2 t4 l1 l2 l3 &optional (duration 1.0))
  (let (actual-dur min-dur ratio t3
    (actual-dur (get-duration duration)))
    (setf min-dur (+ t1 t2 t4 0.002))
    (cond ((< actual-dur min-dur)
       (setf ratio (/ t1 (float (+ t1 t4))))
       (setf t1 (* ratio actual-dur))
       (setf t2 (- actual-dur t1))
       (setf t3 0.0)
       (setf t4 0.0)
       (setf l2 0.0)
       (setf l3 0.0))
      (t
       (setf t3 (- actual-dur t1 t2 t4))))
    (set-logical-stop
      (abs-env (at *rslt*
                   (pwl t1 l1 (+ t1 t2) l2 (- actual-dur t4) l3 actual-dur)))
      duration)))


(defun gate (sound lookahead risetime falltime floor threshold)
    (cond ((< lookahead risetime)
           (break "lookahead must be greater than risetime in GATE function"))
          ((or (< risetime 0) (< falltime 0) (< floor 0))
           (break "risetime, falltime, and floor must all be positive in GATE function"))
          (t
           (let ((s
              (snd-gate (seq (cue sound) (abs-env (s-rest lookahead)))
                    lookahead risetime falltime floor threshold)))
             (snd-xform s (snd-srate s) (snd-t0 sound) 
            (+ (snd-t0 sound) lookahead) MAX-STOP-TIME 1.0)))))


;; (osc-note step &optional duration env sust volume sound)
;;   Creates a note using table-lookup osc, but with an envelope.
;; The ENV parameter may be a parameter list for the env function,
;; or it may be a sound.
;;
(defun osc-note (pitch &optional (duration 1.0) 
               (env-spec '(0.02 0.1 0.3 1.0 .8 .7))
               (volume 0.0)
               (table *TABLE*))
  (set-logical-stop
   (mult (loud volume (osc pitch duration table))
     (if (listp env-spec)
       (apply 'env env-spec)
       env-spec))
   duration))


;; force-srate -- resample snd if necessary to get sample rate
;
(defun force-srate (sr snd)
  (cond ((not (numberp sr))
     (error "force-srate: SR should be a number")))
  (cond ((arrayp snd)
     (let* ((len (length snd))
        (result (make-array len)))
       (dotimes (i len)
            (setf (aref result i) 
              (force-srate sr (aref snd i))))
       result))
    (t
     (let ((snd-sr (snd-srate snd)))
       (cond ((> sr snd-sr) (snd-up sr snd))
         ((< sr snd-sr) (snd-down sr snd))
         (t snd))))))


(defun force-srates (srs snd)
  (cond ((and (numberp srs) (soundp snd))
     (force-srate srs snd))
    ((and (arrayp srs) (arrayp snd))
     (let* ((len (length snd))
        (result (make-array len)))
       (dotimes (i len)
            (setf (aref result i) 
              (force-srate (aref srs i) (aref snd i))))
       result))
    (t (error "arguments not compatible"))))


;; (breakpoints-convert (t1 x1 t2 x2 ... tn) t0)
;;   converts times to sample numbers and scales amplitudes
;;   t0 is the global (after warping) start time
;;
;; NOTE: there were some stack overflow problems with the original
;; recursive version (in comments now), so it was rewritten as an
;; iteration.
;;
(defun breakpoints-convert (list t0)
  (prog (sample-count result sust (last-count 0))
    (setf sust (get-sustain))
 loop
    (setf sample-count 
      (truncate (+ 0.5 (* (- (local-to-global (* (car list) sust)) t0)
                 *control-srate*))))
    ; now we have a new sample count to put into result list
    ; make sure result is non-decreasing
    (cond ((< sample-count last-count)
       (setf sample-count last-count)))
    (setf last-count sample-count)
    (push sample-count result)
    (cond ((cdr list)
       (setf list (cdr list))
       (push (float (car list)) result)))
    (setf list (cdr list))
    (cond (list
       (go loop)))
    (return (reverse result))))

      
 
;; (pwl t1 l1 t2 l2 ... tn)
;;   Creates a piece-wise linear envelope from breakpoint data.
;;
(defun pwl (&rest breakpoints) (pwl-list breakpoints))

(defun pwlr (&rest breakpoints) (pwlr-list breakpoints))

;; (breakpoints-relative list)
;;  converts list, which has the form (value dur value dur value ...)
;;  into the form (value time value time value ...)
;;  the list may have an even or odd length
;;
(defun breakpoints-relative (breakpoints)
  (prog (result (sum 0.0))
 loop
     (cond (breakpoints
        (push (car breakpoints) result)
        (setf breakpoints (cdr breakpoints))
        (cond (breakpoints
           (setf sum (+ sum (car breakpoints)))
           (push sum result)
           (setf breakpoints (cdr breakpoints))
           (go loop)))))
     (return (reverse result))))


(defun breakpoints-relative (breakpoints)
  (prog (result (sum 0.0))
 loop
    (setf sum (+ sum (car breakpoints)))
    (push sum result)
    (cond ((cdr breakpoints)
       (setf breakpoints (cdr breakpoints))
       (push (car breakpoints) result)))
    (setf breakpoints (cdr breakpoints))
    (cond (breakpoints
       (go loop)))
    (return (reverse result))))


(defun pwlr-list (breakpoints)
  (pwl-list (breakpoints-relative breakpoints)))

(defun pwl-list (breakpoints)
  (let ((t0 (local-to-global 0)))
    (snd-pwl t0 *control-srate* (breakpoints-convert breakpoints t0))))

;; (pwlv l1 t1 l2 t2 ... ln)
;; Creates a piece-wise linear envelope from breakpoint data;
;; the function initial and final values are explicit
;;
(defun pwlv (&rest breakpoints)
  ;use pwl, modify breakpoints with initial and final changes
  ;need to put initial time of 0, and final time of 0
  (pwlv-list breakpoints))

(defun pwlv-list (breakpoints)
    (pwl-list (cons 0.0 (append breakpoints '(0.0)))))

(defun pwlvr (&rest breakpoints) (pwlvr-list breakpoints))

(defun pwlvr-list (breakpoints)
  (pwlr-list (cons 0.0 (append breakpoints '(0.0)))))

(defun pwe (&rest breakpoints)
  (pwe-list breakpoints))

(defun pwe-list (breakpoints)
  (pwev-list (cons 1.0 (append breakpoints '(1.0)))))

(defun pwer (&rest breakpoints) (pwer-list breakpoints))

(defun pwer-list (breakpoints)
  (pwe-list (breakpoints-relative breakpoints)))

(defun pwev (&rest breakpoints)
  (pwev-list breakpoints))

(defun pwev-list (breakpoints)
  (let ((lis (breakpoints-log breakpoints)))
    (s-exp (pwl-list lis))))

(defun pwevr (&rest breakpoints) (pwevr-list breakpoints))

(defun pwevr-list (breakpoints)
  (pwev-list (cdr (breakpoints-relative (cons 0.0 breakpoints)))))


(defun breakpoints-log (breakpoints)
  (prog ((result '(0.0)) val tim)
loop
    (cond (breakpoints
       (setf val (float (car breakpoints)))
       (setf breakpoints (cdr breakpoints))
       (cond (breakpoints
          (setf tim (car breakpoints))
          (setf breakpoints (cdr breakpoints))))
       (setf result (cons tim (cons (log val) result)))
       (cond ((null breakpoints)
          (return (reverse result))))
       (go loop))
      (t
       (error "Expected odd number of elements in breakpoint list")))))


;; SOUND-WARP -- apply warp function to a sound
;; 
(defun sound-warp (warp-fn signal &optional wrate)
  (cond (wrate
     (snd-resamplev signal *sound-srate*
            (snd-inverse warp-fn (local-to-global 0) wrate)))
    (t
     (snd-compose signal 
              (snd-inverse warp-fn (local-to-global 0) *sound-srate*)))))

(defun snd-extent (sound maxsamples) 
    (list (snd-t0 sound)
      (+ (snd-t0 sound) (/ (snd-length sound maxsamples)
                   (snd-srate sound)))))

(setfn snd-flatten snd-length)

;; (maketable sound)
;;   Creates a table for osc, lfo, etc. by assuming that the samples
;;   in sound represent one period.  The sound must start at time 0.

(defun maketable (sound)
  (list sound
    (hz-to-step 
     (/ 1.0
        (cadr (snd-extent sound 1000000))))
    T))


;(defmacro endTime (sound)
;   `(get-logical-stop ,sound))


;(defmacro beginTime (sound)
;   `(car (snd-extent ,sound)))


; simple stereo pan: as where goes from 0 to 1, sound
; is linearly panned from left to right
;
(defun pan (sound where)
  (vector (mult sound (sum 1 (mult -1 where)))
      (mult sound where)))


(defun prod (&rest snds)
  (cond ((null snds)
     (snd-zero (local-to-global 0) *sound-srate*))
    ((null (cdr snds))
     (car snds))
    ((null (cddr snds))
     (nyq:prod2 (car snds) (cadr snds)))
    (t
     (nyq:prod2 (car snds) (apply #'prod (cdr snds))))))

(setfn mult prod)


;; (NYQ:PROD-OF-ARRAYS S1 S2) - form pairwise products
;
(defun nyq:prod-of-arrays (s1 s2)
  (let* ((n (length s1))
     (p (make-array n)))
    (cond ((/= n (length s2))
       (error "unequal number of channels in prod")))
    (dotimes (i n)
      (setf (aref p i) (nyq:prod2 (aref s1 i) (aref s2 i))))
    p))


; nyq:prod2 - multiply two arguments
; 
(defun nyq:prod2 (s1 s2)
  (setf s1 (nyq:coerce-to s1 s2))
  (setf s2 (nyq:coerce-to s2 s1))
  (cond ((arrayp s1)
     (nyq:prod-of-arrays s1 s2))
    (t
     (nyq:prod-2-sounds s1 s2))))


; (PROD-2-SOUNDS S1 S2) - multiply two sound arguments
; 
(defun nyq:prod-2-sounds (s1 s2)
  (cond ((numberp s1)
         (cond ((numberp s2)
                (* s1 s2))
               (t
                (scale s1 s2))))
        ((numberp s2)
         (scale s2 s1))
        (t
         (snd-prod s1 s2))))


;; RAMP -- linear ramp from 0 to x
;;
(defun ramp (&optional (x 1))
  (let* ((duration (get-duration x)))
    (set-logical-stop
      (warp-abs nil
        (at *rslt*
          (sustain-abs 1
                       (pwl duration 1 (+ duration (/ *control-srate*))))))
      x)))


(defun resample (snd rate)
  (cond ((arrayp snd)
     (let* ((len (length snd))
        (result (make-array len)))
        (dotimes (i len)
        (setf (aref result i)
              (snd-resample (aref snd i) rate)))
        result))
    (t
     (snd-resample snd rate))))


(defun scale (amt snd)
  (cond ((arrayp snd)
     (let* ((len (length snd))
        (result (make-array len)))
        (dotimes (i len)
        (setf (aref result i) (snd-scale amt (aref snd i))))
        result))
    (t
     (snd-scale amt snd))))


(setfn s-print-tree snd-print-tree)

;; (PEAK sound-expression number-of-samples) - find peak amplitude
;
; NOTE: this used to be called s-max
;
(defmacro peak (expression maxlen)
   `(snd-max ',expression ,maxlen))

;; (S-MAX S1 S2) - return maximum of S1, S2
;
(defun s-max (s1 s2)
  (setf s1 (nyq:coerce-to s1 s2))
  (setf s2 (nyq:coerce-to s2 s1))
  (cond ((arrayp s1)
     (nyq:max-of-arrays s1 s2))
    (t
     (nyq:max-2-sounds s1 s2))))

(defun nyq:max-of-arrays (s1 s2)
  (let* ((n (length s1))
     (p (make-array n)))
    (cond ((/= n (length s2))
       (error "unequal number of channels in max")))
    (dotimes (i n)
      (setf (aref p i) (s-max (aref s1 i) (aref s2 i))))
    p))

(defun nyq:max-2-sounds (s1 s2)
  (cond ((numberp s1)
         (cond ((numberp s2)
                (max s1 s2))
               (t
                (snd-maxv s2
                          (snd-const s1 (local-to-global 0.0)
                                     (snd-srate s2) (get-duration 1.0))))))
        ((numberp s2)
         (snd-maxv s1 (snd-const s2 (local-to-global 0.0)
                   (snd-srate s1) (get-duration 1.0))))
        (t
         (snd-maxv s1 s2))))


(defun s-min (s1 s2)
  (setf s1 (nyq:coerce-to s1 s2))
  (setf s2 (nyq:coerce-to s2 s1))
  (cond ((arrayp s1)
         (nyq:min-of-arrays s1 s2))
        (t
         (nyq:min-2-sounds s1 s2))))

(defun nyq:min-of-arrays (s1 s2)
  (let* ((n (length s1))
     (p (make-array n)))
    (cond ((/= n (length s2))
       (error "unequal number of channels in max")))
    (dotimes (i n)
      (setf (aref p i) (s-min (aref s1 i) (aref s2 i))))
    p))

(defun nyq:min-2-sounds (s1 s2)
  (cond ((numberp s1)
         (cond ((numberp s2)
                (min s1 s2))
               (t
                (snd-minv s2
                          (snd-const s1 (local-to-global 0.0)
                                     (snd-srate s2) (get-duration 1.0))))))
        ((numberp s2)
         (snd-minv s1 (snd-const s2 (local-to-global 0.0)
                   (snd-srate s1) (get-duration 1.0))))
       (t
        (snd-minv s1 s2))))


(defun snd-minv (s1 s2)
  (scale -1.0 (snd-maxv (scale -1.0 s1) (scale -1.0 s2))))

; sequence macros SEQ and SEQREP are now in seq.lsp:
; 
(load "seq" :verbose NIL)


; set-logical-stop - modify the sound and return it, time is shifted and
;			 stretched
(defun set-logical-stop (snd tim)
  (let ((d (local-to-global tim)))
    (multichan-expand #'set-logical-stop-abs snd d)))


; set-logical-stop-abs - modify the sound and return it
; 
(defun set-logical-stop-abs (snd tim) (snd-set-logical-stop snd tim) snd)


(defmacro simrep (pair sound)
  `(let (_snds)
     (dotimes ,pair (push ,sound _snds))
     (sim-list _snds)))

(defun sim (&rest snds)
  (sim-list snds))

(setfn sum sim)

(defun sim-list (snds)
  (cond ((null snds)
         (snd-zero (local-to-global 0) *sound-srate*))
        ((null (cdr snds))
         (car snds))
        ((null (cddr snds))
         (nyq:add2 (car snds) (cadr snds)))
        (t
         (nyq:add2 (car snds) (sim-list (cdr snds))))))


(defun s-rest (&optional (dur 1.0) (chans 1))
  (let ((d (get-duration dur))
        r)
    (cond ((= chans 1)
           (snd-const 0.0 *rslt* *SOUND-SRATE* d))
          (t
           (setf r (make-array chans))
           (dotimes (i chans)
             (setf (aref r i) (snd-const 0.0 *rslt* *SOUND-SRATE* d)))
           r))))


(defun tempo (warpfn)
  (slope (snd-inverse warpfn (local-to-global 0) *control-srate*)))



;; (SUM-OF-ARRAYS S1 S2) - add multichannel sounds
; 
; result has as many channels the largest of s1, s2
; corresponding channels are added, extras are copied
; 
(defun sum-of-arrays (s1 s2)
  (let* ((n1 (length s1))
     (n2 (length s2))
     (n (min n1 n2))
     (m (max n1 n2))
     (result (make-array m))
     (big-s (if (> n1 n2) s1 s2)))
    
    (dotimes (i n)
      (setf (aref result i) (nyq:add-2-sounds (aref s1 i) (aref s2 i))))
    (dotimes (i (- m n))
      (setf (aref result (+ n i)) (aref big-s (+ n i))))
    result))


;; (WARP fn behavior) - warp behavior according to fn
;;
;; fn is a map from behavior time to local time, and *WARP* expresses
;; a map from local to global time.
;; To produce a new *WARP* for the environment, we want to compose the
;; effect of the current *WARP* with fn.  Note that fn is also a behavior.
;; It is evaluated in the current environment first, then it is used to
;; modify the environment seen by behavior.
;; *WARP* is a triple: (d s f) denoting the function f(st+d).
;; Letting g represent the new warp function fn, we want f(st+d) o g, or
;; f(s*g(t) + d) in the form (d' s' f').
;; Let's do this one step at a time:
;; f(s*g(t) + d) = f(scale(s, g) + d)
;;               = (shift f -d)(scale(s, g))
;;               = (snd-compose (shift-time f (- d)) (scale s g))
;;
;; If f in NIL, it denotes the identity mapping f(t)=t, so we can
;; simplify:
;; f(scale(s, g) + d) = scale(s, g) + d
;;                    = (snd-offset (scale s g) d)

(defmacro warp (x s)
 `(progv '(*WARP*) (list 
            (list 0.0 1.0
              (if (warp-function *WARP*)
                  (snd-compose (shift-time (warp-function *WARP*) 
                               (- (warp-time *WARP*)))
                       (scale (warp-stretch *WARP*) 
                          (must-be-sound ,x)))
                  (snd-offset (scale (warp-stretch *WARP*) 
                         (must-be-sound ,x))
                      (warp-time *WARP*)))))
     ,s))


(defmacro warp-abs (x s)
 `(progv '(*WARP*) (list (list 0.0 1.0 ,x))
     ,s))


;; MULTICHAN-EXPAND -- construct and return array according to args
;;
;; arrays are used in Nyquist to represent multiple channels
;; if any argument is an array, make sure all array arguments
;; have the same length.  Then, construct a multichannel result
;; by calling fn once for each channel.  The arguments passed to
;; fn for the i'th channel are either the i'th element of an array
;; argument, or just a copy of a non-array argument.
;;
(defun multichan-expand (fn &rest args)
  (let (len newlen result) ; len is a flag as well as a count
    (dolist (a args)
        (cond ((arrayp a)
           (setf newlen (length a))
           (cond ((and len (/= len newlen))
              (error (format nil "In ~A, two arguments are vectors of differing length." fn))))
           (setf len newlen))))
    (cond (len
       (setf result (make-array len))
       ; for each channel, call fn with args
       (dotimes (i len)
           (setf (aref result i)
             (apply fn
            (mapcar
                #'(lambda (a)
                ; take i'th entry or replicate:
                (cond ((arrayp a) (aref a i))
                      (t a)))
                args))))
       result)
      (t
       (apply fn args)))))


;; SELECT-IMPLEMENTATION-? -- apply an implementation according to args
;;
;; There is a different Nyquist primitive for each combination of 
;; constant (NUMBERP) and time-variable (SOUNDP) arguments.  E.g.
;; a filter with fixed parameters differs from one with varying
;; parameters.  In most cases, the user just calls one function,
;; and the arguments are decoded here:


;; SELECT-IMPLEMENTATION-1-1 -- 1 sound arg, 1 selector
;;
(defun select-implementation-1-1 (fns snd sel1 &rest others)
  (if (numberp sel1)
    (apply (aref fns 0) (cons snd (cons sel1 others)))
    (apply (aref fns 1) (cons snd (cons sel1 others)))))


;; SELECT-IMPLEMENTATION-1-2 -- 1 sound arg, 2 selectors
;;
;; choose implemenation according to args 2 and 3
;;
(defun select-implementation-1-2 (fns snd sel1 sel2 &rest others)
  (if (numberp sel2)
    (if (numberp sel1)
      (apply (aref fns 0) (cons snd (cons sel1 (cons sel2 others))))
      (apply (aref fns 1) (cons snd (cons sel1 (cons sel2 others)))))
    (if (numberp sel1)
      (apply (aref fns 2) (cons snd (cons sel1 (cons sel2 others))))
      (apply (aref fns 3) (cons snd (cons sel1 (cons sel2 others)))))))

;; some waveforms

(setf *saw-table* (pwlvr -1 1 1))		; eh, creepy way to get 2205 samples.
(setf *saw-table* (list *saw-table* (hz-to-step 1) T))

(setf *tri-table* (pwlvr -1 0.5 1 0.5 -1))
(setf *tri-table* (list *tri-table* (hz-to-step 1) T))

(setf *id-shape*  (pwlvr -1 2 1 .01 1))	            ; identity
(setf *step-shape* (seq (const -1) (const 1 1.01)))  ; hard step at zero

(defun exp-dec (hold halfdec length)
  (let* ((target (expt 0.5 (/ length halfdec)))
     (expenv (pwev 1 hold 1 length target)))
    expenv)
)

;;; operations on sounds

(defun diff (x &rest y)
  (cond (y (sum x (prod -1 (car y))))
        (t (prod -1 x))))

; compare-shape is a shape table -- origin 1.
(defun compare (x y &optional (compare-shape *step-shape*))
  (let ((xydiff (diff x y)))
    (shape xydiff compare-shape 1)))

;;; oscs

(defun osc-saw (hz) (hzosc hz *saw-table*))
(defun osc-tri (hz) (hzosc hz *tri-table*))

; bias is [-1, 1] pulse width.  sound or scalar.
; hz is a sound or scalar
(defun osc-pulse (hz bias &optional (compare-shape *step-shape*))
  (compare bias (osc-tri hz) compare-shape))
  
;;; tapped delays

;(tapv snd offset vardelay maxdelay)
(setfn tapv snd-tapv) ;; linear interpolation
(setfn tapf snd-tapf) ;; no interpolation
