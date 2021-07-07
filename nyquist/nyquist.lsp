;;;
;;;   ###########################################################
;;;   ### NYQUIST-- A Language for Composition and Synthesis. ###
;;;   ###                                                     ###
;;;   ### Copyright (c) 1994-2006 by Roger B. Dannenberg      ###
;;;   ###########################################################
;;;
(princ "LOADING NYQUIST RUNTIME DEBUG VERSION\n")

;; #### Error checking and reporting functions ####

(setf *SAL-CALL-STACK* nil) ; because SEQ looks at this

;; MULTICHANNEL-SOUNDP - test for vector of sounds
(defun multichannel-soundp (v)
  (prog ((rslt t))
    (if (not (arrayp v)) (return nil))
    (dotimes (i (length v))
      (cond ((not (soundp (aref v i)))
             (setf rslt nil)
             (return nil))))
    (return rslt)))

;; MULTICHANNELP - test for vector of sounds or numbers
(defun multichannelp (v)
  (prog ((rslt t))
    (if (not (arrayp v)) (return nil))
    (dotimes (i (length v))
      (cond ((not (or (numberp (aref v i)) (soundp (aref v i))))
             (setf rslt nil)
             (return nil))))
    (return rslt)))

;; NUMBERSP - test for vector of numbers
(defun numbersp (v)
  (prog ((rslt t))
    (if (not (arrayp v)) (return nil))
    (dotimes (i (length v))
      (cond ((not (numberp (aref v i)))
             (setf rslt nil)
             (return nil))))
    (return rslt)))


;; PARAM-TO-STRING - make printable parameter for error message
(defun param-to-string (param)
  (cond ((null param)    (format nil "NIL"))
        ((soundp param)  (format nil "a SOUND"))
        ((multichannel-soundp param)
          (format nil "a ~A-channel SOUND" (length param)))
        ((eq (type-of param) 'ARRAY) ;; avoid saying "#(1 2), a ARRAY"
          (format nil "~A, an ARRAY" param))
        ((stringp param) (format nil "~s, a STRING" param)) ;; add quotes
        (t
          (format nil "~A, a ~A" param (symbol-name (type-of param))))))


;; NY:TYPECHECK -- syntactic sugar for "if", used for all nyquist typechecks
(setfn ny:typecheck if)

(defun index-to-string (index)
  (nth index '("" " 1st" " 2nd" " 3rd" " 4th" " 5th" " 6th" " 7th")))

(setf number-anon '((NUMBER) nil))
(setf number-sound-anon '((NUMBER SOUND) nil))

;; NY:TYPE-LIST-AS-STRING - convert permissible type list into
;;   description. E.g. typs = '(NUMBER SOUND) and multi = t returns:
;;   "number, sound or array thereof"
(defun ny:type-list-as-string (typs multi)
  (let (lis last penultimate (string "") multi-clause)
    (if (member 'NUMBER   typs) (push "number" lis))
    (if (member 'POSITIVE typs) (push "positive number" lis))
    (if (member 'NONNEGATIVE typs) (push "non-negative number" lis))
    (if (member 'INTEGER  typs) (push "integer" lis))
    (if (member 'STEP     typs) (push "step number" lis))
    (if (member 'STRING   typs) (push "string" lis))
    (if (member 'SOUND    typs) (push "sound" lis))
    (if (member 'NULL     typs) (push "NIL" lis))
    ;; this should be handled with two entries: INTEGER and NULL, but
    ;; this complicates multichan-expand, where lists of arbitrary types
    ;; are not handled and we need INT-OR-NULL for PV-TIME-PITCH's 
    ;; hopsize parameter.
    (cond ((member 'INT-OR-NULL typs)
           (push "integer" lis)
           (push "NIL" lis)))
    (cond ((member 'POSITIVE-OR-NULL typs)
           (push "positive number" lis)
           (push "NIL" lis)))
    (cond (multi
           (setf multi-clause
                 (cond ((> (length lis) 1) "array thereof")
                       ((equal (car lis) "sound") "multichannel sound")
                       (t (strcat "array of " (car lis) "s"))))
           (push multi-clause lis)))
    (setf last (first lis))
    (setf penultimate (second lis))
    (setf lis (cddr lis))
    (dolist (item lis)
      (setf string (strcat item ", " string)))
    (strcat string (if penultimate (strcat penultimate " or ") "") last)))


;; NY:ERROR -- construct an error message and raise an error
(defun ny:error (src index typ val &optional multi (val2 nil second-val))
  (let ((types-string (ny:type-list-as-string (first typ) multi)))
    (error (strcat "In " src "," (index-to-string index) " argument"
            (if (second typ) (strcat " (" (second typ) ")") "")
            (if (eq (char types-string 0) #\i) " must be an " " must be a ")
            types-string
            ", got " (param-to-string val)
            (if second-val (strcat ", and" (param-to-string val2)) "")))))


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
      (as7 bf7) b7
      c8 (cs8 df8) d8 (ds8 ef8) e8 f8 (fs8 gf8) g8 (gs8 af8) a8
      (as8 bf8) b8))

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
  (setq *WARP*	    '(0.0 1.0 nil))
  (setq *LOUD*      0.0)   ; now in dB
  (setq *TRANSPOSE* 0.0)
  (setq *SUSTAIN*   1.0)
  (setq *START*     MIN-START-TIME)
  (setq *STOP*      MAX-STOP-TIME)
  (setq *CONTROL-SRATE* *DEFAULT-CONTROL-SRATE*)
  (setq *SOUND-SRATE* *DEFAULT-SOUND-SRATE*)
  t)				; return nothing in particular

(nyq:environment-init)

(defun get-duration (dur)
  (ny:typecheck (not (numberp dur))
    (ny:error "GET-DURATION" 0 number-anon dur))
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
  (if (warp-function *WARP*)
      (slope (snd-inverse (get-warp) (local-to-global 0)
                          *control-srate*))
      (/ 1.0 (warp-stretch *WARP*))))

(defun get-transpose ()
  (cond ((numberp *TRANSPOSE*) *TRANSPOSE*)
    ((soundp *TRANSPOSE*)
     (sref *TRANSPOSE* 0))
    (t
     (error (format t "*TRANSPOSE* should be a number or sound: ~A" *TRANSPOSE*)))))


(defun get-warp ()
  (let ((f (warp-function *WARP*)))
    (ny:typecheck (null f)
      (error "In GET-WARP, there is no warp function, probably because you are not within WARP or WARP-ABS"))
    (shift-time (scale-srate f (/ (warp-stretch *WARP*)))
                (- (warp-time *WARP*)))))


(load "dspprims.lsp" :verbose NIL)
(load "fileio.lsp" :verbose NIL)


;;;;;;;;;;;;;;;;;;;;;;
;; OSCILATORS
;;;;;;;;;;;;;;;;;;;;;;

(defun build-harmonic (n table-size)
  (ny:typecheck (not (integerp n))
    (ny:error "BUILD-HARMONIC" 1 '((INTEGER) "n") n))
  (ny:typecheck (not (integerp table-size))
    (ny:error "BUILD-HARMONIC" 2 '((INTEGER) "table-size") table-size))
  (ny:typecheck (>= n (/ table-size 2))
    (error "In BUILD-HARMONIC, harmonic number should be less than half the table size"
    (list n table-size)))
  (snd-sine 0 n table-size 1))


(setf *SINE-TABLE* (list (build-harmonic 1 2048)
             (hz-to-step 1.0)
             T))
(setf *TABLE* *SINE-TABLE*)


(defun calculate-hz (pitch what &optional (max-fraction 0.5) maxlength)
  (let ((hz (step-to-hz (+ pitch (get-transpose))))
        (octaves 0) original)
    (setf original hz)
    (while (>= hz (* *SOUND-SRATE* max-fraction))
      (setf octaves (1+ octaves)
            hz (* hz 0.5)))
    (cond ((> octaves 0)
           (format t 
             "Warning: ~A frequency reduced by ~A octaves from ~A to ~A hz to avoid aliasing.\n" 
             what octaves original hz)
           (setf octaves 0)))
    (while (and maxlength (<= hz (/ *SOUND-SRATE* maxlength)))
      (setf octaves (1+ octaves)
            hz (* hz 2.0)))
    (cond ((> octaves 0)
           (format t 
             "Warning: ~A frequency increased by ~A octaves from ~A to ~A hz due to restriction on maximum table length.\n" 
             what octaves original hz)))
    hz))


(defun ny:assert-env-spec (env-spec message)
  (if (not (ny:env-spec-p env-spec))
      (error message env-spec)))


(defun ny:assert-table (fun-name index formal actual)
  (if (not (and (listp actual) (= 3 (length actual))))
      (error (format nil
       "In ~A,~A argument (~A) should be a list of 3 elements, got ~A"
       fun-name (index-to-string index) formal actual)))
  (if (not (soundp (car actual)))
      (error (format nil
       "In ~A,~A argument (~A) should be a list beginning with a sound, got ~A"
       fun-name (index-to-string index) formal actual)))
  (if (not (numberp (second actual)))
      (error (format nil
       "In ~A,~A argument (~A) should be a list whose 2nd element is a step number (pitch), got ~A"
       fun-name (index-to-string index) formal actual)))
  (if (not (third actual))
      (error (format nil
       "In ~A,~A argument (~A) should be a list whose 3rd element is true, got ~A"
       fun-name (index-to-string index) formal actual))))


(defun ny:assert-sample (fun-name index formal actual)
  (if (not (and (listp actual) (= 3 (length actual))))
      (error (format nil
       "In ~A,~A argument (~A) should be a list of 3 elements, got ~A"
       fun-name (index-to-string index) formal actual)))
  (if (not (soundp (car actual)))
      (error (format nil
       "In ~A,~A argument (~A) should be a list beginning with a sound, got ~A"
       fun-name (index-to-string index) formal actual)))
  (if (not (numberp (second actual)))
      (error (format nil
       "In ~A,~A argument (~A) should be a list whose 2nd element is a step number (pitch), got ~A"
       fun-name (index-to-string index) formal actual)))
  (if (not (numberp (third actual)))
      (error (format nil
       "In ~A,~A argument (~A) should be a list whose 3rd element is the sample start time, got ~A"
       fun-name (index-to-string index) formal actual))))

(defun ny:env-spec-p (env-spec)
  (prog (len (rslt t))
    (if (not (listp env-spec)) (return nil))
    (setf len (length env-spec))
    (if (< len 6) (return nil))
    (if (> len 7) (return nil))
    (dolist (x env-spec)
      (cond ((not (numberp x))
             (setf rslt nil)
             (return nil))))
    (return rslt)))


;; AMOSC
;;
(defun amosc (pitch modulation &optional (sound *table*) (phase 0.0))
  (ny:typecheck (not (numberp pitch))
    (ny:error "AMOSC" 1 '((STEP) "pitch") pitch))
  (ny:typecheck (not (soundp modulation))
    (ny:error "AMOSC" 2 '((SOUND) "modulation") modulation))
  (ny:assert-table "AMOSC" 3 "table" sound)
  (ny:typecheck (not (numberp phase))
    (ny:error "AMOSC" 4 '((NUMBER) "phase") phase))
  (let ((modulation-srate (snd-srate modulation))
        (hz (calculate-hz pitch "amosc")))
    (ny:scale-db (get-loud)
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
  (ny:typecheck (not (numberp pitch))
    (ny:error "FMOSC" 1 '((STEP) "pitch") pitch))
  (ny:typecheck (not (soundp modulation))
    (ny:error "FMOSC" 2 '((SOUND) "modulation") modulation))
  (ny:assert-table "FMOSC" 3 "table" sound)
  (ny:typecheck (not (numberp phase))
    (ny:error "FMOSC" 4 '((NUMBER) "phase") phase))
  (let ((modulation-srate (snd-srate modulation))
        (hz (calculate-hz pitch "fmosc")))
    (ny:scale-db (get-loud)
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
(defun fmfb (pitch index &optional (dur 1.0))
  (ny:typecheck (not (numberp pitch))
    (ny:error "FMFB" 1 '((STEP) "pitch") pitch))
  (ny:typecheck (not (or (numberp index) (soundp index)))
    (ny:error "FMFB" 2 '((NUMBER SOUND) "index") index))
  (ny:typecheck (not (numberp dur))
    (ny:error "FMFB" 3 '((NUMBER) "dur") dur))
 (let ((hz (calculate-hz pitch "fmfb")))
   (setf dur (get-duration dur))
   (cond ((soundp index) (ny:fmfbv hz index))
          (t
           (ny:scale-db (get-loud)
                     (snd-fmfb (local-to-global 0) 
                               hz *SOUND-SRATE* index dur))))))

;; private variable index version of fmfb
(defun ny:fmfbv (hz index)
  (let ((modulation-srate (snd-srate index)))
    (cond ((< *SOUND-SRATE* modulation-srate)
           (format t "Warning: down-sampling FM modulation in fmfb~%")
           (setf index (snd-down *SOUND-SRATE* index))))
    (ny:scale-db (get-loud)
              (snd-fmfbv (local-to-global 0) hz *SOUND-SRATE* index))))


;; BUZZ
;;
;; (ARGUMENTS ("long" "n") ("rate_type" "sr") ("double" "hz")
;;            ("time_type" "t0") ("sound_type" "s_fm"))
;; 
(defun buzz (n pitch modulation)
  (ny:typecheck (not (integerp n))
    (ny:error "BUZZ" 1 '((INTEGER) "number of harmonics") n))
  (ny:typecheck (not (numberp pitch))
    (ny:error "BUZZ" 2 '((STEP) "pitch") pitch))
  (ny:typecheck (not (soundp modulation))
    (ny:error "BUZZ" 3 '((SOUND) "modulation") modulation))
  (let ((modulation-srate (snd-srate modulation))
	(hz (calculate-hz pitch "buzz nominal")))
    (cond ((< *SOUND-SRATE* modulation-srate)
           (format t "Warning: down-sampling modulation in buzz~%")
           (setf modulation (snd-down *SOUND-SRATE* modulation))))
    (setf n (max n 1)) ; avoid divide by zero problem
    (ny:scale-db (get-loud)
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
  (ny:typecheck (not (or (numberp hz) (soundp hz)))
    (ny:error "HZOSC" 1 '((NUMBER SOUND) "hz") hz))
  (ny:assert-table "HZOSC" 2 "table" sound)
  (ny:typecheck (not (numberp phase))
    (ny:error "HZOSC" 3 '((NUMBER) "phase") phase))
  (let (hz-srate)
    (cond ((numberp hz)
           (osc (hz-to-step hz) 1.0 sound phase))
          (t
           (setf hz-srate (snd-srate hz))
           (cond ((< *SOUND-SRATE* hz-srate)
                  (format t "Warning: down-sampling hz in hzosc~%")
                  (setf hz (snd-down *SOUND-SRATE* hz))))
           (ny:scale-db (get-loud)
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
  (prog (sample-count result (last-count 0) time-factor (index 0))
    (setf time-factor
      (- (local-to-global (get-sustain))
         (local-to-global 0.0)))
    (setf time-factor (* time-factor *SOUND-SRATE*))
    (ny:typecheck (not (and (listp breakpoints)
                            (cdr breakpoints)
                            (cddr breakpoints)))
      (error "In SIOSC, 3rd argument (breakpoints) must be a list with at least 3 elements"
             breakpoints))
loop
    (ny:typecheck (not (and (listp breakpoints)
                            (soundp (car breakpoints))))
      (error (format nil 
              "In SIOSC, expected a sound in breakpoints list at index ~A" 
              index)
             (car breakpoints)))
    (push (car breakpoints) result)
    (setf breakpoints (cdr breakpoints))
    (setf index (1+ index))
    (cond (breakpoints
           (ny:typecheck (not (and (listp breakpoints)
                                   (numberp (car breakpoints))))
             (error (format nil
                     "In SIOSC, expected a number (time) in breakpoints list at index ~A"
                     index)
                    (car breakpoints)))
           (setf sample-count (truncate
                               (+ 0.5 (* time-factor (car breakpoints)))))
           (cond ((< sample-count last-count)
                  (setf sample-count (1+ last-count))))
           (push sample-count result)
           (setf last-count sample-count)
           (setf breakpoints (cdr breakpoints))
           (setf index (1+ index))
           (cond (breakpoints
                  (go loop)))))
    (setf result (reverse result))
    (return result)))


;; SIOSC -- spectral interpolation oscillator
;;
;; modulation rate must be less than or equal to sound-srate, so
;; force resampling and issue a warning if necessary. snd-fmosc can
;; handle upsampling cases internally.
;;
(defun siosc (pitch modulation breakpoints)
  (ny:typecheck (not (numberp pitch))
    (ny:error "SIOSC" 1 '((STEP) "pitch") pitch))
  (ny:typecheck (not (soundp modulation))
    (ny:error "SIOSC" 2 '((SOUND) "modulation") modulation))
  (let ((modulation-srate (snd-srate modulation))
	(hz (calculate-hz pitch "siosc nominal")))
    (cond ((< *SOUND-SRATE* modulation-srate)
       (format t "Warning: down-sampling FM modulation in siosc~%")
       (setf modulation (snd-down *SOUND-SRATE* modulation))))
    (ny:scale-db (get-loud)
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
  (ny:typecheck (not (numberp freq))
    (ny:error "LFO" 1 '((NUMBER) "freq") freq))
  (ny:typecheck (not (numberp duration))
    (ny:error "LFO" 2 '((NUMBER) "duration") duration))
  (ny:assert-table "LFO" 3 "table" sound)
  (ny:typecheck (not (numberp phase))
    (ny:error "LFO" 4 '((NUMBER) "phase") phase))
  (let ((d (get-duration duration)))
    (if (minusp d) (setf d 0))
    (cond ((> freq (/ *CONTROL-SRATE* 2))
           (format t "Warning: lfo frequency (~A hz) will alias at current control rate (~A hz).\n"
                     freq *CONTROL-SRATE*)))
    (ny:set-logical-stop
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
  (ny:typecheck (not (soundp freq))
    (ny:error "FMLFO" 1 '((SOUND) "freq") freq))
  (ny:assert-table "FMLFO" 2 "table" sound)
  (ny:typecheck (not (numberp phase))
    (ny:error "FMLFO" 3 '((NUMBER) "phase") phase))
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
  (ny:typecheck (not (numberp pitch))
    (ny:error "OSC" 1 '((STEP) "pitch") pitch))
  (ny:typecheck (not (numberp duration))
    (ny:error "OSC" 2 '((NUMBER) "duration") duration))
  (ny:assert-table "OSC" 3 "table" sound)
  (ny:typecheck (not (numberp phase))
    (ny:error "OSC" 4 '((NUMBER) "phase") phase))
  (let ((d  (get-duration duration))
        (hz (calculate-hz pitch "osc")))
    (ny:set-logical-stop
      (snd-scale (db-to-linear (get-loud))
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
  (ny:typecheck (not (numberp steps))
    (ny:error "PARTIAL" 1 '((STEP) "steps") steps))
  (ny:typecheck (not (soundp env))
    (ny:error "PARTIAL" 2 '((SOUND) "env") env))
  (let ((hz (calculate-hz steps "partial")))
    (ny:scale-db (get-loud)
      (snd-partial *sound-srate* hz
                   (force-srate *sound-srate* env)))))


(setf *SINE-SAMPLE* (list (first *TABLE*) (second *TABLE*) 0.0))


;; SAMPLER -- simple attack + sustain sampler
;;
(defun sampler (pitch modulation 
                &optional (sample *SINE-SAMPLE*) (npoints 2))
  (ny:typecheck (not (numberp pitch))
    (ny:error "SAMPLER" 1 '((STEP) "pitch") pitch))
  (ny:typecheck (not (soundp modulation))
    (ny:error "SAMPLER" 2 '((SOUND) "modulation") modulation))
  (ny:assert-sample "SAMPLER" 3 "table" sample)
  (ny:typecheck (not (integerp npoints))
    (ny:error "SAMPLER" 3 '((INTEGER) "npoints") npoints))
  (let ((samp (car sample))
        (samp-pitch (cadr sample))
        (samp-loop-start (caddr sample))
        (hz (calculate-hz pitch "sampler nominal")))
    ; make a waveform table look like a sample with no attack:
    (cond ((not (numberp samp-loop-start))
           (setf samp-loop-start 0.0)))
    (ny:scale-db (get-loud)
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
  (ny:typecheck (not (numberp steps))
    (ny:error "SINE" 1 '((STEP) "steps") steps))
  (ny:typecheck (not (numberp duration))
    (ny:error "SINE" 2 '((NUMBER) "duration") duration))
  (let ((hz (calculate-hz steps "sine"))
        (d (get-duration duration)))
    (ny:set-logical-stop
      (ny:scale-db (get-loud)
        (snd-sine *rslt* hz *sound-srate* d))
      duration)))


;; PLUCK
;;
;; (ARGUMENTS ("double" "sr") ("double" "hz") ("time_type" "t0") 
;;            ("time_type" "d") ("double" "final_amp"))
;;
(defun pluck (steps &optional (duration 1.0) (final-amp 0.001))
  (ny:typecheck (not (numberp steps))
    (ny:error "PLUCK" 1 '((NUMBER) "steps") steps))
  (ny:typecheck (not (numberp duration))
    (ny:error "PLUCK" 2 '((NUMBER) "duration") duration))
  (ny:typecheck (not (numberp final-amp))
    (ny:error "PLUCK" 3 '((NUMBER) "final-amp") final-amp))
  ;; 200000 is MAXLENGTH in nyquist/tran/pluck.alg - the max table length
  (let ((hz (calculate-hz steps "pluck" (/ 1.0 3) 200000))
        (d (get-duration duration)))
    (ny:set-logical-stop
      (ny:scale-db (get-loud)
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


;; (NYQ:TO-ARRAY SOUND N) - duplicate SOUND to N channels
;
(defun nyq:to-array (value len)
  (let ((a (make-array len)))
    (dotimes (i len)
      (setf (aref a i) value))
    a))


; nyq:add2 - add two arguments. 
;
; Assumes s1 and s2 are numbers, sounds, or multichannel sounds or numbers
;
; Semantics: numbers and sounds can be freely mixed and 
;    add as expected. Arrays (multichannel) arguments are
;    added channel-by-channel, and if one array is larger,
;    the "extra" channels are simply copied to the result.
;    Therefore the result has the channel count of the 
;    maximum channel count in s1 or s2. When adding a
;    multichannel sound to a (non-multichannel) sound, the
;    sound is coerced to a 1-channel multi-channel sound,
;    and therefore adds to channel 1 of the multi-channel 
;    sound. However, when adding a multichannel sound to a
;    number, the number is added to *every* channel.
; Semantics differ from the normal multichan-expand processing
;    in that sounds are considered to be a multichannel sound
;    with 1 channel, and channel counts do not have to match
;    when processing array arguments.
; 
(defun nyq:add2 (s1 s2)
        ; make number + number as fast as possible:
  (cond ((and (numberp s1) (numberp s2)) (+ s1 s2))
        ; if not 2 numbers, the overhead here is amortized by
        ;    computing samples of at least one sound
        ((and (arrayp s1) (numberp s2))
          (sum-of-arrays s1 (nyq:to-array s2 (length s1))))
        ((and (arrayp s2) (numberp s1))
          (sum-of-arrays (nyq:to-array s1 (length s2)) s2))
        ((and (arrayp s1) (soundp s2))
         (sum-of-arrays s1 (vector s2)))
        ((and (arrayp s2) (soundp s1))
         (sum-of-arrays (vector s1) s2))
        ((and (arrayp s1) (arrayp s2))
         (sum-of-arrays s1 s2))
        ((numberp s1)
         (snd-offset s2 s1))
        ((numberp s2)
         (snd-offset s1 s2))
        (t
         (nyq:add-2-sounds s1 s2))))


; (NYQ:ADD-2-SOUNDS S1 S2) - add two sound arguments
; 
; assumes s1 and s2 are sounds
;
(defun nyq:add-2-sounds (s1 s2)
  (let ((s1sr (snd-srate s1))
        (s2sr (snd-srate s2)))
    (cond ((> s1sr s2sr)
           (snd-add s1 (snd-up s1sr s2)))
          ((< s1sr s2sr)
           (snd-add (snd-up s2sr s1) s2))
          (t
           (snd-add s1 s2)))))


(defmacro at (x s)
 `(progv '(*WARP*)
         (let ((shift ,x))
           (ny:typecheck (not (numberp shift))
               (error "1st argument of AT (or 2nd argument of SAL's @ operator) should be a time offset number" shift))
           (list (list (+ (warp-time *WARP*) 
                       (* (warp-stretch *WARP*) shift))
                       (warp-stretch *WARP*)
                       (warp-function *WARP*))))
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
         (let ((tim ,x))
           (ny:typecheck (not (numberp tim))
               (error "1st argument of AT-ABS (or 2nd argument of SAL's @@ operator) should be a number (start time)" tim))
           (if (warp-function *WARP*)
               (list (list (sref-inverse (warp-function *WARP*) tim)
                           (warp-stretch *WARP*)
                           (warp-function *WARP*)))
               (list (list tim (warp-stretch *WARP*) NIL))))
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
  (ny:typecheck (not (or (numberp x) (soundp x) (multichannelp x)))
    (ny:error "CLIP" 1 number-sound-anon x t))
  (ny:typecheck (not (numberp v))
    (ny:error "CLIP" 2 number-anon v))
  (cond ((numberp x)
         (max (min x v) (- v)))
        ((arrayp x)
         (let* ((len (length x))
           (result (make-array len)))
           (dotimes (i len)
             (setf (aref result i) 
             (snd-clip (aref x i) v)))
         result))
        (t ;; x is a sound
         (snd-clip x v))))


;; (NYQ:COERCE-TO S1 S2) - expand sound s1 to type of s2
; 
(defun nyq:coerce-to (s1 s2)
  (cond ((or (soundp s1) (numberp s1))
         (cond ((arrayp s2)
                (nyq:to-array s1 (length s2)))
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
  `(let ((rate ,r))
     (progv '(*CONTROL-SRATE*)
            (progn (ny:typecheck (not (numberp rate))
                     (ny:error "CONTROL-SRATE-ABS" 1 '((NUMBER) "sample rate") rate))
                 (list rate))
      ,s)))

; db = 20log(ratio)
; db = 20 ln(ratio)/ln(10)
; db/20 = ln(ratio)/ln(10)
; db ln(10)/20 = ln(ratio)
; e^(db ln(10)/20) = ratio
;
(setf ln10over20 (/ (log 10.0) 20))

(defun db-to-linear (x) 
  (ny:typecheck (not (or (numberp x) (soundp x) (multichannelp x)))
    (ny:error "DB-TO-LINEAR" 0 number-sound-anon x t))
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
  (ny:typecheck (not (or (numberp x) (soundp x) (multichannelp x)))
    (ny:error "LINEAR-TO-DB" 0 number-sound-anon x t))
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
  (ny:typecheck (not (or (numberp x) (soundp x) (multichannelp x)))
    (ny:error "STEP-TO-HZ" 0 number-sound-anon x t))
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
  (ny:typecheck (not (or (numberp x) (soundp x) (multichannelp x)))
    (ny:error "HZ-TO-STEP" 0 number-sound-anon x t))
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
  (ny:typecheck (not (soundp sound))
    (ny:error "SREF" 1 '((SOUND) "sound") sound))
  (ny:typecheck (not (numberp point))
    (ny:error "SREF" 2 '((NUMBER) "time") point))
  (snd-sref sound (local-to-global point)))


; extract - start is stretched and shifted as is stop
;  result is shifted to start at local time zero
(defun extract (start stop sound)
  (ny:typecheck (not (numberp start))
    (ny:error "EXTRACT" 1 '((NUMBER) "start") start))
  (ny:typecheck (not (numberp stop))
    (ny:error "EXTRACT" 2 '((NUMBER) "stop") stop))
  (ny:typecheck (< stop start) 
    (error
      (format nil "In EXTRACT, stop (~A) must be greater or equal to start (~A)"
                  stop start)))
  (ny:typecheck (not (soundp sound))
    (ny:error "EXTRACT" 3 '((SOUND) "sound") sound))
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
  (ny:typecheck (not (numberp start))
    (ny:error "EXTRACT-ABS" 1 '((NUMBER) "start") start))
  (ny:typecheck (not (numberp stop))
    (ny:error "EXTRACT-ABS" 2 '((NUMBER) "stop") stop))
  (ny:typecheck (< stop start) 
    (error
      (format nil
       "In EXTRACT-ABS, stop (~A) must be greater or equal to start (~A)"
       stop start)))
  (ny:typecheck (not (soundp sound))
    (ny:error "EXTRACT-ABS" 3 '((SOUND) "sound") sound))
  (ny:typecheck (not (numberp start-time))
    (ny:error "EXTRACT-ABS" 4 '((NUMBER) "start-time") start-time))
  (let ((t0 (snd-t0 sound)) offset)
    (cond ((/= t0 start-time)
           (setf offset (- t0 start-time))
           (setf start (- start offset))
           (setf stop (- stop offset))))
    (snd-xform sound (snd-srate sound) start-time start stop 1.0)))


(defun local-to-global (local-time)
  (ny:typecheck (not (numberp local-time))
    (ny:error "LOCAL-TO-GLOBAL" 0 '((NUMBER) "local-time") local-time))
  (let ((d (warp-time *WARP*))
    (s (warp-stretch *WARP*))
    (w (warp-function *WARP*))
    global-time)
    (setf global-time (+ (* s local-time) d))
    (if w (snd-sref w global-time) global-time)))


(defmacro loud (x s)
 `(progv '(*LOUD*)
         (let ((ld ,x))
           (ny:typecheck (not (or (numberp ld) (soundp ld)))
               (ny:error "LOUD" 1 number-sound-anon ld))
           (list (sum *LOUD* ld)))
     ,s))


(defmacro loud-abs (x s)
 `(progv '(*LOUD*)
         (let ((ld ,x))
           (ny:typecheck (not (or (numberp ld) (soundp ld)))
                (ny:error "LOUD-ABS" 1 number-anon ld))
           (list ld))
     ,s))


;(defun must-be-sound (x)
; (cond ((soundp x) x)
;       (t
;        (error "SOUND type expected" x))))


;; NY:SCALE-DB -- a "fast" scale-db: no typechecks and
;;                no multichannel expansion
(defun ny:scale-db (factor sound)
  (snd-scale (db-to-linear factor) sound))


;; SCALE-DB -- same as scale, but argument is in db
;;
(defun scale-db (factor sound)
;  (ny:typecheck (not (or (numberp factor) (numbersp factor)))
;    (ny:error "SCALE-DB" 1 '((NUMBER) "dB") factor t))
;  (ny:typecheck (not (or (soundp sound) (multichannel-soundp sound)))
;    (ny:error "SCALE-DB" 2 '((SOUND) "sound") sound t))
  (multichan-expand "SCALE-DB" #'ny:scale-db 
    '(((NUMBER) "factor") ((SOUND) "sound")) factor sound))
    


(defun set-control-srate (rate)
  (ny:typecheck (not (numberp rate))
    (ny:error "SET-CONTROL-SRATE" 0 '((NUMBER) "rate") rate))
  (setf *default-control-srate* (float rate))
  (nyq:environment-init))

(defun set-sound-srate (rate) 
  (ny:typecheck (not (numberp rate))
    (ny:error "SET-SOUND-SRATE" 0 '((NUMBER) "rate") rate))
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
  (ny:typecheck (not (soundp snd))
    (ny:error "S-PLOT (or PLOT command)" 1 '((SOUND) nil) snd))
  (ny:typecheck (not (numberp dur))
    (ny:error "S-PLOT (or PLOT command)" 2 '((NUMBER) "dur") dur))
  (ny:typecheck (not (integerp n))
    (ny:error "S-PLOT (or PLOT command)" 3 '((INTEGER) nil) n))

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
     ;(display "test" (length points) n)
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
 `(progv '(*SOUND-SRATE*) 
         (let ((rate ,r))
            (ny:typecheck (not (numberp rate))
              (ny:error "SOUND-SRATE-ABS" 1 '((NUMBER) "sample rate") rate))
            (list rate))
      ,s))


(defmacro stretch (x s)
 `(progv '(*WARP*)
         (let ((str ,x))
           (ny:typecheck (not (numberp str))
               (error "1st argument of STRETCH (or 2nd argument of SAL's ~ operator) should be a number (stretch factor)" str))
                (list (list (warp-time *WARP*)
                            (* (warp-stretch *WARP*) str)
                            (warp-function *WARP*))))
     (ny:typecheck (minusp (warp-stretch *WARP*))
         (error "In STRETCH (or SAL's ~ operator), negative stretch factor is not allowed"
                (warp-stretch *WARP*)))
     ,s))

         
(defmacro stretch-abs (x s)
 `(progv '(*WARP*)
         (let ((str ,x))
           (ny:typecheck (not (numberp str))
               (error "1st argument of STRETCH-ABS (or 2nd argument of SAL's ~~ operator) should be a number (stretch factor)" str))
           (list (list (local-to-global 0) str nil)))
     (ny:typecheck (minusp (warp-stretch *WARP*))
         (error "In STRETCH-ABS (or SAL's ~~ operator), negative stretch factor is not allowed"
                (warp-stretch *WARP*)))
     ,s))


(defmacro sustain (x s)
 `(progv '(*SUSTAIN*)
         (let ((sus ,x))
           (ny:typecheck (not (or (numberp sus) (soundp sus)))
               (ny:error "SUSTAIN" 1 number-sound-anon sus))
           (list (prod *SUSTAIN* sus)))
      ,s))


(defmacro sustain-abs (x s)
 `(progv '(*SUSTAIN*)
         (let ((sus ,x))
           (ny:typecheck (not (or (numberp sus) (soundp sus)))
               (ny:error "SUSTAIN-ABS" 1 number-sound-anon sus))
           (list sus))
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
 `(progv '(*TRANSPOSE*)
         (let ((amt ,x))
           (ny:typecheck (not (or (numberp amt) (soundp amt)))
                         (ny:error "TRANSPOSE" 1 number-sound-anon amt))
           (list (sum *TRANSPOSE* amt)))
      ,s))


(defmacro transpose-abs (x s)
 `(progv '(*TRANSPOSE*)
         (let ((amt ,x))
           (ny:typecheck (not (or (numberp amt) (soundp amt)))
               (ny:error "TRANSPOSE-ABS" 1 number-anon amt))
           (list amt))
      ,s))


;; CONTROL-WARP -- apply a warp function to a control function
;; 
(defun control-warp (warp-fn control &optional wrate)
  (ny:typecheck (not (soundp warp-fn))
    (ny:error "CONTROL-WARP" 1 '((SOUND) "warp-fn") warp-fn))
  (ny:typecheck (not (soundp control))
    (ny:error "CONTROL-WARP" 2 '((SOUND) "control") control))
  (cond (wrate
     (ny:typecheck (not (numberp wrate))
       (ny:error "CONTROL-WARP" 3 '((NUMBER) "wrate") wrate))
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
  (ny:typecheck (not (or (soundp sound) (multichannel-soundp sound)))
    (ny:error "CUE" 0 '((SOUND) nil) sound t))
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
  (ny:typecheck (not (or (soundp sound) (multichannel-soundp sound)))
    (ny:error "SOUND" 0 '((SOUND) nil) sound t))
  (cond ((arrayp sound)
     (nyq:sound-of-array sound))
    (t
     (nyq:sound sound))))


;; (SCALE-SRATE SOUND SCALE)
;; multiplies the sample rate by scale
(defun scale-srate (sound scale)
  (ny:typecheck (not (soundp sound))
    (ny:error "SCALE-SRATE" 1 '((SOUND) "sound") sound))
  (ny:typecheck (not (numberp scale))
    (ny:error "SCALE-SRATE" 2 '((NUMBER) "scale") scale))
  (let ((new-srate (* scale (snd-srate sound))))
    (snd-xform sound new-srate (snd-time sound) 
           MIN-START-TIME MAX-STOP-TIME 1.0)))


;; (SHIFT-TIME SOUND SHIFT)
;; shift the time of a function by SHIFT, i.e. if SOUND is f(t),
;; then (shift-time SOUND SHIFT) is f(t - SHIFT).  Note that if
;; you look at plots, the shifted sound will move *right* when SHIFT
;; is positive.  
(defun shift-time (sound shift)
  (ny:typecheck (not (soundp sound))
    (ny:error "SHIFT-TIME" 1 '((SOUND) "sound") sound))
  (ny:typecheck (not (numberp shift))
    (ny:error "SHIFT-TIME" 2 '((NUMBER) "shift") shift))
  (snd-xform sound (snd-srate sound) (+ (snd-t0 sound) shift)
         MIN-START-TIME MAX-STOP-TIME 1.0))


;; (control sound)
;;    Same as (sound sound), except this is used for control signals.  
;;    This code is identical to sound.
(defun control (sound)
  (ny:typecheck (not (or (soundp sound) (multichannel-soundp sound)))
    (ny:error "CONTROL" 0 '((SOUND) nil) sound t))
  (cond ((arrayp sound)
     (nyq:sound-of-array sound))
    (t
     (nyq:sound sound))))


;; (cue-file string)
;;    Loads a sound file with the given name, returning a sound which is
;; transformed to the current environment.
(defun cue-file (name)
    (ny:typecheck (not (stringp name))
      (ny:error "CUE-FILE" 0 '((STRING) "name") name))
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
;; the beginning of the envelope is used to determining absolute duration.
;; Since PWL is ultimately called to create the envelope, we must use
;; ABS-ENV to prevent any further transforms inside PWL.  We use
;; (AT global-start ...) inside ABS-ENV so that the final result has 
;; the proper starting time.
;;
(defun env (t1 t2 t4 l1 l2 l3 &optional (duration 1.0))
  (ny:typecheck (not (and (numberp t1) (numberp t2) (numberp t4)
                          (numberp l1) (numberp l2) (numberp l3)))
    (error "In ENV, expected 6 numbers (t1, t2, t4, l1, l2, l3)"
           (list t1 t2 t4 l1 l2 l3)))
  (ny:typecheck (not (numberp duration))
    (ny:error "ENV" 7 '((NUMBER) "duration") duration))
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
    (ny:set-logical-stop
      (abs-env (at *rslt*
                   (pwl t1 l1 (+ t1 t2) l2 (- actual-dur t4) l3 actual-dur)))
      duration)))


(defun to-mono (sound)
  (ny:typecheck (not (or (soundp sound) (multichannel-soundp sound)))
    (ny:error "TO-MONO" 1 '((SOUND) NIL) sound t))
  (let ((s sound))
    (cond ((arrayp sound)
           (setf s (aref sound 0))  ;; ANY channel opens the gate
            (dotimes (i (1- (length sound)))
             (setf s (nyq:add-2-sounds s (aref sound (1+ i)))))))
    s))


(defun gate (sound lookahead risetime falltime floor threshold 
             &optional (source "GATE"))
  ;(ny:typecheck (not (soundp sound))
  (ny:typecheck (not (or (soundp sound) (multichannel-soundp sound)))
    (ny:error source 1 '((SOUND) "sound") sound t))
  (ny:typecheck (not (numberp lookahead))
    (ny:error source 2 '((NUMBER) "lookahead") lookahead))
  (ny:typecheck (not (numberp risetime))
    (ny:error source 3 '((NUMBER) "risetime") risetime))
  (ny:typecheck (not (numberp falltime))
    (ny:error source 4 '((NUMBER) "falltime") falltime))
  (ny:typecheck (not (numberp floor))
    (ny:error source 5 '((NUMBER) "floor") floor))
  (ny:typecheck (not (numberp threshold))
    (ny:error source 6 '((NUMBER) "threshold") threshold))
  (cond ((< lookahead risetime)
         (format t "WARNING: lookahead (~A) ~A (~A) in ~A ~A ~A.\n"
                 lookahead "must be greater than risetime" risetime
                 source "function; setting lookahead to" risetime)
         (setf lookahead risetime)))
  (cond ((< risetime 0)
         (format t "WARNING: risetime (~A) ~A ~A ~A\n" risetime
                 "must be greater than zero in" source
                 "function; setting risetime to 0.01.")
         (setf risetime 0.01)))
  (cond ((< falltime 0)
         (format t "WARNING: ~A ~A function; setting falltime to 0.01.\n"
                 "falltime must be greater than zero in" source)
         (setf falltime 0.01)))
  (cond ((< floor 0.00001)
         (format t "WARNING: ~A ~A function; setting floor to 0.00001.\n"
                 "floor must be greater than zero in" source)
         (setf floor 0.00001)))
  (let (s) ;; s becomes sound after collapsing to one channel
    (cond ((arrayp sound)           ;; use s-max over all channels so that
           (setf s (aref sound 0))  ;; ANY channel opens the gate
           (dotimes (i (1- (length sound)))
             (setf s (s-max s (aref sound (1+ i))))))
          (t (setf s sound)))
    (setf s (snd-gate (seq (cue s)
                           (stretch-abs 1.0 (s-rest lookahead)))
                      lookahead risetime falltime floor threshold))
    ;; snd-gate delays everything by lookahead, so this will slide the sound
    ;; earlier by lookahead and delete the first lookahead samples
    (prog1 (snd-xform s (snd-srate s) (snd-t0 s)
                      (+ (snd-t0 s) lookahead) MAX-STOP-TIME 1.0)
           ;; This is *really* tricky. Normally, we would return now and
           ;; the GC would free s and sound which are local variables. The
           ;; only references to the sounds once stored in s and sound are
           ;; lazy unit generators that will free samples almost as soon as
           ;; they are computed, so no samples will accumulate. But wait! The
           ;; 2nd SEQ expression with S-REST can reference s and sound because
           ;; (due to macro magic) a closure is constructed to hold them until
           ;; the 2nd SEQ expression is evaluated. It's almost as though s and
           ;; sound are back to being global variables. Since the closure does
           ;; not actually use either s or sound, we can clear them (we are
           ;; still in the same environment as the closures packed inside SEQ,
           ;; so s and sound here are still the same variables as the ones in
           ;; the closure. Note that the other uses of s and sound already made
           ;; copies of the sounds, and s and sound are merely references to
           ;; them -- setting to nil will not alter the immutable lazy sound
           ;; we are returning. Whew!
           (setf s nil) (setf sound nil))))


;; (osc-note step &optional duration env sust volume sound)
;;   Creates a note using table-lookup osc, but with an envelope.
;; The ENV parameter may be a parameter list for the env function,
;; or it may be a sound.
;;
(defun osc-note (pitch &optional (duration 1.0) 
               (env-spec '(0.02 0.1 0.3 1.0 .8 .7))
               (volume 0.0)
               (table *TABLE*))
  (ny:typecheck (not (numberp pitch))
    (ny:error "OSC-NOTE" 1 '((STEP) "pitch")  pitch))
  (ny:typecheck (not (numberp duration))
    (ny:error "OSC-NOTE" 2 '((NUMBER) "duration") duration))
  (ny:assert-env-spec env-spec
    "In OSCNOTE, 3rd argument (env-spec) must be a  list of 6 or 7 numbers to pass as arguments to ENV")
  (ny:typecheck (not (numberp volume))
    (ny:error "OSC-NOTE" 4 '((NUMBER) "volume") volume))
  (ny:assert-table "OSC-NOTE" 5 "table" table)
    
  (ny:set-logical-stop
   (mult (loud volume (osc pitch duration table))
     (if (listp env-spec)
       (apply 'env env-spec)
       env-spec))
   duration))


;; force-srate -- resample snd if necessary to get sample rate
;
(defun force-srate (sr snd)
  (ny:typecheck (not (numberp sr))
    (ny:error "FORCE-SRATE" 1 '((NUMBER) "sr") sr))
  (ny:typecheck (not (or (soundp snd) (multichannel-soundp snd)))
    (ny:error "FORCE-SRATE" 2 '((SOUND) "snd") snd t))
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
    (t (error (format nil "In force-srates: arguments not compatible. srs is ~A, snd is ~A. Perhaps you are constructing a sequence using both mono and multi-channel sounds."
               (type-of srs) (type-of snd))))))


;; (breakpoints-convert (t1 x1 t2 x2 ... tn) t0)
;;   converts times to sample numbers and scales amplitudes
;;   t0 is the global (after warping) start time
;;
;; input list is one or more numbers
;; result is abs-sample-count, val, abs-sample-count, val, ...
;;     if the list length is odd, the result length is odd, and
;;     snd-pwl treats it as if a final value of zero was appended
;; 
;; NOTE: there were some stack overflow problems with the original
;; recursive version (in comments now), so it was rewritten as an
;; iteration.
;;
(defun breakpoints-convert (list t0 source)
  (prog (sample-count result sust (last-count 0))
    (setf sust (get-sustain))
    (ny:typecheck (not (consp list))
      (error (format nil "In ~A, expected a list of numbers" source) list))
 loop
    (ny:typecheck (not (numberp (car list)))
      (error (format nil "In ~A, expected only numbers in breakpoint list, got ~A"
              source (car list))))
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
       (ny:typecheck (not (numberp (car list)))
         (error (format nil "In ~A, expected only numbers in breakpoint list" source)
                (car list)))
       (push (float (car list)) result)))
    (setf list (cdr list))
    (cond (list
       (go loop)))
    (return (reverse result))))

 
;; (pwl t1 l1 t2 l2 ... tn)
;;   Creates a piece-wise linear envelope from breakpoint data.
;;
(defun pwl (&rest breakpoints) (pwl-list breakpoints "PWL"))

(defun pwlr (&rest breakpoints) (pwlr-list breakpoints "PWLR"))

;; BREAKPOINTS-RELATIVE list source 
;;  converts list, which has the form (value dur value dur value ...)
;;  into the form (value time value time value ...)
;;  the list may have an even or odd length
;;
(defun breakpoints-relative (breakpoints source)
  (prog (result (sum 0.0))
    (ny:typecheck (not (consp breakpoints))
      (error (format nil "In ~A, expected list of numbers, got ~A"
             source breakpoints)))
 loop
    (ny:typecheck (not (numberp (car breakpoints)))
      (error (format nil 
              "In ~A, expected only numbers in breakpoints list, got ~A"
              source (car breakpoints))))
    (setf sum (+ sum (car breakpoints)))
    (push sum result)
    (cond ((cdr breakpoints)
       (setf breakpoints (cdr breakpoints))
       (ny:typecheck (not (numberp (car breakpoints)))
         (error (format nil 
                 "In ~A, expected only numbers in breakpoints list, got ~A"
                 source (car breakpoints))))
       (push (car breakpoints) result)))
    (setf breakpoints (cdr breakpoints))
    (cond (breakpoints
       (go loop)))
    (return (reverse result))))


(defun pwlr-list (breakpoints &optional (source "PWLR-LIST"))
  (pwl-list (breakpoints-relative breakpoints source) source))

(defun pwl-list (breakpoints &optional (source "PWL-LIST"))
  (let ((t0 (local-to-global 0)))
    (snd-pwl t0 *control-srate* (breakpoints-convert breakpoints t0 source))))

;; (pwlv l1 t1 l2 t2 ... ln)
;; Creates a piece-wise linear envelope from breakpoint data;
;; the function initial and final values are explicit
;;
(defun pwlv (&rest breakpoints)
  ;use pwl, modify breakpoints with initial and final changes
  ;need to put initial time of 0, and final time of 0
  (pwlv-list breakpoints "PWLV"))

(defun pwlv-list (breakpoints &optional (source "PWLV-LIST"))
  (ny:typecheck (not (consp breakpoints))
    (error (format nil "In ~A, expected list of numbers, got ~A"
           source breakpoints)))
  (pwl-list (cons 0.0 breakpoints) source))

(defun pwlvr (&rest breakpoints) (pwlvr-list breakpoints "PWLVR"))

(defun pwlvr-list (breakpoints &optional (source "PWLVR-LIST"))
  (ny:typecheck (not (consp breakpoints))
     (error (format nil "In ~A, expected list of numbers, got ~A"
            source breakpoints)))
  (pwlr-list (cons 0.0 breakpoints) source))

(defun pwe (&rest breakpoints)
  (pwe-list breakpoints "PWE"))

(defun pwe-list (breakpoints &optional (source "PWE-LIST"))
  (ny:typecheck (not (consp breakpoints))
     (error (format nil "In ~A, expected list of numbers, got ~A"
            source breakpoints)))
  (pwev-list (cons 1.0 breakpoints) source))

(defun pwer (&rest breakpoints)
  (pwer-list breakpoints "PWER"))

(defun pwer-list (breakpoints &optional (source "PWER-LIST"))
  (pwe-list (breakpoints-relative breakpoints source) source))

(defun pwev (&rest breakpoints)
  (pwev-list breakpoints "PWEV"))

(defun pwev-list (breakpoints &optional (source "PWEV-LIST"))
  (let ((lis (breakpoints-log breakpoints source)))
    (s-exp (pwl-list lis))))

(defun pwevr (&rest breakpoints) (pwevr-list breakpoints "PWEVR"))

(defun pwevr-list (breakpoints &optional (source "PWEVR-LIST"))
  (ny:typecheck (not (consp breakpoints))
     (error (format nil "In ~A, expected list of numbers, got ~A"
            source breakpoints)))
  (pwev-list (cdr (breakpoints-relative (cons 0.0 breakpoints) source)) source))


;; input is 2 or more numbers representing val, time, val, time, ...
;; output is odd number of 1 or more numbers representing
;;     time, val, time, val, ..., time
;; 
;;
(defun breakpoints-log (breakpoints source)
  (prog ((result '(0.0)) val tim)
loop
    (ny:typecheck (not (consp breakpoints))
      (error (format nil "In ~A, expected list of numbers, got ~A"
                         source breakpoints)))
    (ny:typecheck (not (numberp (car breakpoints)))
      (error (format nil "In ~A, expected number in breakpoint list, got ~A"
                         source (car breakpoints))))

    (setf val (float (car breakpoints)))
    (setf breakpoints (cdr breakpoints))

    (cond (breakpoints
       (ny:typecheck (not (consp breakpoints))
         (error (format nil "In ~A, expected list of numbers, got ~A"
                source (car breakpoints))))
       (setf tim (car breakpoints))
       (setf breakpoints (cdr breakpoints))
       (ny:typecheck (not (numberp tim))
         (error (format nil "In ~A, expected number in breakpoint list, got ~A"
                source tim)))))

    (setf result (cons tim (cons (log val) result)))
    (cond ((null breakpoints)
           (return (reverse result))))
    (go loop)))


;; SOUND-WARP -- apply warp function to a sound
;; 
(defun sound-warp (warp-fn signal &optional wrate)
  (ny:typecheck (not (soundp warp-fn))
    (ny:error "SOUND-WARP" 1 '((SOUND) "warp-fn") warp-fn))
  (ny:typecheck (not (soundp signal))
    (ny:error "SOUND-WARP" 2 '((SOUND) "signal") signal))
  (cond (wrate
     (ny:typecheck (not (numberp wrate))
       (ny:error "SOUND-WARP" 3 '((NUMBER) "wrate") wrate))
     (snd-resamplev signal *sound-srate*
            (snd-inverse warp-fn (local-to-global 0) wrate)))
    (t
     (snd-compose signal 
              (snd-inverse warp-fn (local-to-global 0) *sound-srate*)))))

(defun snd-extent (sound maxsamples) 
    (ny:typecheck (not (soundp sound))
      (ny:error "SND-EXTENT" 1 '((SOUND) "sound") sound))
    (ny:typecheck (not (integerp maxsamples))
      (ny:error "SND-EXTENT" 2 '((INTEGER) "maxsamples") maxsamples))
    (list (snd-t0 sound)
      (+ (snd-t0 sound) (/ (snd-length sound maxsamples)
                   (snd-srate sound)))))

(setfn snd-flatten snd-length)

;; (maketable sound)
;;   Creates a table for osc, lfo, etc. by assuming that the samples
;;   in sound represent one period.  The sound must start at time 0.

(defun maketable (sound)
  (ny:typecheck (not (soundp sound))
    (ny:error "MAKETABLE" 0 '((SOUND) nil) sound))
  (list sound
    (hz-to-step 
     (/ 1.0
        (cadr (snd-extent sound 1000000))))
    T))


; simple stereo pan: as where goes from 0 to 1, sound
; is linearly panned from left to right
;
(defun pan (sound where)
  (ny:typecheck (not (soundp sound))
    (ny:error "PAN" 1 '((SOUND) "sound") sound))
  (ny:typecheck (not (or (soundp where) (numberp where)))
    (ny:error "PAN" 2 '((NUMBER SOUND) "where")  where))
  (vector (mult sound (sum 1 (mult -1 where)))
          (mult sound where)))


(setf prod-source "PROD (or * in SAL)")

(defun prod (&rest snds)
  (cond ((null snds)
     (snd-zero (local-to-global 0) *sound-srate*))
    ((null (cdr snds))
     (car snds))
    ((null (cddr snds))
     (nyq:prod2 (car snds) (cadr snds) prod-source))
    (t
     (nyq:prod2 (car snds) (apply #'prod (cdr snds)) prod-source))))

(setfn mult prod)


;; (NYQ:PROD-OF-ARRAYS S1 S2) - form pairwise products
;
(defun nyq:prod-of-arrays (s1 s2 source)
  (let* ((n (length s1))
     (p (make-array n)))
    (ny:typecheck (/= n (length s2))
       (error (strcat "In " source ", unequal number of channels, got "
               (param-to-string s1) " and " (param-to-string s2))))
    (dotimes (i n)
      (setf (aref p i) (nyq:prod2 (aref s1 i) (aref s2 i) source)))
    p))


; nyq:prod2 - multiply two arguments
; 
(defun nyq:prod2 (s1 s2 source)
  (setf s1 (nyq:coerce-to s1 s2))
  (setf s2 (nyq:coerce-to s2 s1))
  (cond ((arrayp s1)
     (nyq:prod-of-arrays s1 s2 source))
    (t
     (nyq:prod-2-sounds s1 s2 source))))


; (PROD-2-SOUNDS S1 S2) - multiply two sound arguments
; 
(defun nyq:prod-2-sounds (s1 s2 source)
  (cond ((numberp s1)
         (cond ((numberp s2)
                (* s1 s2))
               ((soundp s2)
                (snd-scale s1 s2))
               (t
                (ny:error source 0 number-sound-anon s2 t))))
        ((numberp s2)
         (ny:typecheck (not (soundp s1))
           (ny:error source 0 number-sound-anon s1 t))
         (snd-scale s2 s1))
        ((and (soundp s1) (soundp s2))
         (snd-prod s1 s2))
        ((soundp s1)
         (ny:error source 0 number-sound-anon s2 t))
        (t
         (ny:error source 0 number-sound-anon s1 t))))


;; RAMP -- linear ramp from 0 to x
;;
(defun ramp (&optional (x 1))
  (ny:typecheck (not (numberp x))
    (ny:error "RAMP" 0 number-anon x))
  (let* ((duration (get-duration x)))
    (ny:set-logical-stop
      (warp-abs nil
        (at *rslt*
          (sustain-abs 1
                       (pwl duration 1 (+ duration (/ *control-srate*))))))
      x)))


(defun resample (snd rate)
  (ny:typecheck (not (or (soundp snd) (multichannel-soundp snd)))
    (ny:error "RESAMPLE" 1 '((SOUND) nil) snd t))
  (ny:typecheck (not (numberp rate))
    (ny:error "RESAMPLE" 2 '((NUMBER) "rate") rate))
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
  (multichan-expand "SCALE" #'snd-scale
    '(((NUMBER) "amt") ((SOUND) "snd")) amt snd))


(setfn s-print-tree snd-print-tree)


;; (PEAK sound-expression number-of-samples) - find peak amplitude
;
; NOTE: this used to be called s-max
; It is tempting to try using multichan-expand here to get peaks
; from multichannel sounds, but at this point the argument is just
; an expression, so we cannot tell if it is multichannel. We could
; evaluate the expression, but then we'd have a local binding and
; would retain samples in memory if we called snd-max on each channel.
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
    (ny:typecheck (/= n (length s2))
       (error (strcat "In S-MAX, unequal number of channels, got "
                      (param-to-string s1) " and " (param-to-string s2))))
    (dotimes (i n)
      (setf (aref p i) (s-max (aref s1 i) (aref s2 i))))
    p))

(defun nyq:max-2-sounds (s1 s2)
  (cond ((numberp s1)
         (cond ((numberp s2)
                (max s1 s2))
               ((soundp s2)
                (snd-maxv s2
                          (snd-const s1 (local-to-global 0.0)
                                     (snd-srate s2) (get-duration 1.0))))
               (t
                (ny:error "S-MAX" 2 number-sound-anon s2 t))))
        ((numberp s2)
         (ny:typecheck (not (soundp s1))
           (ny:error "S-MAX" 2 number-sound-anon s2 t))
         (snd-maxv s1 (snd-const s2 (local-to-global 0.0)
                       (snd-srate s1) (get-duration 1.0))))
        ((and (soundp s1) (soundp s2))
         (snd-maxv s1 s2))
        ((soundp s1)
         (ny:error "S-MAX" 2 number-sound-anon s2 t))
        (t
         (ny:error "S-MAX" 1 number-sound-anon s1 t))))


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
    (ny:typecheck (/= n (length s2))
       (error (strcat "In S-MIN, unequal number of channels, got "
                      (param-to-string s1) (param-to-string s2))))
    (cond ((/= n (length s2))
       (error "unequal number of channels in max")))
    (dotimes (i n)
      (setf (aref p i) (s-min (aref s1 i) (aref s2 i))))
    p))

(defun nyq:min-2-sounds (s1 s2)
  (cond ((numberp s1)
         (cond ((numberp s2)
                (min s1 s2))
               ((soundp s2)
                (snd-minv s2
                          (snd-const s1 (local-to-global 0.0)
                                     (snd-srate s2) (get-duration 1.0))))
               (t
                (ny:error "S-MIN" 2 number-sound-anon s2 t))))
        ((numberp s2)
         (ny:typecheck (not (soundp s1))
           (ny:error "S-MIN" 2 number-sound-anon s2 t))
         (snd-minv s1 (snd-const s2 (local-to-global 0.0)
                   (snd-srate s1) (get-duration 1.0))))
        ((and (soundp s1) (soundp s2))
         (snd-minv s1 s2))
        ((soundp s1)
         (ny:error "S-MIN" 2 number-sound-anon s2 t))
        (t
         (ny:error "S-MIN" 1 number-sound-anon s1 t))))


(defun snd-minv (s1 s2)
  (snd-scale -1.0 (snd-maxv (snd-scale -1.0 s1) (snd-scale -1.0 s2))))

; sequence macros SEQ and SEQREP are now in seq.lsp:
; 
(load "seq" :verbose NIL)


; set-logical-stop - modify the sound and return it, time is shifted and
;			 stretched
(defun set-logical-stop (snd tim)
  (ny:typecheck (not (numberp tim))
    (ny:error "SET-LOGICAL-STOP" 2 '((NUMBER) "logical stop time") tim))
  (ny:typecheck (not (or (soundp snd) (multichannel-soundp snd)))
    (ny:error "SET-LOGICAL-STOP" 1 '((SOUND) "snd") snd t))
  (multichan-expand "SET-LOGICAL-STOP" #'ny:set-logical-stop 
    '(((SOUND) "snd") ((NUMBER) "logical stop time")) snd tim))


;; NY:SET-LOGICAL-STOP - "fast" set-logical-stop: no typechecks and no
;;                       multichannel expansion
(defun ny:set-logical-stop (snd tim)
  (let ((d (local-to-global tim)))
    (snd-set-logical-stop snd d)
    snd))
  

; SET-LOGICAL-STOP-ABS - modify the sound and return it
; 
(defun set-logical-stop-abs (snd tim)
  (ny:typecheck (not (numberp tim))
    (ny:error "SET-LOGICAL-STOP-ABS" 2 '((NUMBER) "logical stop time") tim))
  (ny:typecheck (not (or (soundp snd) (multichannel-soundp snd)))
    (ny:error "SET-LOGICAL-STOP-ABS" 1 '((SOUND) "snd") snd t))
  (multichan-expand "SET-LOGICAL-STOP-ABS" #'ny:set-logical-stop-abs 
    '(((SOUND) "snd") ((NUMBER) "logical stop time")) snd tim))


(defun ny:set-logical-stop-abs (snd tim)
  (snd-set-logical-stop snd tim)
  snd)
  

(defmacro simrep (pair sound)
  `(let (_snds)
     (dotimes ,pair (push ,sound _snds))
       (sim-list _snds "SIMREP")))

(defun sim (&rest snds)
  (sim-list snds "SIM or SUM (or + in SAL)"))

(setfn sum sim)

(defun sim-list (snds source)
 (let (a b)
  (cond ((null snds)
         (snd-zero (local-to-global 0) *sound-srate*))
        ((null (cdr snds))
         (setf a (car snds))
         (ny:typecheck (not (or (numberp a) (soundp a) (multichannel-soundp a)))
           (ny:error source 0 number-sound-anon a t))
         a)
        ((null (cddr snds))
         ;; sal-plus does typechecking, then calls nyq:add2
         (sal-plus (car snds) (cadr snds)))
        (t
         (setf a (car snds))
         (ny:typecheck (not (or (numberp a) (soundp a) (multichannel-soundp a)))
           (ny:error source 0 number-sound-anon a t))
         (nyq:add2 a (sim-list (cdr snds) source))))))


(defun s-rest (&optional (dur 1.0) (chans 1))
  (ny:typecheck (not (numberp dur))
    (ny:error "S-REST" 1 '((NUMBER) "dur") dur))
  (ny:typecheck (not (integerp chans))
    (ny:error "S-REST" 2 '((INTEGER) "chans")  chans))
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
  (ny:typecheck (not (soundp warpfn))
    (ny:error "TEMPO" 0 '((SOUND) "warpfn") warpfn))
  (slope (snd-inverse warpfn (local-to-global 0) *control-srate*)))


;; (SUM-OF-ARRAYS S1 S2) - add multichannel sounds
; 
; assumes s1 & s2 are arrays of numbers and sounds
;
; result has as many channels the largest of s1, s2
; corresponding channels are added, extras are copied
; 
(defun sum-of-arrays (s1 s2)
;  (ny:typecheck (not (multichannel-soundp s1))
;    (error (strcat "In SUM or SIM (or + in SAL), at least one channel in the array contains a non-sound, got " (param-to-string s1))))
;  (ny:typecheck (not (multichannel-soundp s2))
;    (error (strcat "In SUM or SIM (or + in SAL), at least one channel in the array contains a non-sound, got " (param-to-string s2))))
  (let* ((n1 (length s1))
     (n2 (length s2))
     (n (min n1 n2))
     (m (max n1 n2))
     (result (make-array m))
     (big-s (if (> n1 n2) s1 s2))
     v1 v2)
    
    (dotimes (i n)
      (setf v1 (aref s1 i) v2 (aref s2 i))
      (setf (aref result i) 
        (cond ((numberp v1)
               (if (numberp v2) (+ v1 v2) (snd-offset v2 v1)))
              ((numberp v2)
               (if (numberp v1) (+ v1 v2) (snd-offset v1 v2)))
              (t
               (nyq:add-2-sounds v1 v2)))))
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
 `(progv '(*WARP*)
     (let ((wp ,x))
       (list (list 0.0 1.0
              (cond ((warp-function *WARP*)
                     (ny:typecheck (not (soundp wp))
                       (ny:error "WARP" 1 '((SOUND) "warp function") wp))
                     (snd-compose (shift-time (warp-function *WARP*) 
                                              (- (warp-time *WARP*)))
                                  (snd-scale (warp-stretch *WARP*) wp)))
                    (t
                     (ny:typecheck (not (soundp wp))
                       (ny:error "WARP" 1 '((SOUND) "warp function") wp))
                     (snd-offset (snd-scale (warp-stretch *WARP*) wp)
                                 (warp-time *WARP*)))))))
     ,s))


(defmacro warp-abs (x s)
 `(progv '(*WARP*)
     (let ((wp ,x))
       (ny:typecheck (and wp (not (soundp wp)))
         (ny:error "WARP-ABS" 1 '((NULL SOUND) NIL) wp))
       (list (list 0.0 1.0 wp)))
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
;; types should be a list of type info for each arg, where type info is:
;;   (arg1-info arg2-info ...), where each arg-info is
;;   (valid-type-list name-or-nil), where valid-type-list is a list 
;;      of valid types from among NUMBER, SOUND, POSITIVE (number > 0),
;;      NONNEGATIVE (number >= 0), INTEGER, STEP, STRING,
;;      POSITIVE-OR_NULL (a positive number or nil),
;;      INT-OR-NULL (integer or nil), or NULL (the value can be nil).
;;      It is implied that arrays of these are valid too.  name-or-nil 
;;      is the parameter name as a string if the parameter name should 
;;      be printed, or NIL if the parameter name should not be printed.
;;      There can be at most 2 elements in valid-type-list, and if 
;;      there are 2 elements, the 2nd one must be SOUND. For example, 
;;      arg-info '((NUMBER SOUND) "cutoff") might generate the error
;;          In LOPASS8, 2nd argument (cutoff) must be a number, sound
;;          or array thereof, got "bad-value"
;;
;; Many existing Nyquist plug-ins require the old version of multichan-expand,
;; so in Audacity we need to support both the old and new versions.
(defun multichan-expand (&rest args)
  (if (stringp (first args))
      (apply 'multichan-expand-new args)
      (apply 'multichan-expand-old args)))

;; Legacy version:
(defun multichan-expand-old (fn &rest args)
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

;; The new (Nyquist 3.15) version:
(defun multichan-expand-new (src fn types &rest args)
  (let (chan len newlen result prev typ (index 0) nonsnd)
    ; len is a flag as well as a count
    (dolist (a args)
      (setf typ (car types) types (cdr types))
      ;; we only report argument position when there is more than one.
      ;; index tracks argument position, where 0 means no position to report
      (if (> (length args) 1) (setf index (1+ index)))
      (setf nonsnd (caar typ)) ;; if non-sound type allowed, what is it?
      ;; compute the length of any array argument, and typecheck all of them
      (cond ((arrayp a)
             (setf newlen (length a))
             (ny:typecheck (and len (/= len newlen))
               (error (strcat "In " src
                 ", two arguments are multichannels of differing length, got "
                 (param-to-string prev) ", and " (param-to-string a))))
             (dotimes (i newlen)
               (setf chan (aref a i))
               (cond ((and (eq nonsnd 'NUMBER) (numberp chan)))
                     ((and (member 'SOUND (car typ)) (soundp chan)))
                     ((and (eq nonsnd 'STEP) (numberp chan)))
                     ((and (eq nonsnd 'POSITIVE) (numberp chan) (> chan 0)))
                     ((and (eq nonsnd 'POSITIVE-OR-NULL)
                           (or (and (numberp chan) (> chan 0)) (null chan))))
                     ((and (eq nonsnd 'NONNEGATIVE) (numberp chan) (>= chan 0)))
                     ((and (eq nonsnd 'INTEGER) (integerp chan)))
                     ((and (eq nonsnd 'STRING) (stringp chan)))
                     ((and (eq nonsnd 'NULL) (null chan)))
                     ((and (eq nonsnd 'INT-OR-NULL)
                           (or (integerp chan) (null chan))))
                     (t (ny:error src index typ a t))))
             (setf prev a)
             (setf len newlen))
            ((and (eq nonsnd 'NUMBER) (numberp a)))
            ((and (member 'SOUND (car typ)) (soundp a)))
            ((and (eq nonsnd 'STEP) (numberp a)))
            ((and (eq nonsnd 'POSITIVE) (numberp a) (>= a 0)))
            ((and (eq nonsnd 'POSITIVE-OR-NULL)
                  (or (and (numberp a) (> a 0)) (null a))))
            ((and (eq nonsnd 'NONNEGATIVE) (numberp a) (>= a 0)))
            ((and (eq nonsnd 'INTEGER) (integerp a)))
            ((and (eq nonsnd 'STRING) (stringp a)))
            ((and (eq nonsnd 'NULL) (null a)))
            ((and (eq nonsnd 'INT-OR-NULL)
                  (or (integerp a) (null a))))
            (t
             (ny:error src index typ a t))))
    (cond (len
           (setf result (make-array len))
           ; for each channel, call fn with args
           (dotimes (i len)
             (setf (aref result i)
                   (apply fn
                     (mapcar
                       #'(lambda (a) ; take i'th entry or replicate:
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
(defun select-implementation-1-1 (source fns snd sel1 &rest others)
  (ny:typecheck (not (soundp snd))
    (ny:error source 1 '((SOUND) nil) snd t))
  (cond ((numberp sel1)
         (apply (aref fns 0) (cons snd (cons sel1 others))))
        ((soundp sel1)
         (apply (aref fns 1) (cons snd (cons sel1 others))))
        (t
         (ny:error source 2 number-sound-anon sel1 t))))


;; SELECT-IMPLEMENTATION-1-2 -- 1 sound arg, 2 selectors
;;
;; choose implementation according to args 2 and 3. In this implementation,
;; since we have two arguments to test for types, we return from prog
;; if we find good types. That way, we can fall through the decision tree
;; and all paths lead to one call to ERROR if good types are not found.
;;
(defun select-implementation-1-2 (source fns snd sel1 sel2 &rest others)
  (prog ()
    (ny:typecheck (not (soundp snd))
      (ny:error source 1 '((SOUND) nil) snd t))
    (cond ((numberp sel2)
           (cond ((numberp sel1)
                  (return (apply (aref fns 0)
                                 (cons snd (cons sel1 (cons sel2 others))))))
                 ((soundp sel1)
                  (return (apply (aref fns 1)
                                 (cons snd (cons sel1 (cons sel2 others))))))))
          ((soundp sel2)
           (cond ((numberp sel1)
                  (return (apply (aref fns 2)
                          (cons snd (cons sel1 (cons sel2 others))))))
                 ((soundp sel1)
                  (return (apply (aref fns 3)
                          (cons snd (cons sel1 (cons sel2 others)))))))))
    (ny:typecheck (not (or (numberp sel1) (soundp sel1)))
      (ny:error src 2 number-sound-anon sel1 t)
      (ny:error src 3 number-sound-anon sel2 t))))


;; some waveforms

(setf *saw-table* (pwlvr -1 1 1))		; eh, creepy way to get 2205 samples.
(setf *saw-table* (list *saw-table* (hz-to-step 1) T))

(setf *tri-table* (pwlvr -1 0.5 1 0.5 -1))
(setf *tri-table* (list *tri-table* (hz-to-step 1) T))

(setf *id-shape*  (pwlvr -1 2 1 .01 1))	            ; identity

(setf *step-shape* (seq (const -1) (const 1 1.01)))  ; hard step at zero

(defun exp-dec (hold halfdec length)
  (ny:typecheck (not (numberp hold))
    (ny:error "EXP-DEC" 1 '((NUMBER) "hold") hold))
  (ny:typecheck (not (numberp halfdec))
    (ny:error "EXP-DEC" 2 '((NUMBER) "halfdec") halfdec))
  (ny:typecheck (not (numberp length))
    (ny:error "EXP-DEC" 3 '((NUMBER) "length") length))
  (let* ((target (expt 0.5 (/ length halfdec)))
     (expenv (pwev 1 hold 1 length target)))
    expenv)
)

;;; operations on sounds

(defun diff (x &rest y) (diff-list x y "DIFF (or - in SAL)"))

(defun diff-list (x y source)
  (cond ((and (numberp x) (numberp (car y)) (null (cdr y)))
         (- x (car y))) ;; this is a fast path for the common case
        (y (sal-plus x (nyq:prod2 -1 (car y) source) source))
        (t (nyq:prod2 -1 x source))))


; compare-shape is a shape table -- origin 1.
(defun compare (x y &optional (compare-shape *step-shape*))
  (ny:typecheck (not (or (soundp x) (soundp y)))
    (error "In COMPARE, either first or second argument must be a sound"))
  (ny:typecheck (not (soundp compare-shape))
    (ny:error "COMPARE" 3 '((SOUND) "compare-shape") compare-shape))
  (ny:typecheck (not (or (soundp x) (numberp x)))
    (ny:error "COMPARE" 1 '((SOUND NUMBER) nil) x))
  (ny:typecheck (not (or (soundp y) (numberp y)))
    (ny:error "COMPARE" 2 '((SOUND NUMBER) nil) y))
  (let ((xydiff (diff-list x (list y) "COMPARE")))
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
(defun tapv (snd offset vardelay maxdelay)
  (multichan-expand "TAPV" #'snd-tapv
    '(((SOUND) "snd") ((NUMBER) "offset") 
      ((SOUND) "vardelay") ((NUMBER) "maxdelay")) 
    snd offset vardelay maxdelay))

(defun tapf (snd offset vardelay maxdelay)
  (multichan-expand "TAPF" #'snd-tapf
    '(((SOUND) "snd") ((NUMBER) "offset") 
      ((SOUND) "vardelay") ((NUMBER) "maxdelay")) 
    snd offset vardelay maxdelay))


;; autoload functions -- SELF-MODIFYING CODE!
;; generate functions that replace themselves by loading more files
;; and then re-calling themselves as if they were already loaded
;;
(defun autoload (filename &rest fns)
  ;; filename is the file to load (a string) from the current path
  ;; fns are symbols to be defined as function that will load filename
  ;;     the first time any one is called, and it is assumed that
  ;;     filename will define each function in fns, so the called
  ;;     function can be called again to execute the real implementation
  (let ((cp (current-path)))
    (cond ((string-equal cp "./") ;; this is the typical case
           (setf cp (setdir "."))))
    ;; make sure cp ends in file separator
    (cond ((not (equal (char cp (1- (length cp))) *file-separator*))
           (setf cp (strcat cp (string *file-separator*)))))
    (setf cp (strcat cp filename))
    (dolist (fn fns)
      (eval `(defun ,fn (&rest args)
               (autoload-helper ,cp ',fn args))))))


(defun autoload-helper (path fn args)
  (if (abs-env (sal-load path))
      (apply fn args)
      (error (strcat "Could not load " path))))


(autoload "spec-plot.lsp" 'spec-plot)

(autoload "spectral-analysis.lsp" 'sa-init)

