;; stk.lsp -- STK-based instruments
;;
;; currently clarinet and saxophony are implemented

(defun instr-parameter (parm)
  ;; coerce parameter into a *sound-srate* signal
  (cond ((numberp parm)
         (stretch 30 (control-srate-abs *sound-srate* (const (float parm)))))
        (t
         (force-srate *sound-srate* parm))))


(defun clarinet (step breath-env)
  (snd-clarinet (step-to-hz step) (force-srate *sound-srate* breath-env) *sound-srate*))


(defun clarinet-freq (step breath-env freq-env)
  ;; note that the parameters are in a different order -- I defined 
  ;; clarinet-freq this way so that the first two parameters are always
  ;; step and breath. I didn't redo snd-clarinet-freq.
  (snd-clarinet_freq (step-to-hz step) 
                (instr-parameter breath-env)
                (instr-parameter freq-env)
                *sound-srate*))



(defun clarinet-all (step breath-env freq-env vibrato-freq vibrato-gain reed-stiffness noise)
  ;; note that the parameters are not in the same order as snd-clarinet-all
  (setf breath-env (instr-parameter breath-env))
  (setf freq-env (instr-parameter freq-env))
  (setf reed-stiffness (instr-parameter reed-stiffness))
  (setf noise (instr-parameter noise))
  (snd-clarinet_all (step-to-hz step)
                    breath-env freq-env 
                    ;; STK scales 1.0 to 12Hz. Scale here so vibrato-freq is in Hz
                    (/ vibrato-freq 12.0) vibrato-gain
                    reed-stiffness noise 
                    *sound-srate*))


(defun sax (step breath-env)
  (snd-sax (step-to-hz step) (force-srate *sound-srate* breath-env) *sound-srate*))

(defun sax-freq (step breath-env freq-env)
  (snd-sax_freq (step-to-hz step)
          (instr-parameter breath-env)
          (instr-parameter freq-env)
          *sound-srate*))

(defun sax-all (step breath-env freq-env vibrato-freq vibrato-gain reed-stiffness noise blow-pos reed-table-offset)
  (snd-sax_all (step-to-hz step)
	       (instr-parameter freq-env)
               (instr-parameter breath-env)
               (instr-parameter (/ vibrato-freq 12.0))
               (instr-parameter vibrato-gain)
               (instr-parameter reed-stiffness)
               (instr-parameter noise)
               (instr-parameter blow-pos)
               (instr-parameter reed-table-offset)
               *sound-srate*)
)

; instr-parameter already defined in stk.lsp

(defun flute (step breath-env)
  (snd-flute (step-to-hz step) (force-srate *sound-srate* breath-env) *sound-srate*))
 
(defun flute-freq (step breath-env freq-env)
  (snd-flute_freq (step-to-hz step) 
		  (instr-parameter breath-env)
		  (instr-parameter freq-env)
		  *sound-srate*))

(defun flute-all (step breath-env freq-env vibrato-freq vibrato-gain jet-delay noise)
  ;; note that the parameters are not in the same order as snd-clarinet-all
  (setf breath-env (instr-parameter breath-env))
  (setf freq-env (instr-parameter freq-env))
  (setf jet-delay (instr-parameter jet-delay))
  (setf noise (instr-parameter noise))
  (snd-flute_all (step-to-hz step)
                    breath-env freq-env 
                    ;; STK scales 1.0 to 12Hz. Scale here so vibrato-freq is in Hz
                    (/ vibrato-freq 12.0) vibrato-gain
                    jet-delay noise 
                    *sound-srate*))


(defun bowed (step bowpress-env)
  (snd-bowed (step-to-hz step) (force-srate *sound-srate* bowpress-env) *sound-srate*))

(defun bowed-freq (step bowpress-env freq-env)
  (snd-bowed_freq (step-to-hz step)
		  (instr-parameter bowpress-env)
		  (instr-parameter freq-env)
		  *sound-srate*))

(defun mandolin (step dur &optional (detune 4.0))
  (let ((d (get-duration dur)))
    (snd-mandolin *rslt* (step-to-hz step) d 1.0 detune *sound-srate*)))

(defun wg-uniform-bar (step bowpress-env)
  (snd-bandedwg (step-to-hz step) (force-srate *sound-srate* bowpress-env) 0 *sound-srate*))

(defun wg-tuned-bar (step bowpress-env)
  (snd-bandedwg (step-to-hz step) (force-srate *sound-srate* bowpress-env) 1 *sound-srate*))

(defun wg-glass-harm (step bowpress-env)
  (snd-bandedwg (step-to-hz step) (force-srate *sound-srate* bowpress-env) 2 *sound-srate*))

(defun wg-tibetan-bowl (step bowpress-env)
  (snd-bandedwg (step-to-hz step) (force-srate *sound-srate* bowpress-env) 3 *sound-srate*))
 
(defun modalbar (preset step duration)
   (let ((preset (case preset
			(MARIMBA 0)
			(VIBRAPHONE 1)
			(AGOGO 2)
			(WOOD1 3)
			(RESO 4)
			(WOOD2 5)
			(BEATS 6)
			(TWO-FIXED 7)
			(CLUMP 8)
			(t (error (format nil "Unknown preset for modalbar %A" preset)))))
	 (d (get-duration duration)))
     (snd-modalbar *rslt* (step-to-hz step) preset d *sound-srate*)))

(defun sitar (step dur)
  (let ((d (get-duration dur)))
    (snd-sitar *rslt* (step-to-hz step) d *sound-srate*)))

(defun nyq:nrev (snd rev-time mix)
  (snd-stkrev 0 snd rev-time mix))

(defun nyq:jcrev (snd rev-time mix)
  (snd-stkrev 1 snd rev-time mix))

(defun nyq:prcrev (snd rev-time mix)
  (snd-stkrev 2 snd rev-time mix))

(defun nrev (snd rev-time mix)
  (multichan-expand "NREV" #'nyq:nrev 
    '(((SOUND) "snd") ((NUMBER) "rev-time") ((NUMBER) "mix"))
    snd rev-time mix))

(defun jcrev (snd rev-time mix)
  (multichan-expand "JCREV" #'nyq:jcrev 
    '(((SOUND) "snd") ((NUMBER) "rev-time") ((NUMBER) "mix"))
    snd rev-time mix))

(defun prcrev (snd rev-time mix)
  (multichan-expand "PRCREV" #'nyq:prcrev 
    '(((SOUND) "snd") ((NUMBER) "rev-time") ((NUMBER) "mix"))
    snd rev-time mix))

(defun nyq:chorus (snd depth freq mix &optional (base-delay 6000))
  (snd-stkchorus snd base-delay depth freq mix))

(defun stkchorus (snd depth freq mix &optional (base-delay 6000))
  (multichan-expand "STKCHORUS" #'nyq:chorus 
    '(((SOUND) "snd") ((NUMBER) "depth") ((NUMBER) "freq") ((NUMBER) "mix")
      ((INTEGER) "base-delay"))
    snd depth freq mix base-delay))

(defun nyq:pitshift (snd shift mix)
  (snd-stkpitshift snd shift mix))

(defun pitshift (snd shift mix)
  (multichan-expand "PITSHIFT" #'nyq:pitshift 
    '(((SOUND) "snd") ((NUMBER) "shift") ((NUMBER) "mix"))
    snd shift mix))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pass in rates of increase/decrease in begin/end... this is like noteOn and noteOff
;
; STK uses setRate but the actual ramp time is also a function of the sample rate.
; I will assume the clarinet was run at 44100Hz and fix things so that the envelope
; is sample-rate independent.
;
; STK seemed to always give a very fast release, so I changed the numbers so that
; note-off values from 0.01 to 1 give an interesting range of articulations.
;
; IMPORTANT: the returned envelope is 0.1s longer than dur. There is 0.1s of silence
; at the end so that the clarinet can "ring" after the driving force is removed.
;
(defun stk-breath-env (dur note-on note-off)
  (let* ((target (+ 0.55 (* 0.3 note-on)))
         (on-time (/ (* target 0.0045) note-on))
         (off-time (/ (* target 0.02) note-off)))
    ;(display "clarinet-breath-env" target on-time off-time)
    (pwl on-time target
         (- dur off-time) target
         dur 0 (+ dur 0.1))))


