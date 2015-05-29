;nyquist plug-in
;version 4
;type process
;preview linear
;preview selection
;categories "http://lv2plug.in/ns/lv2core#MixerPlugin"
;name "Adjustable Fade..."
;action "Applying Fade..."
;author "Steve Daulton"
;copyright "Released under terms of the GNU General Public License version 2"

;; adjustable-fade.ny by Steve Daulton Dec 2012
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html 
;;
;; For information about writing and modifying Nyquist plug-ins:
;; http://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;control type "Fade Type" choice "Fade Up,Fade Down,S-Curve Up,S-Curve Down" 0
;control curve "Mid-fade Adjust (%)" real "" 0 -100 100
;control units "Start/End as" choice "% of Original,dB Gain" 0 
;control gain-string-0 "Start (or end)" string "" "0" ""
;control gain-string-1 "End (or start)" string "" "100" ""
;control preset "   Handy Presets\n(override controls)" choice "None Selected,Linear In,Linear Out,Exponential In,Exponential Out,Logarithmic In,Logarithmic Out,Rounded In,Rounded Out,Cosine In,Cosine Out,S-Curve In,S-Curve Out" 0


(defun get-input (sig)
"Preview takes the entire selection so that we know the correct
selection length, but preview only needs to process preview length."
  (if (get '*track* 'view)  ;NIL if preview
      sig
      (multichan-expand #'trim-input sig)))

(defun trim-input (sig)
"Trim input when previewing."
  (let ((dur (min (get-duration 1)
                  (get '*project* 'preview-duration))))
    (setf sig (extract-abs 0 dur *track*))))


(setq err "")
; bad things may happen outside of the slider range.
(setq curve (min 1 (max -1 (/ curve 100.0))))

;;; Convert string to value
(defun string-to-val (string)
  (setq val-array 
    (read (make-string-input-stream (format nil "(~a)" string))))
  (if (and (= (length val-array) 1)
           (numberp (car val-array)))
      (let ((val (float (car val-array))))
        (case units
          (0 (/ val 100))
          (t (db-to-linear val))))))

;;; invalid string error
(defun invalid-string (x y)
  (unless x 
    (setf err (format nil "~aYou entered \"~a\"~%~
      \"Start (or end):\" must be one number.~%"
      err
      gain-string-0)))
  (unless y
    (setf err (format nil "~aYou entered \"~a\"~%~
      \"End (or start):\" must be one number.~%"
      err gain-string-1)))
  err)

;;; invalid values
(defun check-values (x y)
  (when (or (< x 0)(< y 0))
    (setf err (format nil "~a% values cannot be negative.~%" err))))

;;; select and apply fade
(defun fade (sig type curve g0 g1)
  (mult (get-input sig)
    (case preset
      (0  (case type                  ; Custom fade
            (0 (simple (min g0 g1) (max g0 g1) curve))
            (1 (simple (max g0 g1) (min g0 g1) curve))
            (2 (raised-cos (min g0 g1)(max g0 g1) curve))
            (T (raised-cos (max g0 g1) (min g0 g1) curve))))
      (1  (linear 0 1))               ; Linear In
      (2  (linear 1 0))               ; Linear Out
      (3  (log-exp-curve -60 0))      ; Exponential In
      (4  (log-exp-curve -60 1))      ; ExponentialOut
      (5  (log-exp-curve 15.311 0))   ; Logarithmic In
      (6  (log-exp-curve 15.311 1))   ; Logarithmic Out
      (7  (simple-curve 0 1 0.5))     ; Rounded In
      (8  (simple-curve 1 0 0.5))     ; Rounded Out
      (9  (cosine-curve 0 1))         ; Cosine In
      (10 (cosine-curve 1 0))         ; Cosine Out
      (11 (raised-cos 0 1 0.0))       ; S-Curve In
      (t  (raised-cos 1 0 0.0)))))    ; S-Curve Out

;;; Simple Curve:
;;; Use cosine for + values and linear for -ve.
(defun simple (g0 g1 curve)
  (cond 
    ((= g0 g1) g0)                    ; amplify
    ((and (> curve 0)(< curve 0.5))   ; +ve curve less than 0.5, lin to cosine
      (let ((curve (* curve 2)))
        (sim 
          (mult (- 1 curve)
            (scale-curve g0 g1 (linear g0 g1)))           ; linear
          (mult curve
            (scale-curve g0 g1 (cosine-curve g0 g1))))))  ; cosine curve
    ((> curve 0)
      (cos-curve g0 g1 (- 1.5 curve)))                    ; +ve curve > 0.5
    (t (simple-curve g0 g1 (- 1 (* 2 curve))))))          ; -ve curve

;;; linear fade to the power of pow
(defun simple-curve (g0 g1 pow)
  (curve-adjust g0 g1 pow
    (linear g0 g1)))

;;; cosine fade to the power of pow
(defun cos-curve (g0 g1 pow)
  (curve-adjust g0 g1 pow
    (cosine-curve g0 g1)))

(defun curve-adjust (g0 g1 pow env)
  (scale-curve g0 g1
    (if (= pow 1)
        env
        (snd-exp
          (mult pow
            (snd-log env))))))

;;; scale curves to min, max
(defun scale-curve (g0 g1 env)
  (sum (min g0 g1)
    (mult (abs (- g0 g1)) env)))

;;; cosine curve
(defun cosine-curve (g0 g1)
  (let ((step (hz-to-step (/ 0.25 (get-duration 1))))
        (phase (if (> g0 g1) 90 0)))
    (osc step 1 *sine-table* phase)))

;;; linear fade in, out
(defun linear (g0 g1)
  (control-srate-abs *sound-srate*
    (if (> g0 g1)                         ; g0 = g1 does not occur here.
        (pwlv 1 1 0)                      ; fade out
        (pwlv 0 1 1))))                   ; else fade in

;;; raised cosine fades
(defun raised-cos (g0 g1 curve)
  (setq curve
    (if (> curve 0)
        (exp-scale-mid (* 2 curve))       ; mid-point -3dB @ Adjust = 50%
        (exp-scale-mid (* 1.63 curve))))  ; mid-point -12dB @ Adjust = -50%
  (setf env
    (control-srate-abs *sound-srate*      ; sound srate required for accuracy.
      (cond
        ((= g0 g1) g0)                    ; amplify
        ((> g0 g1)                        ; fade down
          (snd-exp 
            (mult (pwlv (- 1 curve) 1 1)
              (snd-log (raised-cosin 90)))))
        (t (snd-exp                       ; fade up
             (mult (pwlv 1 1 (- 1 curve))
               (snd-log (raised-cosin -90))))))))
  (sum (min g0 g1)
    (mult (abs (- g0 g1)) env)))

;;; raised cosine curve
(defun raised-cosin (phase)
  (let ((hz (hz-to-step (/ (get-duration 2)))))
    (mult 0.5 
      (sum 1 
        (osc hz 1 *sine-table* phase)))))

;;; log or exponential curve scaled 0 to 1
;;; x is the minimum level in dB before scaling
(defun log-exp-curve (x direction)
  (control-srate-abs *sound-srate*
    (let ((x (db-to-linear x)))
      ;; If direction=0 fade-in else fade-out
      (if (= direction 0)
        (setf env (pwev x 1 1))
        (setf env (pwev 1 1 x)))      
      (mult (/ (- 1 x))                   ; normalize to 0 dB
        (diff env x)))))                  ; drop down to silence

;;; curve scaling for S-curve
(defun exp-scale-mid (x)
  (let ((e (exp 1.0)))
    (/ (- (exp (- 1 x)) e)
      (- 1 e))))

(let ((gain0 (string-to-val gain-string-0))
      (gain1 (string-to-val gain-string-1)))
  (if (and gain0 gain1)
    (check-values gain0 gain1)
    (setf err (format nil "~a" (invalid-string gain0 gain1))))
  (if (= (length err) 0)
      (fade *track* type curve gain0 gain1)
      (format nil "Error.~%~a." err)))
