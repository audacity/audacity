$nyquist plug-in
$version 4
$type process
$preview linear
$preview selection
$name (_ "Adjustable Fade")
$debugbutton false
$author (_ "Steve Daulton")
$release 3.0.4-2
$copyright (_ "GNU General Public License v2.0 or later")

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control TYPE (_ "Fade Type") choice (("Up" (_ "Fade Up"))
                                      ("Down" (_ "Fade Down"))
                                      ("SCurveUp" (_ "S-Curve Up"))
                                      ("SCurveDown" (_ "S-Curve Down"))) 0
$control CURVE (_ "Mid-fade Adjust (%)") real "" 0 -100 100
$control UNITS (_ "Start/End as") choice (("Percent" (_ "% of Original"))
                                          ("dB" (_ "dB Gain"))) 0 
$control GAIN0 (_ "Start (or end)") float-text "" 0 nil nil
$control GAIN1 (_ "End (or start)") float-text "" 100 nil nil
$control PRESET (_ "Handy Presets (override controls)") choice (("None" (_ "None Selected"))
                                                                ("LinearIn" (_ "Linear In"))
                                                                ("LinearOut" (_ "Linear Out"))
                                                                ("ExponentialIn" (_ "Exponential In"))
                                                                ("ExponentialOut" (_ "Exponential Out"))
                                                                ("LogarithmicIn" (_ "Logarithmic In"))
                                                                ("LogarithmicOut" (_ "Logarithmic Out"))
                                                                ("RoundedIn" (_ "Rounded In"))
                                                                ("RoundedOut" (_ "Rounded Out"))
                                                                ("CosineIn" (_ "Cosine In"))
                                                                ("CosineOut" (_ "Cosine Out"))
                                                                ("SCurveIn" (_ "S-Curve In"))
                                                                ("SCurveOut" (_ "S-Curve Out"))) 0

;;; Preview takes the entire selection so that we know the correct
;;; selection length, but preview only needs to process preview length."
(defun get-input (sig)
  (if *previewp*
      (multichan-expand #'trim-input sig)
      sig))


;;; Trim input when previewing."
(defun trim-input (sig)
  (let ((dur (min (get-duration 1)
                  (get '*project* 'preview-duration))))
    (setf sig (extract-abs 0 dur sig))))


;;; Check gain values are in valid range.
(defun validate-gain ()
  (setf err (format nil (_ "Error~%~%")))
  (if (= UNITS 0)  ;percentage values
      (cond
        ((or (< GAIN0 0)(< GAIN1 0))
          (throw 'err (format nil (_ "~aPercentage values cannot be negative.") err)))
        ((or (> GAIN0 1000)(> GAIN1 1000))
          (throw 'err (format nil (_ "~aPercentage values cannot be more than 1000 %.") err))))
      (cond   ;dB values
        ((or (> GAIN0 100)(> GAIN1 100))
          (throw 'err (format nil (_ "~adB values cannot be more than +100 dB.~%~%~
                                      Hint: 6 dB doubles the amplitude~%~
                                      -6 dB halves the amplitude.") err))))))


;;; Select and apply fade
(defun fade (sig)
  (when (= PRESET 0)
    ; Can't use widget validation for gain as range depends on units.
    (validate-gain))
  (psetq curve-ratio (/ CURVE 100.0)
         g0 (gainscale GAIN0)
         g1 (gainscale GAIN1))
  (mult (get-input sig)
    (case PRESET
      (0  (case TYPE  ; Custom fade
            (0 (simple (min g0 g1) (max g0 g1) curve-ratio))
            (1 (simple (max g0 g1) (min g0 g1) curve-ratio))
            (2 (raised-cos (min g0 g1)(max g0 g1) curve-ratio))
            (T (raised-cos (max g0 g1) (min g0 g1) curve-ratio))))
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
(defun simple (g0 g1 curve-ratio)
  (cond 
    ((= g0 g1) g0)  ; amplify
    ((and (> curve-ratio 0)(< curve-ratio 0.5))  ; +ve curve less than 0.5, lin to cosine
      (let ((curve-ratio (* curve-ratio 2)))
        (sim (mult (scale-curve g0 g1 (linear g0 g1))
                   (- 1 curve-ratio))  ; linear
             (mult (scale-curve g0 g1 (cosine-curve g0 g1))
                   curve-ratio))))  ; cosine curve
    ((> curve-ratio 0)
      (cos-curve g0 g1 (- 1.5 curve-ratio)))              ; +ve curve > 0.5
    (t (simple-curve g0 g1 (- 1 (* 2 curve-ratio))))))    ; -ve curve


;;; Linear fade to the power of 'pow'.
(defun simple-curve (g0 g1 pow)
  (curve-adjust g0 g1 pow (linear g0 g1)))


;;; Cosine fade to the power of 'pow'.
(defun cos-curve (g0 g1 pow)
  (curve-adjust g0 g1 pow (cosine-curve g0 g1)))


(defun curve-adjust (g0 g1 pow env)
  (scale-curve g0 g1
    (if (= pow 1)
        env
        (snd-exp (mult pow (snd-log env))))))


;;; Scale curves to min, max.
(defun scale-curve (g0 g1 env)
  (sum (min g0 g1)
       (mult (abs (- g0 g1)) env)))


;;; Cosine curve.
(defun cosine-curve (g0 g1)
  (let ((step (hz-to-step (/ 0.25 (get-duration 1))))
        (phase (if (> g0 g1) 90 0)))
    (osc step 1 *sine-table* phase)))


;;; Linear fade in, out.
(defun linear (g0 g1)
  (control-srate-abs *sound-srate*
    (if (> g0 g1)         ; g0 = g1 does not occur here.
        (pwlv 1 1 0)      ; fade out
        (pwlv 0 1 1))))   ; else fade in


;;; Raised cosine fades.
(defun raised-cos (g0 g1 curve-ratio)
  (setq curve-ratio
    (if (> curve-ratio 0)
        (exp-scale-mid (* 2 curve-ratio))       ; mid-point -3dB @ Adjust = 50%
        (exp-scale-mid (* 1.63 curve-ratio))))  ; mid-point -12dB @ Adjust = -50%
  (setf env
    ;; sound srate required for accuracy.
    (control-srate-abs *sound-srate*
      (cond
        ((= g0 g1) g0)    ; amplify
        ((> g0 g1)        ; fade down
          (snd-exp 
            (mult (pwlv (- 1 curve-ratio) 1 1)
                  (snd-log (raised-cosin 90)))))
        (t (snd-exp       ; fade up
             (mult (pwlv 1 1 (- 1 curve-ratio))
                   (snd-log (raised-cosin -90))))))))
  (sum (min g0 g1)
       (mult (abs (- g0 g1)) env)))


;;; Raised cosine curve.
(defun raised-cosin (phase)
  (let ((hz (hz-to-step (/ (get-duration 2)))))
    (mult 0.5 
          (sum 1 (osc hz 1 *sine-table* phase)))))


;;; log or exponential curve scaled 0 to 1
;;; x is the minimum level in dB before scaling.
(defun log-exp-curve (x direction)
  (control-srate-abs *sound-srate*
    (let ((x (db-to-linear x)))
      ;; If direction=0 fade-in else fade-out
      (if (= direction 0)
          (setf env (pwev x 1 1))
          (setf env (pwev 1 1 x)))
      (mult (/ (- 1 x))  ; normalize to 0 dB
            (diff env x)))))  ; drop down to silence


;;; Curve scaling for S-curve.
(defun exp-scale-mid (x)
  (let ((e (exp 1.0)))
    (/ (- (exp (- 1 x)) e)
       (- 1 e))))


(defun gainscale (gain)
  (if (= UNITS 0)  ; percent
      (/ gain 100.0)
      (db-to-linear gain)))


(catch 'err (fade *track*))
