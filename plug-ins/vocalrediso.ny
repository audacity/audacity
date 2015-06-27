;nyquist plug-in
;version 4
;type process
;categories "http://lv2plug.in/ns/lv2core#MixerPlugin"
;name "Vocal Reduction and Isolation..."
;action "Applying Action..."
;author "Robert Haenggi"
;copyright "Released under terms of the GNU General Public License version 2"
;;
;; vocrediso.ny, based on rjh-stereo-tool.ny
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;; Plug-in version 1.56, June 2015
;; Requires Audacity 2.1.1  or later, developed under Audacity 2.1.1
;;
;control action "Action" choice "Remove Vocals,Isolate Vocals,Isolate Vocals and Invert,Remove Center,Isolate Center,Isolate Center and Invert,Remove Center Classic: Mono,Analyze" 0
(setf rotation 0.0)
;;control rotation "Rotation (Degrees)" real "" 0 -180 180
;control strength "Strength" real "" 1.0 0.0 50.0
;control low-transition "Low Cut for Vocals (Hz)" real "" 120 1 24000
;control high-transition "High Cut for Vocals (Hz)" real "" 9000 1 24000
;
;preview linear
; 
;; make aref shorter
(defmacro  : (array index) (backquote (aref ,array ,index)))
;;
;; input corrections
(defmacro limit (symbol lower upper)
  (backquote (min ,upper (max ,lower ,symbol))))
;;;
;;; Some statistical functions
;;
;; Running Sum
(defun sigma (sig)
  (snd-biquad sig 1 0 0 1 0 0 0))
;;
;; Compares two sounds (Y = A + B * X)
(defun least-squares-xy (x y &key show)
  (let* ((n (float (min (snd-length x ny:all)
                           (snd-length y ny:all))))
         (t-n (/ (1- n) (snd-srate x)))
         (bar-x (* (/ n) (snd-sref (sigma x) t-n)))
         (bar-y (* (/ n) (snd-sref (sigma y) t-n)))
         (x (diff x bar-x))
         (y (diff y bar-y))
         (pos-x (max (snd-sref (sigma (s-abs x)) t-n) 1e-17))
         (pos-y (max (snd-sref (sigma (s-abs y)) t-n) 1e-17))
         (pos-xy (cond
            ((>  pos-x pos-y) (- (/  pos-y pos-x) 1)) 
            (t (- 1 (/ pos-x pos-y)))))
         (s-xy (* (/ n) (snd-sref (sigma (prod x y)) t-n)))
         (s-x2 (* (/ n) (snd-sref (sigma (prod x x)) t-n)))
         (s-y2 (* (/ n) (snd-sref (sigma (prod y y)) t-n)))
         (r (/ s-xy (max 1e-17 (sqrt (* s-x2 s-y2)))))
         (r2 (expt r 2.0))
         (a1 (cond 
               ((= s-x2 0) 0)
               ((= s-xy 0) 0)
               (t (/ s-xy s-x2))))
         (a0 (- bar-y (* a1 bar-x)))) 
   (if show (format t 
"Average x: ~a, y: ~a
Covariance x y: ~a
Average variance x: ~a, y: ~a
Standard deviation x: ~a, y: ~a
Coefficient of correlation: ~a
Coefficient of determination: ~a 
Variation of residuals: ~a
y equals ~a plus ~a times x~%"
bar-x bar-y s-xy s-x2 s-y2 (sqrt s-x2) (sqrt s-y2) r r2 (* s-y2 (- 1 r2)) a0 a1)) 
   (list r r2 pos-xy  a0 a1)))
;;
(defun between (val low high)
  (and (> val low) (<= val high)))
;;
;; Summary for "Analyse", fed with coeff. of correlation
(defun summary (analysis &aux (corr (car analysis)) (pan-position (third analysis)))
  (format nil "Pan position: ~a~%The left and right channels are correlated by about ~a %. This means:~%~a~%" 
            pan-position
            (round (* corr 100))
            (cond
             ((between corr 0.97 1.1)
              " - The two channels are identical, i.e. dual mono.
   The center can't be removed.
      Any remaining  difference may be caused by lossy encoding.")
             ((between corr 0.9 0.97)  
              " - The two Channels are strongly related, i.e. nearly mono or extremely panned.
   Most likely, the center extraction will be poor.")
             ((between corr 0.5 0.9)
              " - A fairly good value, at least stereo in average and not too wide spread.")
             ((between corr 0.2 0.5)
              " - An ideal value for Stereo.
  However, the center extraction depends also on the used reverb.")
             ((between  corr -0.2 0.2)
              " - The two channels are almost not related.
   Either you have only noise or the piece is mastered in a unbalanced manner.
   The center extraction can still be good though.")
             ((between corr -0.8 -0.2)
              " - Although the Track is stereo, the field  is obviously extra wide.
   This can cause strange effects.
   Especially when played by only one speaker.")
             (t " - The two channels are nearly identical.
   Obviously, a pseudo stereo effect has been used
   to spread the signal over the physical distance between the speakers.
 Don't expect good results from a center removal."))))
;;;
;;; FFT Functionality
;;
;; different windows
(defun fft-window (fs type hop zeros)
  (cond
        ; Bartlett, but first value > 0
        ((= type 0) 
         (if (= zeros 0)
             (snd-pwl 0 fs
                      (list 0 (/ (float hop)) (1- hop) 1.0 (1- fs) 0.0 fs))
             (progn (setf cut (truncate (- fs zeros 1)))
                    (snd-pwl 0 fs
                             (list 0 (/ (float hop))(- cut hop) 1.0 cut 0.0 fs 0.0 fs)))))
        ; Han
        ((= type 1)
         (seq (cue (control-srate-abs fs
          (mult 0.5 (sum 1 (lfo (/ fs (* 2.0 hop)) (/ (- fs zeros)            (get-duration fs)) *table* 270)))))
          (cue (snd-const 0 0 fs (/ (float zeros) fs)))))
        ; rectangle 
        (t (if (= fs hop) (snd-pwl 0 fs (list 0 1.0 fs 1.0 fs))
         (snd-pwl 0 fs
                  (list 0 1.0 (1- hop) 1.0 hop 0.0 fs 0.0 fs))))))
;;
;; objects and classes
(setf fft-class (send class :new
                 '(sound length skip window function argument2 wt-max) ))
(send fft-class :answer :next '() '(
    (if argument2
        (funcall function (snd-fft sound length skip window)                  argument2)
        (funcall function  (snd-fft sound length skip window)))))
(send fft-class :answer :isnew '(snd len skp win fn arg2) '(
    (setf wt-max 0.0) 
    (setf sound snd)
    (setf length len)
    (setf skip skp)
    (setf window win)
    (setf function fn)
    (setf argument2 arg2)))
;;
;;; Short Time Fourier Transform
(defun stft (sound length skip window
        &optional (function #'(lambda (fr) fr)) (argument2 nil))
  (send fft-class :new sound length skip window function argument2))
;;
;; Power spectrum calculated from fft (as sound)
(defun power-spectrum (frame size sr)
  (let* ((snd (scale    (/ (sqrt 8.0) *win-sigma*) (snd-from-array 0 sr frame)))
         (zero (snd-from-array 0 sr #(0))))
     (s-log  (scale 2 (snd-avg (seq (cue zero) (cue (prod    snd snd))) 2 2 op-average)))))
;;
;; Make a weighted center (mono)
;; that can be substracted from L&R
(defun steer (side obj &aux (mid (send obj :next)))
  (cond ((and mid side)  
   (let* ((power-sum  (power-spectrum mid fs 2))
          (power-dif (power-spectrum side fs 2))
          (wt-exp (s-exp   (scale strength    (diff power-dif power-sum)))) 
          (weight (shape wt-exp *map* 0))
          (weight (snd-samples weight ny:all)))
     (do ((i low-transition (+ i 2))) ((>= i high-transition))
       (setf (: out i) (: weight (/ (1+ i) 2)))
       (setf (: out (1+ i)) (: weight (/ (1+ i) 2))))
     (snd-samples  (mult (snd-from-array 0 1 mid) (snd-from-array 0 1 out)) fs)))
    (t nil)))
;;;
;;; Sound Pre-processing
;;
;; rotate the stereo field around the center point
;; between the two speakers
(defun transform  (snd &optional ( cosine (cos (abs rotation))) (sine (sin (abs rotation)))) 
  (let* ((direction (/  (+ 1e-15 rotation) (abs  (+ 1e-15 rotation))))
         (fft-offset (s-rest (if (< action 6) (/ hop (get-duration *sr*)) 0)))
         (L (seq (cue fft-offset) (cue (: snd 0))))
         (R (seq (cue fft-offset) (cue (: snd 1)))))
         (vector (sum (mult cosine  L) (mult (- direction) sine R))
      (sum (mult direction  sine L) (mult cosine R)))))
;;;
;;; main procedure
(defun catalog  (&aux  snd
(original-len (/ (+ len hop) *sr*)) (dur (get-duration 1)))
  (if (soundp *track*) (return-from catalog  "This plug-in works only with stereo tracks.")
      (setf snd (vector (snd-copy (: *track* 0)) (snd-copy (: *track* 1)))))
  (cond
         ((= action 7)
          (return-from catalog (summary (least-squares-xy (: snd 0) (: snd 1) :show nil))))
         ((= action 6) 
          (return-from catalog (diff (: snd 0) (: snd 1))))
         (t; For everything that involves center isolation
          (setf snd  (transform  snd))
          (setf analyze-win (s-sqrt (fft-window fs type hop zs)))
          (setf synthesis-win analyze-win)
          (unless double-win (setf analyze-win (fft-window fs type hop zs))
                             (setf synthesis-win nil))
          (setf *win-sigma* (* fs (peak (integrate analyze-win) ny:all)))
          (setf sum-fft (stft (sum (: snd 0) (: snd 1)) fs hop analyze-win))
          (setf dif-fft (stft (diff (: snd 0) (: snd 1)) fs hop analyze-win 'steer sum-fft))
          (setf c (snd-ifft 0 *sr* dif-fft hop  synthesis-win ))
          (cond 
                ((member action '(0 3)) 
                 (setf output (vector (extract-abs  (/ hop *sr*) original-len (diff (: snd 0) c)) 
                                       (extract-abs  (/ hop *sr*) original-len (diff (: snd 1)  c)))))
                ((member action '(1 4)) 
                 (setf strength (recip strength))
                 (setf output (extract-abs (/ hop *sr*) original-len c)))
                ((member action '(2 5))
                 (setf strength (recip strength))
                 (setf output (extract-abs  (/ hop *sr*) original-len (mult -1 c)))))))
          (if (soundp output) (setf output (vector output output)))
              (mult *norm* output))
;;;; Main
*track*; Return original audio if something goes wrong
;;;  we start with some variable assignements
(setf *sr* *sound-srate*)
;; hard coded STFT parameters
;; Change for experimental purposes
(setf type 1); -1 = square 0 =triangle 1 = Han
(setf double-win t); t = windows before and after
(setf fs (* 16 512)); fft-frame-size
(setf hop (* 7 512)); Hop (step size to advance)
(setf zs (- fs (* 2 hop))); zero-padding
;; Some input corrections
(setf strength  (expt (limit strength 0.02 50.0) 2.0))
; bins to be ignored (bass and treble) 
(if (> action 2) (psetq low-transition 0.0 high-transition 24000.0))
(let* ((ltrans (logior (truncate (/ (* 2 fs (limit low-transition 1 (/ *sr* 2.0))) *sr*)) 1))
       (htrans (logior  (limit (truncate (/ (* 2 fs  high-transition) *sr*)) 1 (1- fs)) 1)))
      (psetq low-transition (min ltrans htrans)
             high-transition (max ltrans htrans)))
(setf out (snd-samples (snd-const 0.0 0 fs fs) fs)); holds the left/right weights (removal)  
(setf *map* (snd-pwl 0 10000 (list 0 0.5 10000 0.0 20000 -0.5 20001)))
(setf *norm* 1.0)
(expand 120); remove for lower efficiency/more conservative  memory management 
(catalog)