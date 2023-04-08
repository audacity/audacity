$nyquist plug-in
$version 4
$type process
$preview linear
$name (_ "Vocal Reduction and Isolation")
$debugbutton false
$author (_ "Robert Haenggi")
$release 3.0.1-2
$copyright (_ "GNU General Public License v2.0")


;; vocrediso.ny, based on rjh-stereo-tool.ny

;; License: GPL v2
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control ACTION (_ "Action") choice (("RemoveToMono" (_ "Remove Vocals: to mono"))
                                     ("Remove" (_ "Remove Vocals"))
                                     ("Isolate" (_ "Isolate Vocals"))
                                     ("IsolateInvert" (_ "Isolate Vocals and Invert"))
                                     ("RemoveCenterToMono" (_ "Remove Center: to mono"))
                                     ("RemoveCenter" (_ "Remove Center"))
                                     ("IsolateCenter" (_ "Isolate Center"))
                                     ("IsolateCenterInvert" (_ "Isolate Center and Invert"))
                                     (_ "Analyze")) 0

$control STRENGTH (_ "Strength") real "" 1.0 0.0 50.0
$control LOW-TRANSITION (_ "Low Cut for Vocals (Hz)") real "" 120 1 24000
$control HIGH-TRANSITION (_ "High Cut for Vocals (Hz)") real "" 9000 1 24000


; Global constant.
(setf BIGNUM 1000000000)

;;control ROTATION "Rotation (Degrees)" real "" 0 -180 180
(setf ROTATION 0.0)

;; make aref shorter
(defmacro  : (array index) (backquote (aref ,array ,index)))
;;
;; input corrections
(defmacro limit (symbol lower upper)
  (backquote (min ,upper (max ,lower ,symbol))))


;;; Some statistical functions
;;
;; Running Sum
(defun sigma (sig)
  (snd-biquad sig 1 0 0 1 0 0 0))


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
    (if show
        (format t
                (_ "Average x: ~a, y: ~a
                    Covariance x y: ~a
                    Average variance x: ~a, y: ~a
                    Standard deviation x: ~a, y: ~a
                    Coefficient of correlation: ~a
                    Coefficient of determination: ~a
                    Variation of residuals: ~a
                    y equals ~a plus ~a times x~%")
                bar-x   bar-y
                s-xy
                s-x2  s-y2
                (sqrt s-x2) (sqrt s-y2)
                r
                r2
                (* s-y2 (- 1 r2))
                a0  a1))
    (list r r2 pos-xy  a0 a1)))


(defun between (val low high)
  (and (> val low) (<= val high)))


;; Summary for "Analyse", fed with coeff. of correlation
(defun summary (analysis &aux (corr (car analysis)) (pan-position (third analysis)))
  (format nil (_ "Pan position: ~a~%The left and right channels are correlated by about ~a %. This means:~%~a~%")
          pan-position
          (round (* corr 100))
          (cond
           ((between corr 0.97 1.1)
            (_ " - The two channels are identical, i.e. dual mono.
                The center can't be removed.
                Any remaining difference may be caused by lossy encoding."))
           ((between corr 0.9 0.97)
            (_ " - The two Channels are strongly related, i.e. nearly mono or extremely panned.
                Most likely, the center extraction will be poor."))
           ((between corr 0.5 0.9)
            (_ " - A fairly good value, at least stereo in average and not too wide spread."))
           ((between corr 0.2 0.5)
            (_ " - An ideal value for Stereo.
                However, the center extraction depends also on the used reverb."))
           ((between  corr -0.2 0.2)
            (_ " - The two channels are almost not related.
                Either you have only noise or the piece is mastered in a unbalanced manner.
                The center extraction can still be good though."))
           ((between corr -0.8 -0.2)
            (_ " - Although the Track is stereo, the field is obviously extra wide.
                This can cause strange effects.
                Especially when played by only one speaker."))
           (t (_ " - The two channels are nearly identical.
                  Obviously, a pseudo stereo effect has been used
                  to spread the signal over the physical distance between the speakers.
                  Don't expect good results from a center removal.")))))


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
    ; Hann
    ((= type 1)
        (seq (cue (control-srate-abs fs
                        (mult 0.5
                              (sum 1
                                   (lfo (/ fs (* 2.0 hop)) (/ (- fs zeros) (get-duration fs)) *table* 270)))))
              (cue (snd-const 0 0 fs (/ (float zeros) fs)))))
    ; rectangle
    (t  (if (= fs hop)
            (snd-pwl 0 fs (list 0 1.0 fs 1.0 fs))
        (snd-pwl 0 fs (list 0 1.0 (1- hop) 1.0 hop 0.0 fs 0.0 fs))))))


;; objects and classes
(setf fft-class (send class :new
                 '(sound length skip window function argument2 wt-max)))

(send fft-class :answer :next '() '(
    (if argument2
        (funcall function (snd-fft sound length skip window) argument2)
        (funcall function  (snd-fft sound length skip window)))))

(send fft-class :answer :isnew '(snd len skp win fn arg2) '(
    (setf wt-max 0.0)
    (setf sound snd)
    (setf length len)
    (setf skip skp)
    (setf window win)
    (setf function fn)
    (setf argument2 arg2)))


;;; Short Time Fourier Transform
(defun stft (sound length skip window
             &optional (function #'(lambda (fr) fr)) (argument2 nil))
  (send fft-class :new sound length skip window function argument2))


;; Power spectrum calculated from fft (as sound)
(defun power-spectrum (frame size sr)
  (let* ((snd (scale    (/ (sqrt 8.0) *win-sigma*) (snd-from-array 0 sr frame)))
         (zero (snd-from-array 0 sr #(0))))
     (s-log  (scale 2 (snd-avg (seq (cue zero) (cue (prod    snd snd))) 2 2 op-average)))))


;; Make a weighted center (mono)
;; that can be subtracted from L&R
(defun steer (side obj &aux (mid (send obj :next)))
  (cond
    ((and mid side)
        (let* ((power-sum  (power-spectrum mid fs 2))
               (power-dif (power-spectrum side fs 2))
               (wt-exp (s-exp   (scale STRENGTH (diff power-dif power-sum))))
               (weight (shape wt-exp *map* 0))
               ;(weight (shape (db-to-linear power-dif) (s-exp (mult 2 (s-log *map2*))) 1))
               (weight (snd-samples weight BIGNUM)))  ;Fix for bug 2706
          (do ((i LOW-TRANSITION (+ i 2)))
              ((>= i HIGH-TRANSITION))
            (setf (: out i) (: weight (/ (1+ i) 2)))
            (setf (: out (1+ i)) (: weight (/ (1+ i) 2))))
          (snd-samples  (mult (snd-from-array 0 1 mid) (snd-from-array 0 1 out)) fs)))
    (t nil)))


;;; Sound Pre-processing
;;
;; rotate the stereo field around the center point
;; between the two speakers
(defun transform  (snd &optional (cosine (cos (abs ROTATION))) (sine (sin (abs ROTATION))))
  (let* ((direction (/  (+ 1e-15 ROTATION) (abs  (+ 1e-15 ROTATION))))
         (fft-offset (s-rest (if (< ACTION 8) (/ hop (get-duration *sr*)) 0)))
         (L (seq (cue fft-offset) (cue (: snd 0))))
         (R (seq (cue fft-offset) (cue (: snd 1)))))
    (vector (sum (mult cosine  L) (mult (- direction) sine R))
            (sum (mult direction  sine L) (mult cosine R)))))


;;; main procedure
(defun catalog  (&aux  snd (original-len (/ (+ len hop) *sr*)) (dur (get-duration 1)))
  (if (soundp *track*)
      (return-from catalog  (_ "This plug-in works only with stereo tracks."))
      (setf snd (vector (snd-copy (: *track* 0)) (snd-copy (: *track* 1)))))
  (cond
    ((= ACTION 8)
        (return-from catalog (summary (least-squares-xy (: snd 0) (: snd 1) :show nil))))
    ((= ACTION 0)
        (display "" LOW-TRANSITION HIGH-TRANSITION) ;values are quantized to bins
        (return-from catalog
                     (sum (: snd 0)
                          (mult -1 (: snd 1))
                          (lowpass8 (: snd 1) LOW-TRANSITION)
                          (highpass8 (diff (: snd 1) (lowpass8 (: snd 1) LOW-TRANSITION))
                                     HIGH-TRANSITION))))
    ((= ACTION 4)
        (return-from catalog (diff (: snd 0) (: snd 1))))
    (t  ;For everything that involves center isolation
        (setf snd  (transform  snd))
        (setf analyze-win (s-sqrt (fft-window fs type hop zs)))
        (setf synthesis-win analyze-win)
        (unless double-win
            (setf analyze-win (fft-window fs type hop zs))
            (setf synthesis-win nil))
        (setf *win-sigma* (* fs (peak (integrate analyze-win) ny:all)))
        (setf sum-fft (stft (sum (: snd 0) (: snd 1)) fs hop analyze-win))
        (setf dif-fft (stft (diff (: snd 0) (: snd 1)) fs hop analyze-win 'steer sum-fft))
        (setf c (snd-ifft 0 *sr* dif-fft hop  synthesis-win))
        (cond
          ((member ACTION '(1 5))
              (setf output (vector (extract-abs  (/ hop *sr*) original-len (diff (: snd 0) c))
                                   (extract-abs  (/ hop *sr*) original-len (diff (: snd 1)  c)))))
          ((member ACTION '(2 6))
              ; Caution: modifying global variable 'STRENGTH'.
              (setf STRENGTH (recip STRENGTH))
              (setf output (extract-abs (/ hop *sr*) original-len c)))
          ((member ACTION '(3 7))
              ; Caution: modifying global variable 'STRENGTH'.
              (setf STRENGTH (recip STRENGTH))
              (setf output (extract-abs  (/ hop *sr*) original-len (mult -1 c)))))))
    (if (soundp output)
        (setf output (vector output output)))
    (mult *norm* output))


;;;; Main

*track* ;Return original audio if something goes wrong

;;;  we start with some variable assignments
(setf *sr* *sound-srate*)

;; hard coded STFT parameters
;; Change for experimental purposes
(setf type 1); -1 = square 0 =triangle 1 = Han
(setf double-win t); t = windows before and after
(setf fs (* 16 512)); fft-frame-size
(setf hop (* 7 512)); Hop (step size to advance)
(setf zs (- fs (* 2 hop))); zero-padding

;; Some input corrections
; Caution: modifying global variable 'STRENGTH'.
(setf STRENGTH  (expt (limit STRENGTH 0.02 50.0) 2.0))

; bins to be ignored (bass and treble)
(if (> ACTION 3)
    ; Caution: modifying global variables 'LOW-TRANSITION' and 'HIGH-TRANSITION'.
    (psetq LOW-TRANSITION 0.0
           HIGH-TRANSITION 24000.0))

(let* ((ltrans (logior (truncate (/ (* 2 (1- fs) (limit LOW-TRANSITION 1 (/ *sr* 2.0))) *sr*)) 1))
       (htrans (logior  (limit (truncate (/ (* 2 fs  HIGH-TRANSITION) *sr*)) 1 (1- fs)) 1)))
  ; Caution: modifying global variables 'LOW-TRANSITION' and 'HIGH-TRANSITION'.
  (psetq LOW-TRANSITION (min ltrans htrans)
         HIGH-TRANSITION (max ltrans htrans)))

; back to real frequencies for the classic Vocal Remover
; Note: Fqs are quantized as if FFT would be used
; ca. 2.6 Hz bin-distance @ 44.1 kHz
(when (= ACTION 0)
  (setq bin-distance (/ *sr* 2.0 fs))
  ; Caution: modifying global variables 'LOW-TRANSITION' and 'HIGH-TRANSITION'.
  (psetq LOW-TRANSITION (* LOW-TRANSITION bin-distance)
         HIGH-TRANSITION (* HIGH-TRANSITION bin-distance)))

(setf out (snd-samples (snd-const 0.0 0 fs fs) fs)); holds the left/right weights (removal)
(setf *map* (snd-pwl 0 10000 (list 0 0.5 10000 0.0 20000 -0.5 20001)))
(setf *norm* 1.0)
(expand 120); remove for lower efficiency/more conservative  memory management
(catalog)
