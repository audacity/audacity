$nyquist plug-in
$version 4
$type process
$preview linear
$name (_ "Delay")
$debugbutton false
$author (_ "Steve Daulton")
$release 2.4.2-2
$copyright (_ "GNU General Public License v2.0")


;; License: GPL v2 or later.
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;; based on 'Delay' by David R. Sky
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control DELAY-TYPE (_ "Delay type") choice ((_ "Regular")
                                             ("BouncingBall" (_ "Bouncing Ball"))
                                             ("ReverseBouncingBall" (_ "Reverse Bouncing Ball"))) 0
$control DGAIN (_ "Delay level per echo (dB)") real "" -6 -30 1
$control DELAY (_ "Delay time (seconds)") real "" 0.3 0 5
$control PITCH-TYPE (_ "Pitch change effect") choice (("PitchTempo" (_ "Pitch/Tempo"))
                                                      ("LQPitchShift" (_ "Low-quality Pitch Shift"))
                                                      ("HQPitchShift" (_ "High-quality Pitch Shift"))) 0
$control SHIFT (_ "Pitch change per echo (semitones)") real "" 0 -2 2
$control NUMBER (_ "Number of echoes") int "" 5 1 30
$control CONSTRAIN (_ "Allow duration to change") choice ((_ "Yes")(_ "No")) 0 


;; High-quality Pitch Shift option added, March 2023.
;;
;; "High-quality Pitch Shift" is accomplished with a phase vocoder.
;; "Pitch/Tempo" and "Low-quality Pitch Shift" remain identical
;; to previous version of Audacity.
;;
;; "Pitch/Tempo" is simple resampling, so both pitch and tempo
;; of the delayed audio will change (as in Audacity's
;; "Change Speed" effect).
;;
;; "Low-quality Pitch Shift" changes the pitch without changing
;; the tempo, but has relatively poor sound quality.


;;; Pitch shift audio.
(defun p-shift (sig snd-len ratio)
  (when (= SHIFT 0)
    ; no-op.
    (return-from p-shift sig))
  (case PITCH-TYPE (0 (change-speed sig ratio))
                   (1 (lq-pitch sig ratio))
                   (t (hq-pitch sig snd-len ratio))))


;;; Change speed.
(defun change-speed (sig ratio)
  (force-srate *sound-srate* 
    (stretch-abs (/ ratio) (sound sig))))


;;; Low quality pitch shift.
;; This uses the ancient "Synthesis Toolkit" pitch shifter.
;; STK_PITSHIFT: a simple pitch shifter using delay lines.
;; Filtering and fixed sample rate are used to squeeze slightly
;; better sound quality out of this old library.
(defun lq-pitch(sig ratio)
  ; pitshift quality best at 44100
  (let ((sig (force-srate 44100 sig))
          ; anti-alias filter frequency
          (minrate (* 0.5 (min *sound-srate* 44100))))
      (force-srate *sound-srate*
        ; pitshift requires rates to match
        (progv '(*sound-srate*) (list 44100)
          (cond 
            ((> SHIFT 5)  ; reduce aliasing
              (pitshift (lp-wall sig (/ minrate ratio)) ratio 1))
            ((< SHIFT -2)  ; reduce sub-sonic frequencies
              (pitshift (hp sig 20) ratio 1))
            (T (pitshift sig ratio 1)))))))


;;; Anti-alias low pass filter
(defun lp-wall (sig freq)
  (do ((count 0 (1+ count))
       (freq (* 0.94 freq)))
      ((= count 10) sig)
    (setf sig (lowpass8 sig freq))))


;;; High quality pitch shift.
(defun hq-pitch(sig snd-len shift-ratio)
  (let ((stretchfn (const 1))
        (pitchfn (const shift-ratio)))
    (pv-time-pitch sig stretchfn pitchfn snd-len)))


;;; Apply effects to echo
(defun modify (sig echo-num snd-len)
  (let ((gain (db-to-linear (* echo-num DGAIN)))
        ; convert semitone shift to ratio.
        (ratio (power 2.0 (/ (* echo-num SHIFT) 12.0))))
    (if (= PITCH-TYPE 0)
        (mult gain (change-speed sig ratio))
        (mult gain (p-shift sig snd-len ratio)))))


;;; Compute echoes.
(defun delays (sig snd-len)
  (if (>= DELAY-TYPE 1)  ; Bouncing delay.
      (setf time-shift (/ DELAY NUMBER))
      (setf time-shift DELAY))
  ;; The echo loop.
  (let ((echo (s-rest 0)))
    (do ((count 1 (1+ count))
         (dly 0))
         ((> count NUMBER)(sim echo sig))
      (let ((modified-sig (modify sig count snd-len)))
        (setq dly 
          (case DELAY-TYPE
            (0 (+ dly time-shift))
            (1 (+ dly (* time-shift (- (1+ NUMBER) count))))
            (2 (+ dly (* time-shift count)))))
        (setf echo (sim
            (at 0 (cue echo))
            (at-abs dly
                (cue modified-sig))))))))


(defun constrain-abs (sig dur)
  (extract-abs 0 dur (cue sig)))


(let* ((dur (get-duration 1))
       (output (multichan-expand #'delays *track* dur)))
  (if (= CONSTRAIN 1)
      (multichan-expand #'constrain-abs output dur)
      output))
