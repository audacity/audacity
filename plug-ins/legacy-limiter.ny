$nyquist plug-in
$version 4
$type process
$name (_ "Legacy Limiter")
$debugbutton false
$preview enabled
$author (_ "Steve Daulton")
$release 3.0.4-2
$copyright (_ "GNU General Public License v2.0 or later")

;; limiter.ny by Steve Daulton November 2011, updated May 2015.

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control TYPE (_ "Type") choice (("SoftLimit" (_ "Soft Limit"))
                                 ("HardLimit" (_ "Hard Limit"))
;i18n-hint: clipping of wave peaks and troughs, not division of a track into clips
                                 ("SoftClip" (_ "Soft Clip"))
                                 ("HardClip" (_ "Hard Clip"))) 0

;; Translations don't support "\n", and widgets need a literal string,
;; so the next two controls must be written on two lines.
$control GAIN-L (_ "Input Gain (dB)
mono/Left") real "" 0 0 10

$control GAIN-R (_ "Input Gain (dB)
Right channel") real "" 0 0 10

$control THRESH (_ "Limit to (dB)") real "" -3 -10 0
$control HOLD (_ "Hold (ms)") real "" 10 1 50
$control MAKEUP (_ "Apply Make-up Gain") choice ((_ "No") (_ "Yes")) 0


(setf gain-left (db-to-linear GAIN-L))
(setf gain-right (db-to-linear GAIN-R))
(setf thresh-lin (db-to-linear THRESH))
(setf bmakeup (= MAKEUP 1))


;;; brick wall limiter
(defun hardlimit (sig limit)
  (let* ((time (/ HOLD 3000.0))  ; lookahead time (seconds)
         (samples (round (* time *sound-srate*)))  ; lookahead in samples
         (peak-env (get-env sig samples time limit)))
    (mult sig
          (snd-exp (mult -1 (snd-log peak-env))))))


;;; Envelope follower for brick wall limiter
(defun get-env (sig step lookahead limit)
  (let* ((sig (mult (/ limit) sig))
         (pad-time (* 3 lookahead))       ; padding required at start (seconds)
         (pad-s (* 3 step))               ; padding samples
         (padding (snd-const (peak sig pad-s) 0 *sound-srate* pad-time))
         (peak-env (snd-avg sig (* 4 step) step OP-PEAK)))
    (extract 0 1
        (s-max 1
               (sim padding
                    (at-abs pad-time (cue peak-env)))))))


(defun softlimit (sig threshold)
  (let* ((sig (hardlimit sig 1))
         (step (truncate (* (/ HOLD 3000.0) *sound-srate*)))
         (waveshape (snd-avg sig (* 4 step) step op-peak))
         (env (sum threshold (mult threshold (diff 1 waveshape))))
         (env (clip env 1))
         (offset (/ (* 3 step) *sound-srate*))
         (initial (peak sig (* 2 step)))
         (pause-lev (sum threshold (mult threshold (diff 1 initial))))
         (pause-lev (clip pause-lev 0.9))
         (pause (snd-const pause-lev 0 *sound-srate* offset)))
    (setf env (sim pause
                   (at-abs offset (cue env))))
    (mult sig env)))


(defun soft-clip-table ()
  ;;; Lookup table for soft clipping wave-shaper.
  (abs-env
    (sound-srate-abs 44100
      (control-srate-abs 44100
        (let* ((knee (- 1 (/ 1.0 pi)))
               (kcurve (mult knee (osc (hz-to-step (/ (* 4 knee))) knee)))
               (ikcurve (mult knee (osc (hz-to-step (/ (* 4 knee))) knee *sine-table* -90)))
               (lin (pwlv -0.5 knee -0.5
                               (+ knee (/ 2.0 pi)) 0.5
                               2.0 0.5
                               2.0 (+ 0.5 knee)
                               2.1 (+ 0.5 knee))))
          (mult (/ 2.0 pi)
                (sim (at-abs 0 (cue ikcurve))
                     (at-abs 0 (cue lin))
                     (at-abs (+ knee (/ 2.0 pi)) (cue kcurve)))))))))


(defun soft-clip (sig threshold)
  (let* ((knee (- 1 (/ 1.0 pi)))
         (clip-level (* (+ 0.5 knee)(/ 2.0 pi)))
         (sig (mult clip-level (/ threshold) sig)))
    (if bmakeup
        ; Allow a little overhead to avoid hitting 0dB.
        (mult (/ 0.999 clip-level)
              (shape sig (soft-clip-table) 1.0))
        (mult (/ threshold clip-level)
              (shape sig (soft-clip-table) 1.0)))))


(defun makeupgain (sig threshold)
  (if bmakeup
      (mult (/ 0.999 threshold) sig) ;keep below 0dB
      sig))


;; Pre-gain
(setf *track*
  (if (arrayp *track*)
      (vector (mult (aref *track* 0) gain-left)
              (mult (aref *track* 1) gain-right))
      (mult *track* gain-left)))


(case TYPE
  (0 (makeupgain (multichan-expand #'softlimit *track* thresh-lin)
                 thresh-lin))
  (1 (makeupgain (multichan-expand #'hardlimit *track* thresh-lin)
                 thresh-lin))
  (2 (soft-clip *track* thresh-lin))
  (T (makeupgain (clip *track* thresh-lin)
                 thresh-lin)))
