$nyquist plug-in
$version 4
$type process
$name (_ "Limiter")
$manpage "Limiter"
$debugbutton false
$action (_ "Limiting...")
$preview enabled
$author (_ "Steve Daulton")
$release 2.3.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; limiter.ny by Steve Daulton November 2011, updated May 2015.

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

$control type (_ "Type") choice (
   ("SoftLimit" (_ "Soft Limit"))
   ("HardLimit" (_ "Hard Limit"))
   ;i18n-hint: clipping of wave peaks and troughs, not division of a track into clips
   ("SoftClip" (_ "Soft Clip"))
   ("HardClip" (_ "Hard Clip"))
) 0
$control gain-L (_ "Input Gain (dB)
mono/Left") real "" 0 0 10
$control gain-R (_ "Input Gain (dB)
Right channel") real "" 0 0 10
$control thresh (_ "Limit to (dB)") real "" -3 -10 0
$control hold (_ "Hold (ms)") real "" 10 1 50
$control makeup (_ "Apply Make-up Gain") choice (
   (_ "No")
   (_ "Yes")
) 0

(if (not (boundp 'type))
    (setf type 0))

(if (boundp 'gain-L)
    (setf gain-L (db-to-linear gain-L))
    (setf gain-L 1))

(if (boundp 'gain-R)
    (setf gain-R (db-to-linear gain-R))
    (setf gain-R 1))

(if (boundp 'thresh)
    (setf thresh (db-to-linear thresh))
    (setf thresh (db-to-linear -3.0)))

(if (not (boundp 'hold))
    (setf hold 10.0))

(if (boundp 'makeup)
    (if (= makeup 1) (setf makeup T) (setf makeup nil))
    (setf makeup nil))

;;; brick wall limiter
(defun hardlimit (sig limit hld)   
  (let* ((time (/ hld 3000.0))  ; lookahead time (seconds)
         (samples (round (* time *sound-srate*)))  ; lookahead in samples
         (peak-env (get-env sig samples time limit)))
    (mult sig
      (snd-exp 
        (mult -1 (snd-log peak-env))))))

;;; Envelope follower for brick wall limiter
(defun get-env (sig step lookahead limit)
  (let* ((sig (mult (/ limit) sig))
         (pad-time (* 3 lookahead))       ; padding required at start (seconds)
         (pad-s (* 3 step))               ; padding smaples
         (padding (snd-const (peak sig pad-s) 0 *sound-srate* pad-time))
         (peak-env (snd-avg sig (* 4 step) step OP-PEAK)))
    (extract 0 1
      (s-max 1 
        (sim padding
          (at-abs pad-time (cue peak-env)))))))

(defun softlimit (sig thresh hld)
  (let* ((sig (hardlimit sig 1 hold))
         (step (truncate (* (/ hld 3000.0) *sound-srate*)))
         (waveshape (snd-avg sig (* 4 step) step op-peak))
         (env (sum thresh (mult thresh (diff 1 waveshape))))
         (env (clip env 1))
         (offset (/ (* 3 step) *sound-srate*))
         (initial (peak sig (* 2 step)))
         (pause-lev (sum thresh (mult thresh (diff 1 initial))))
         (pause-lev (clip pause-lev 0.9))
         (pause (snd-const pause-lev 0 *sound-srate* offset)))
    (setf env 
      (sim
         pause
         (at-abs offset (cue env))))
    (mult sig env)))


(defun soft-clip-table ()
"Lookup table for soft clipping wave-shaper"
  (abs-env
    (sound-srate-abs 44100
      (Control-srate-abs 44100
        (let* ((knee (- 1 (/ 1.0 pi)))
               (kcurve (mult knee (osc (hz-to-step (/ (* 4 knee))) knee)))
               (ikcurve (mult knee (osc (hz-to-step (/ (* 4 knee))) knee *sine-table* -90)))
               (lin (pwlv -0.5 knee -0.5
                               (+ knee (/ 2.0 pi)) 0.5 
                               2.0 0.5
                               2.0 (+ 0.5 knee)
                               2.1 (+ 0.5 knee))))
          (mult (/ 2.0 pi)
            (sim
              (at-abs 0 (cue ikcurve))
              (at-abs 0 (cue lin))
              (at-abs (+ knee (/ 2.0 pi)) (cue kcurve)))))))))

(defun soft-clip (sig)
  (let* ((knee (- 1 (/ 1.0 pi)))
         (clip-level (* (+ 0.5 knee)(/ 2.0 pi)))
         (sig (mult clip-level (/ thresh) sig)))
    (if makeup
        (mult 0.999
              (/ clip-level)
              (shape sig (soft-clip-table) 1.0))
        (mult thresh
              (/ clip-level)
              (shape sig (soft-clip-table) 1.0)))))


(defun makupgain (sig)
  (if makeup
      (mult (/ 0.999 thresh) sig) ;keep below 0dB
      sig))

;; Pre-gain
(setf *track*
  (if (arrayp *track*)
      (vector (mult (aref *track* 0) gain-L)
              (mult (aref *track* 1) gain-R))
      (mult *track* gain-L)))


(case type
  (0 (makupgain (multichan-expand #'softlimit *track* thresh hold)))
  (1 (makupgain (multichan-expand #'hardlimit *track* thresh hold)))
  (2 (soft-clip *track*))
  (T (makupgain (clip *track* thresh))))
