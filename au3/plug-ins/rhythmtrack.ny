$nyquist plug-in
$version 4
$type generate
$name (_ "Rhythm Track")
$debugbutton false
$preview linear
$author (_ "Dominic Mazzoni, David R. Sky and Steve Daulton")
$release 3.0.0-2
$copyright (_ "GNU General Public License v2.0")


;; Drip sound generator by Paul Beach

;; TODO: add more drum sounds

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control TEMPO (_ "Tempo (bpm)") real (_ "30 - 300 beats/minute") 120 30 300
$control TIMESIG (_ "Beats per bar") int (_ "1 - 20 beats/measure") 4 1 20
$control SWING (_ "Swing amount") float (_ "+/- 1") 0 -1 1
$control text (_ "Set 'Number of bars' to zero to enable the 'Rhythm track duration'.")
$control BARS (_ "Number of bars") int (_ "1 - 1000 bars") 16 0 1000
$control CLICK-TRACK-DUR (_ "Rhythm track duration") time (_ "Used if 'Number of bars' = 0") 0 0 nil  
$control OFFSET (_ "Start time offset") time (_ "Silence before first beat") 0 0 nil
$control CLICK-TYPE (_ "Beat sound") choice (("Metronome" (_ "Metronome Tick"))
                                             (_ "Ping (short)")
                                             (_ "Ping (long)")
                                             (_ "Cowbell")
                                             ("ResonantNoise" (_ "Resonant Noise"))
                                             ("NoiseClick" (_ "Noise Click"))
                                             (_ "Drip (short)")
                                             (_ "Drip (long)")) 0

$control HIGH (_ "MIDI pitch of strong beat") int (_ "18 - 116") 84 18 116
$control LOW (_ "MIDI pitch of weak beat") int (_ "18 - 116") 80 18 116


;; Helper functions:

(defun round-up (x)
  (if (> x (truncate x))
    (truncate (1+ x))
    (truncate x)))


;; Filtering causes changes amplitude, so we normalize to
;; achieve a predictable level.
(defun normalize (sig)
  (scale (/ (peak sig ny:all)) sig))


(defun s-rest-abs (d)
  (abs-env (s-rest d)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Click sound synthesis
;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Drip sound by Paul Beach www.proviewlandscape.com/liss/
(defun drip (p) ;p is pitch in hz
  (let* ((maxhz (/ *sound-srate* 2.1))
        (hz1 (min maxhz (* 2.40483  p)))
        (hz2 (min maxhz (* 5.52008  p)))
        (hz3 (min maxhz (* 8.653  p)))
        (hz4 (min maxhz (* 11.8  p))))
    (lp 
      (stretch-abs 1
        (mult (exp-dec 0 0.015 0.25) 
          (sim
            (mult (hzosc hz1) 0.5)
            (mult (hzosc hz2)  0.25)
            (mult (hzosc hz3)  0.125)
            (mult (hzosc hz4)  0.0625))))
    440)))


;; Metronome tick by Steve Daulton.
(defun metronome-tick (hz peak)
  (let* ((ln 300)
         (sig-array (make-array ln))
         (x 1))
    ;; generate some 'predictable' white noise
    (dotimes (i ln)
      (setf x (rem (* 479 x) 997))
      (setf (aref sig-array i) (- (/ x 500.0) 1)))
    (setf sig (sim (s-rest-abs 0.2)
                   (snd-from-array 0 44100 sig-array)))
    (setf sig
      (mult (abs-env (pwev 10 (/ ln 44100.0) 2 1 0))
            (highpass8  (lowpass2 sig (* 2 hz) 6)
                        hz)))
    (let ((gain (/ (peak sig 300))))
      ; The '1.11' factor makes up for gain reduction in 'resample'
      (mult (abs-env (pwlv 1.11 0.02 1.11 0.05 0 ))
        (jcrev (mult peak gain sig) 0.01 0.1)))))


;; Cowbell by Steve Daulton.
(defun cowbell (hz)
  (sim
    (mult (pwev 0.3 0.8 0.0005)
          (hzosc hz *tri-table*)
          (hzosc (* hz 3.46) *tri-table*))
    (mult (pwev 0.7 0.2 0.01)
          (hzosc (* hz 7.3) *tri-table*)
          (hzosc (* hz 1.52) *tri-table*))))


;; Single tick generators:

(defun get-metronome-tick (hz gain)
  (resample
    (sound-srate-abs 44100 (metronome-tick hz gain))
    *sound-srate*))


(defun get-ping (pitch ticklen)
  (stretch-abs ticklen
    (mult
      (control-srate-abs *sound-srate* (pwl 0.005 amp 0.995 amp 1))
      (osc pitch))))


(defun get-resonant-noise (pitch)
  (stretch-abs 0.05 ; 50 milliseconds
    (mult
      (control-srate-abs *sound-srate* (pwl 0.05 amp 0.95 amp 1))
      (normalize (lowpass2 (noise 1) (step-to-hz pitch) 20)))))


(defun get-noise-click (pitch)
  (stretch-abs 0.005
    (mult
      (control-srate-abs *sound-srate* (pwl 0.005 amp 0.995 amp 1))
      (normalize (lowpass2 (noise 1) (step-to-hz pitch) 2)))))


(defun get-drip (pitch ticklen)
  (stretch-abs ticklen
    (mult
      (control-srate-abs *sound-srate* (pwl 0.005 amp 0.995 amp 1))
      (normalize (drip (step-to-hz pitch))))))


(defun get-cowbell (pitch)
  (mult 0.8 (cowbell (step-to-hz pitch))))


;; Make selected click
(defun click (accent)
  (setq pitch (if (= accent 1) HIGH LOW))
  (setq amp (if (= accent 1) 0.75 0.5))
  (case CLICK-TYPE
    (0 (get-metronome-tick (step-to-hz pitch) amp))
    (1 (get-ping pitch 0.01))
    (2 (get-ping pitch 0.08))
    (3 (get-cowbell pitch))
    (4 (get-resonant-noise pitch))
    (5 (get-noise-click pitch))
    (6 (get-drip pitch 0.007))
    (t (get-drip pitch 0.1))))


(defun swing-adjust (i val)
  (* val (/ 3.0) (rem (1+ i) 2)))


;Make one measure and save it in the global *measure*
(defun makemeasure ()
  (setf *measure*
    (sim
      (s-rest (* TIMESIG beatlen)) ;required for trailing silence
      (click 1) ;accented beat
      (simrep (count (- TIMESIG 1))
        (at-abs (* beatlen (+ count 1 (swing-adjust count SWING)))
            (cue (click 0))))))) ;unaccented beat


(defun samplecount (total)
  ;;; Return number of samples required to reach target
  (defun lastsample (target)
    (let ((required (- target total)))
      (setf total target)
      required))
  (function lastsample))


(defun get-measure (barnum)
  (let ((end (* (1+ barnum) (* TIMESIG beatlen)))
        required-samples)
    ;; Actual end time is integer samples
    (setf end (round (* end *sound-srate*)))
    (setf required-samples (funcall addsamples end))
    (setf *measure* (set-logical-stop (cue *measure*)
                                      (/ required-samples *sound-srate*))))
  *measure*)


(defun make-click-track (barcount mdur)
  (seqrep (i barcount) (cue (get-measure i))))


;;;;;;;;;;;;;;;;;
;; MAIN PROGRAM 
;;;;;;;;;;;;;;;;;
      

(setf beatlen (/ 60.0 TEMPO))

;call function to make one measure
(makemeasure)

; If 'Number of bars' = 0, calculate bars from 'Rhythm track duration'.
(if (= BARS 0)
    (setq barcount (/ CLICK-TRACK-DUR (* TIMESIG beatlen)))
    (setf barcount BARS))

;if previewing, restrict number of bars
(let ((preview (/ (get '*project* 'preview-duration)
                  (* TIMESIG beatlen))))
  (if *previewp*
      (setf barcount (min preview barcount))))

;round up number of bars
(setf barcount (round-up barcount))

;; Calculate LEN for progress bar.
(setf len (/ (* 60.0 *sound-srate* TIMESIG barcount) TEMPO))

;; Initialize sample count
(setf addsamples (samplecount 0))

(if (< barcount 1)
    (format nil (_ "Set either 'Number of bars' or~%~
                    'Rhythm track duration' to greater than zero."))
    (if *previewp*
        ;; Don't preview the offset (silence).
        (make-click-track barcount (* TIMESIG beatlen))
        (seq (s-rest OFFSET)
             (make-click-track barcount (* TIMESIG beatlen)))))
