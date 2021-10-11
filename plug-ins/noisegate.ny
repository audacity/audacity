$nyquist plug-in
$version 4
$type process
$name (_ "Noise Gate")
$manpage "Noise_Gate"
$debugbutton false
$preview enabled
$author (_ "Steve Daulton")
$release 3.0.4
$copyright (_ "GNU General Public License v2.0 or later")

;; Released under terms of the GNU General Public License v2.0 or later:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .

$control mode (_ "Select Function") choice (("Gate" (_ "Gate"))
                                            ("Analyze" (_ "Analyze Noise Level")))
                                           0
$control stereo-link (_ "Stereo Linking") choice (("LinkStereo" (_ "Link Stereo Tracks"))
                                                  ("DoNotLink" (_ "Don't Link Stereo")))
                                                 0
;; Work around bug 2336 - Text after control is not read by screen reader.
$control threshold (_ "Gate threshold (dB)") float "" -40 -96 -6
$control gate-freq (_ "Gate frequencies above (kHz)") float "" 0 0 10
$control level-reduction (_ "Level reduction (dB)") float "" -24 -100 0
$control attack (_ "Attack (ms)") float "" 10 1 1000
$control hold (_ "Hold (ms)") float "" 50 0 2000
$control decay (_ "Decay (ms)") float "" 100 10 4000


;; The gain envelope for the noisegate function may be a mono sound (stereo-link = 1, or *track* is mono)
;; or an array of sounds (stereo-link = 0 and *track* is stereo).
;; 'Level Reduction' is similar to "Range" or "Floor", but is a (negative) amount of gain
;; rather than a fixed level.
;;
;; To create the gain envelope:
;; 1. If stereo track and stereo-link = 1, get the max of left and right.
;; 2. Add 'hold' when level > threshold.
;;    This adds a high level signal for 'hold' seconds when the level
;;    falls below the threshold.
;; 3. Nyquist GATE function to generate exponential rise and decay.
;;    Unlike analog noise gates, lookahead is used so that the gate
;;    begins to open before the signal rises above the threshold.
;;    When the threshold is reached, the gate is fully open.
;;    This prevents the gate from clipping the beginning of words / sounds.
;; 4. Scale level of envelope and offset so that we have unity gain above
;;    threshold, and 'level-reduction' below the threshold.
;;    If silence-flag is set (= 1), gain below the threshold is zero.


; Global variables
(setf silence-flag (if (> level-reduction -96) 0 1))
(setf gate-freq (* 1000.0 gate-freq))
(setf floor (db-to-linear level-reduction))
(setf threshold (db-to-linear threshold))
(setf attack (/ attack 1000.0))
(setf lookahead attack)
(setf decay (/ decay 1000.0))
(setf hold (/ hold 1000.0))


(defun error-check ()
  (let ((max-hz (* *sound-srate* 0.45))  ;10% below Nyquist should be safe maximum.
        (max-khz (roundn (* 0.00045 *sound-srate*) 1))
        (gate-freq-khz (roundn (/ gate-freq 1000.0) 1)))
    (when (>= gate-freq max-hz)
      ;; Work around bug 2012.
      (throw 'err (format nil (_ "Error.
\"Gate frequencies above: ~s kHz\"
is too high for selected track.
Set the control below ~a kHz.")
                        gate-freq-khz
                        max-khz))))
  (let ((start (get '*selection* 'start))
        (end (get '*selection* 'end)))
    (when (> (* *sound-srate* (- end start)) (1- (power 2 31)))
      ;; Work around bug 2012 and 439.
      (throw 'err (format nil (_ "Error.
Selection too long.
Maximum length is ~a.") (format-time (/ (1- (power 2 31)) *sound-srate*))))))
  (when (< len 100) ;100 samples required 
    ;; Work around bug 2012.
    (throw 'err (format nil (_ "Error.
Insufficient audio selected.
Make the selection longer than ~a ms.")
                        (round-up (/ 100000 *sound-srate*))))))


;;; Analysis functions:
;; Measure the peak level (dB) and suggest setting threshold a little higher.

(defun analyze (sig)
  ; Return analysis text.
  (let* ((test-length (truncate (min len (/ *sound-srate* 2.0))))
         (peakdb (peak-db sig test-length))
         (target (+ 1.0 peakdb))) ;suggest 1 dB above noise level
    ;; Work around bug 2012.
    (format nil (_ "Peak based on first ~a seconds ~a dB~%
Suggested Threshold Setting ~a dB.")
            (roundn (/ test-length *sound-srate*) 2)
            (roundn peakdb 2)
            (roundn target 0))))

(defun peak-db (sig test-len)
  ;; Return absolute peak (dB).
  ;; For stereo tracks, return the maximum of the channels.
  (if (arrayp sig)
      (let ((peakL (peak (aref sig 0) test-len))
            (peakR (peak (aref sig 1) test-len)))
        (linear-to-db (max peakL peakR)))
      (linear-to-db test-len)))


;;; Utility functions

(defun round-up (num)
  (round (+ num 0.5)))

(defun roundn (num places)
  ;; Return number rounded to specified decimal places.
  (if (= places 0)
      (round num)
      (let* ((x (format NIL "~a" places))
             (ff (strcat "%#1." x "f")))
        (setq *float-format* ff)
        (format NIL "~a" num))))

(defun format-time (s)
  ;;; format time in seconds as h m.
  (let* ((hh (truncate (/ s 3600)))
         (mm (truncate (/ s 60))))
  ;i18n-hint: hours and minutes. Do not translate "~a".
  (format nil (_ "~ah ~am") hh (- mm (* hh 60)))))


;;; Gate Functions


(defun noisegate (sig follow)
  ;; Takes a sound and a 'follow' sound as arguments.
  ;; Returns the gated audio.
  (let ((gain (/ (- 1 (* silence-flag floor)))) ; silence-flag is 0 or 1.
        (env (get-env follow)))
    (if (> gate-freq 20)
        (let* ((high (highpass8 sig gate-freq))
               (low  (lowpass8 sig (* 0.91 gate-freq)))) ;magic number 0.91 improves crossover.
          (sim (mult high gain env) low))
        (mult sig gain env))))

(defun get-env (follow)
  ;; Return gate's envelope
  (let* ((gate-env (gate follow lookahead attack decay floor threshold))
         (gate-env (clip gate-env 1.0)))  ;gain must not exceed unity.
    (diff gate-env (* silence-flag floor))))

(defun peak-follower (sig)
  ;; Return signal that gate will follow.
  (setf sig (multichan-expand #'snd-abs sig))
  (when (and (arrayp sig)(= stereo-link 0))
    (setf sig (s-max (aref sig 0) (aref sig 1))))
  (if (> hold 0)
      (multichan-expand #'snd-oneshot sig threshold hold)
      sig))

(defun process ()
  (error-check)
  ;; For stereo tracks, 'peak-follower' may return a sound
  ;; or array of sounds, so pass it to 'noisegate' rather than
  ;; calculating in 'noisegate'.
  (multichan-expand #' noisegate *track* (peak-follower *track*)))


;; Run program
(case mode
  (0 (catch 'err (process)))
  (T (analyze *track*)))
