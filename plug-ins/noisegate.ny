$nyquist plug-in
$version 4
$type process
$name (_ "Noise Gate")
$manpage "Noise_Gate"
$action (_ "Gating audio...")
$debugbutton false
$preview enabled
$author (_ "Steve Daulton")
$release 2.4.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .


$control mode (_ "Select Function") choice ((_ "Gate")
                                            ("Analyze" (_ "Analyse Noise Level")))
                                            0
$control stereo-link (_ "Stereo Linking") choice (("LinkStereo" (_ "Link Stereo Tracks"))
                                                  ("DoNotLink" (_ "Don't Link Stereo")))
                                                  0
$control low-cut (_ "Apply Low-Cut filter") choice ((_ "No")
                                                    ("10Hz" (_ "10Hz 6dB/octave"))
                                                    ("20Hz" (_ "20Hz 6dB/octave")))
                                                    0
;; Work around bug 2336.
$control gate-freq (_ "Gate frequencies above (kHz)") float "" 0 0 10
$control level-reduction (_ "Level reduction (dB)") float "" -12 -100 0
$control threshold (_ "Gate threshold (dB)") float "" -48 -96 -6
$control attack (_ "Attack/Decay (ms)") float "" 250 10 1000


; Global variables
(setq silence (if (> level-reduction -96) 0 1)) ; flag for silence
(setq freq (* 1000.0 gate-freq))
(setq level-reduction (db-to-linear level-reduction))
(setq threshold (db-to-linear threshold))
(setq attack (/ attack 1000.0))
(setq look attack)  ; lookahead
(setq decay attack) ; this could be replaced with a slider if required


(defun error-check ()
  (when (>= freq (* *sound-srate* 0.45))  ;10% below Nyquist should be safe maximum.
    ;; Work around bug 2012.
    (throw 'err (format nil (_ "Error.
\"Gate frequencies above: ~s kHz\"
is too high for selected track.
Set the control below ~a kHz.")
                        gate-freq
                        (roundn (* 0.00045 *sound-srate*) 1))))
  (when (< len 100) ;100 samples required 
    ;; Work around bug 2012.
    (throw 'err (format nil (_ "~%Insufficient audio selected.
Make the selection longer than ~a ms.")
                        (round-up (/ 100000 *sound-srate*))))))


;;; Analysis functions:

(defun analyze (sig)
  ; Return analysis text.
  (let* ((test-length (truncate (min len (/ *sound-srate* 2.0))))
         (levm (peak-avg-db sig test-length))
         (target (* 0.925 levm))) ;suggest 7.5% above noise level
    ;; Work around bug 2012.
    (format nil (_ "Peak based on first ~a seconds ~a dB~%
Suggested Threshold Setting ~a dB.")
            (roundn (/ test-length *sound-srate*) 2)
            (roundn levm 2)
            (roundn target 0))))

(defun peak-avg-db (sig test-len)
  ;; Return average of positive/ negative peaks (dB).
  ;; For stereo tracks, return the maximum of the channels.
  (if (arrayp sig)
      (let ((avgL (peak-avg (aref sig 0) test-len))
            (avgR (peak-avg (aref sig 1) test-len)))
        (linear-to-db (max avgL avgR)))
      (let ((avg (peak-avg sig test-len)))
        (linear-to-db avg))))

(defun peak-avg (sig test-len)
  ;; Return average of positive/ negative peaks.
  ;; We use the average for the analysis so as to ignore
  ;; DC offet (assuming noise is approximately symetrical).
  (let* ((pos (s-max sig 0))
         (neg (s-min sig 0))
         (lev (peak pos test-len))
         (inv-lev (peak neg test-len)))
    (/ (+ lev inv-lev) 2.0)))


;;; Utility functions

(defun round-up (num)
  (round (+ num 0.5)))

(defun roundn (num places)
  ;; Round number to specified decimal places.
  (if (= places 0)
      (round num)
      (let* ((x (format NIL "~a" places))
             (ff (strcat "%#1." x "f")))
        (setq *float-format* ff)
        (format NIL "~a" num))))


;;; Gate Functions

(defun noisegate (sig gatefollow lookahead risetime falltime floor threshold Hz)
  (let ((gain (/ (- 1 (* silence floor))))
        (env (get-env gatefollow lookahead risetime falltime floor threshold)))
    (if (> Hz 20)
        (sim (mult (highpass8 sig Hz) gain env)
             (lowpass8 sig (* 0.91 Hz))) ;magic number 0.91 improves crossover.
        (mult sig gain env))))

(defun get-env (follow look rise fall floor thresh)
  ;; Return gate's envelope
  (let* ((gate-env (gate follow look rise fall floor thresh))
         (gate-env (clip gate-env 1.0)))  ;gain must not exceed unity.
    (diff gate-env (* silence floor))))

(defun gate-follow (sig mode)
  ;; Return signal that gate will follow.
  (setf sig (hp sig 20)) ; hp filter to remove DC off-set
  (if (and (= mode 0)(arrayp sig)) ; stereo track set to mono/link
      (s-max (s-abs (aref sig 0)) (s-abs (aref sig 1)))
      (s-abs sig)))

(defun hpfilter (sig)
  ;; First order filters to minimise ringing.
  (case low-cut
    (1 (hp sig 10))
    (2 (hp sig 20))
    (T sig)))

(defun process ()
  ;; Main function
  (error-check)
  (let ((sig (hpfilter *track*)))
        (setf *track* nil)
        (setf gatefollow (gate-follow sig stereo-link))
        (multichan-expand #' noisegate sig
                                       gatefollow
                                       look
                                       attack
                                       decay
                                       level-reduction
                                       threshold
                                       freq)))

;; Run program
(case mode
  (0 (catch 'err (process)))
  (T (analyze *track*)))
