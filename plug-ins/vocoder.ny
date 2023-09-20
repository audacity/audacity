$nyquist plug-in
$version 4
$type process
$preview enabled
$name (_ "Vocoder")
$debugbutton false
$author (_ "Edgar-RFT and Steve Daulton")
$release 3.1.2-1
$copyright (_ "GNU General Public License v2.0")


;; If selected track is mono, the vocoder uses sine waves as the modulation
;; carrier, mixed with noise and radar needles according to slider settings.
;; If selected track is stereo, the right channel is used as the carrier wave.

;; License: GPL v2
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control DST (_ "Distance: (1 to 120, default = 20)") float "" 20 1 120
$control MST (_ "Output choice") choice (("BothChannels" (_ "Both Channels"))
                                         ("RightOnly" (_ "Right Only"))) 0
$control BANDS (_ "Number of vocoder bands") int "" 40 10 240
$control TRACK-VL (_ "Amplitude of carrier wave (percent)") float "" 100 0 100
$control NOISE-VL (_ "Amplitude of white noise (percent)") float "" 0 0 100
$control RADAR-VL (_ "Amplitude of Radar Needles (percent)") float "" 0 0 100
$control RADAR-F (_ "Frequency of Radar Needles (Hz)") float "" 30 1 100


;; Return log to base 2 of x.
(defun log2 (x)
  (/ (log (float x)) (log 2.0)))


;; Global constants.
;; Scale slider values for better control.
(setf TRACK-VOL (sqrt (/ TRACK-VL 100.0)))
(setf NOISE-VOL (expt (/ NOISE-VL 100.0) 2.0))
(setf RADAR-VOL (sqrt (/ RADAR-VL 100.0)))

;; number of octaves from 20 Hz.
;; Maximum number of octaves is: log2(high-hz / low-hz)
;; "2.205" is for compatibility with older versions of vocoder effect.
(setf OCTAVES (log2 (/ (/ *sound-srate* 2.205) 20)))

;; interval - number of semitones per vocoder band
(setf INTERVAL (/ (* OCTAVES 12.0) BANDS))


(defun make-radar-table (hz)
  (let ((one (/ *sound-srate*)) ;one sample period
        radar-table)
    (setf radar-table
        (stretch-abs 1 (sim (snd-const 1 one *sound-srate* one)
                            (s-rest (/ 1.0 hz)))))
    (list radar-table (hz-to-step hz) T)))


;;; The Mixer
(defun mix-noise (sig)
  (sum (cond ((= TRACK-VOL 0) 0)
             ((< TRACK-VOL 1) (mult TRACK-VOL sig))
             (t sig))
       (if (> RADAR-VL 0)
           (let ((r-table (make-radar-table RADAR-F)))
             (mult RADAR-VOL
                   (osc (hz-to-step RADAR-F) 1 r-table)))
           0)
       (if (> NOISE-VL 0)
           (mult NOISE-VOL (noise 1))
           0)))


;; Raise 'hz' by 'INTERVAL' semitones.
(defmacro next-hz (hz INTERVAL)
  `(let* ((prev-step (hz-to-step ,hz))
          (next-step (+ prev-step ,INTERVAL)))
    (step-to-hz next-step)))


(defmacro sumto (x y)
  `(setf ,x (sum ,x ,y)))


;;; Stereo Vocoder - returns mono sound.
(defun vocoder (sig is-mono-track)
  (let (mod-envelope
        band
        (result 0))
    (do ((i 0 (1+ i))
         (q (/ (sqrt 2.0) (/ OCTAVES BANDS)))  ; quick approximation of q
         (f (next-hz 20 (/ INTERVAL 2.0))
            (next-hz f INTERVAL)))
        ((= i BANDS) result)
      (when is-mono-track
        (sumto (aref sig 1) (mult 0.5 (/ TRACK-VOL BANDS) (hzosc f))))
      (setf band (bandpass2 sig f q)) ; intermediate results (2 channels)
      (setf mod-envelope (lowpass8 (s-abs (aref band 0)) (/ f DST)))
      (sumto result (bandpass2 (mult mod-envelope (aref band 1)) f q)))))


;;; The Program
(if (= (+ TRACK-VOL NOISE-VOL RADAR-VOL) 0)
    (format nil (_ "Error.~%No modulation carrier."))
    (progn
      (if (arrayp *track*)
          (setf sig (vector (aref *track* 0) (mix-noise (aref *track* 1))))
          (setf sig (vector *track* (mix-noise (s-rest 0)))))
      (setf sig (vocoder sig (soundp *track*)))
      ;; Normalize *track* to 0 db peak based on first 10 million samples.
      (setf sig (scale (/ (peak sig 10000000)) sig))
      (if (or MST (soundp *track*))
          sig
          (vector (aref *track* 0) sig))))
