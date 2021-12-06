$nyquist plug-in
$version 4
$type process
$preview enabled
$name (_ "Vocoder")
$manpage "Vocoder"
$action (_ "Processing Vocoder...")
$author (_ "Edgar-RFT and Steve Daulton")
$release 3.1.2
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; Based on vocoder.ny by Edgar-RFT
;;
;; If selected track is mono, the vocoder uses sine waves as the modulation
;; carrier, mixed with noise and radar needles according to slider settings.
;; If selected track is stereo, the right channel is used as the carrier wave.

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

$control dst (_ "Distance: (1 to 120, default = 20)") float "" 20 1 120
$control mst (_ "Output choice") choice (("BothChannels" (_ "Both Channels"))
                                         ("RightOnly" (_ "Right Only"))) 0
$control bands (_ "Number of vocoder bands") int "" 40 10 240
$control track-vl (_ "Amplitude of carrier wave (percent)") float "" 100 0 100
$control noise-vl (_ "Amplitude of white noise (percent)") float "" 0 0 100
$control radar-vl (_ "Amplitude of Radar Needles (percent)") float "" 0 0 100
$control radar-f (_ "Frequency of Radar Needles (Hz)") float "" 30 1 100


;; Return log to base 2 of x.
(defun log2 (x)
  (/ (log (float x)) (log 2.0)))

;; number of octaves from 20 Hz.
;; Maximum number of octaves is: log2(high-hz / low-hz)
;; "2.205" is for compatibility with older versions of vocoder effect.
(setf octaves (log2 (/ (/ *sound-srate* 2.205) 20)))


;; interval - number of semitones per vocoder band
(setf interval (/ (* octaves 12.0) bands))


;; Scale slider values for better control.
(setf track-vl (sqrt (/ track-vl 100.0)))
(setf noise-vol (expt (/ noise-vl 100.0) 2.0))
(setf radar-vol (sqrt (/ radar-vl 100.0)))



(defun make-radar-table (hz)
  (let ((one (/ *sound-srate*)) ;one sample period
        radar-table)
    (setf radar-table
        (stretch-abs 1 (sim (snd-const 1 one *sound-srate* one)
                            (s-rest (/ 1.0 hz)))))
    (list radar-table (hz-to-step hz) T)))


;;; The Mixer
(defun mix-noise (sig)
  (sum (cond ((= track-vl 0) 0)
             ((< track-vl 1) (mult track-vl sig))
             (t sig))
       (if (> radar-vl 0)
           (let ((r-table (make-radar-table radar-f)))
             (mult radar-vol
                   (osc (hz-to-step radar-f) 1 r-table)))
           0)
       (if (> noise-vl 0)
           (mult noise-vol (noise 1))
           0)))


;; Raise 'hz' by 'interval' semitones.
(defmacro next-hz (hz interval)
  `(let* ((prev-step (hz-to-step ,hz))
          (next-step (+ prev-step ,interval)))
    (step-to-hz next-step)))


(defmacro sumto (x y)
  `(setf ,x (sum ,x ,y)))


;;; Stereo Vocoder - returns mono sound.
(defun vocoder (sig is-mono-track)
  (let (mod-envelope
        band
        (result 0))
    (do ((i 0 (1+ i))
         (q (/ (sqrt 2.0) (/ octaves bands)))  ; quick approximation of q
         (f (next-hz 20 (/ interval 2.0))
            (next-hz f interval)))
        ((= i bands) result)
      (when is-mono-track
        (sumto (aref sig 1) (mult 0.5 (/ track-vl bands) (hzosc f))))
      (setf band (bandpass2 sig f q)) ; intermediate results (2 channels)
      (setf mod-envelope (lowpass8 (s-abs (aref band 0)) (/ f dst)))
      (sumto result (bandpass2 (mult mod-envelope (aref band 1)) f q)))))


;;; The Program
(if (= (+ track-vl noise-vol radar-vol) 0)
    (format nil (_ "Error.~%No modulation carrier."))
    (progn
      (if (arrayp *track*)
          (setf sig (vector (aref *track* 0) (mix-noise (aref *track* 1))))
          (setf sig (vector *track* (mix-noise (s-rest 0)))))
      (setf sig (vocoder sig (soundp *track*)))
      ;; Normalize *track* to 0 db peak based on first 10 million samples.
      (setf sig (scale (/ (peak sig 10000000)) sig))
      (if (or mst (soundp *track*))
          sig
          (vector (aref *track* 0) sig))))
