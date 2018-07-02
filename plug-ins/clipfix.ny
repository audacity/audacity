$nyquist plug-in
$version 4
$type process
$preview enabled
$name (_ "Clip Fix")
$manpage "Clip_Fix"
$action (_ "Reconstructing clips...")
$author (_ "Benjamin Schwartz and Steve Daulton")
$release 2.3.0
$copyright (_ "Licensing confirmed under terms of the GNU General Public License version 2")

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;; Algorithm by Benjamin Schwartz
;; Clip Fix is a simple, stupid (but not blind) digital-clipping-corrector
;; The algorithm is fairly simple:
;; 1. Find all clipped regions
;; 2. Get the slope immediately on either side of the region
;; 3. Do a cubic spline interpolation.
;; 4. Go to next region


$control threshold (_ "Threshold of Clipping (%)") float "" 95 0 100
$control gain (_ "Reduce amplitude to allow for restored peaks (dB)") float "" -9 -30 0

(setf threshold (/ threshold 100))
(setf gain (db-to-linear gain))
(setf buffersize 100000)
(setf slopelength 4)  ; number of samples used to calculate the exit / re-entry slope


(defun declip (sig thresh peak)
  (let* ((threshold (* thresh peak))
         (ln (truncate len))
         (finalbufsize (rem ln buffersize)))
    ;; Calculate the number of buffers we can process.
    ;; if final buffer is not large enough for de-clipping we
    ;; will just add it on the end as is.
    (if (>= finalbufsize slopelength)
        (setf buffercount (1+ (/ ln buffersize))) 
        (setf buffercount (/ ln buffersize)))
    ;;; Make output sequence from processed buffers
    (setf out
      (seqrep (i buffercount)
        (let* ((step (min buffersize (- ln (* i buffersize))))
               (buffer (snd-fetch-array sig step step))
               (processed (process buffer threshold step)))
          (cue (mult gain
                    (snd-from-array 0 *sound-srate* processed))))))
    ;;; If there's unprocessed audio remaining, add it to the end
    (if (and (> finalbufsize 0)(< finalbufsize slopelength))
        (seq out (cue (getfinalblock sig finalbufsize gain)))
        out)))


(defun getfinalblock (sig step gain)
  (let ((block (snd-fetch-array sig step step)))
    (mult gain (snd-from-array 0 *sound-srate* block))))


(defun process (buffer threshold bufferlength)
  ;;; Find threshold crossings
  (setf exit-list ())         ; list of times when waveform exceeds threshold
  (setf return-list ())       ; list of times when waveform returns below threshold
  ;; Limitation of algorithm: the first and last 'slopelength' at ends of buffer are ignored
  ;; so that we have enough samples beyond the threshold crossing to calculate the slope.
  (let ((last-sample (- bufferlength slopelength)))
    (do ((i slopelength (1+ i)))
        ((>= i last-sample))
      (if (>= (abs (aref buffer i)) threshold)
          (when (< (abs (aref buffer (- i 1))) threshold)   ; we just crossed threshold
            (push (- i 1) exit-list))
          (when (>= (abs (aref buffer (- i 1))) threshold)  ; we just got back in range
            (push i return-list)))))
  ;; Reverse lists back into chronological order.
  ;; This is faster than appending values in chronological order.
  (setf exit-list (reverse exit-list))
  (setf return-list (reverse return-list))
  ;; If the audio begins in a clipped region, discard the first return
  (when (>= (abs (aref buffer (1- slopelength))) threshold)
    (setq return-list (cdr return-list)))
  ;; Interpolate between each pair of exit / entry points
  (let ((slopelen (1- slopelength)))
    (mapc (lambda (t0 t1)
            (interpolate buffer t0 t1 slopelen))
          exit-list return-list))
  buffer)


(defun interpolate (buffer t0 t1 dur)
  "Cubic spline interpolation"
  (let* ((d0 (/ (- (aref buffer t0) (aref buffer (- t0 dur))) dur)) ; slope at start
         (d1 (/ (- (aref buffer (+ t1 dur)) (aref buffer t1)) dur)) ; slope at end
         (m (/ (+ d1 d0) (* (- t1 t0) (- t1 t0))))
         (b (- (/ d1 (- t1 t0)) (* m t1))))
    (do ((j (1+ t0) (1+ j)))
        ((= j t1))
      (setf (aref buffer j)
        (+ (* (- t1 j) (/ (aref buffer t0) (- t1 t0)))
           (* (- j t0) (/ (aref buffer t1) (- t1 t0)))
           (* (- j t0) (- j t1) (+ (* m j) b)))))))


;; (get '*selection* 'peak) introduced in Audacity 2.1.3
(multichan-expand #'declip *track* threshold (get '*selection* 'peak))
