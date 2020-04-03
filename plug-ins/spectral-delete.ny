$nyquist plug-in
$version 4
$type process spectral
$name (_ "Spectral Delete")
$manpage "Spectral_Delete"
$author (_ "Steve Daulton")
$release 2.4.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


(defun sinc (x fc)
  ;; http://www.dspguide.com/ch16/1.htm
  ;; Note that fc is a fraction of the sample rate.
  (if (= x 0)
      (* 2 pi fc)
      (/ (sin (* 2 pi fc x)) x)))

(defun blackman (i M)
  ;; Where: 0 <= i <= M
  (+ 0.42
     (* -0.5 (cos (/ (* 2.0 pi i) M)))
     (* 0.08 (cos (/ (* 4 pi i) M)))))

(defun calc-kernel (size fc)
  ;; Generate windowed sinc kernel impulse
  (when (oddp size)
    (error "Size of sinc filter must be even"))
  (let ((ar (make-array (1+ size)))
        (norm 0)  ;Normalization factor
        val)
    (do ((i 0 (1+ i))
         (j size (1- j))
         (x (- halfk) (1+ x)))
        ((> i j))
      (setf val (* (sinc x fc)(blackman i size)))
      (setf norm (+ norm val))
      (setf (aref ar i) val)
      (setf (aref ar j) val))
    ;; norm is sum of all samples, but don't count middle value twice.
    (setf norm (- (* norm 2)(aref ar halfk)))
    (dotimes (i size ar)
      (setf (aref ar i)(/ (aref ar i) norm)))))

(defun get-kernel (size fc type)
  ;; type: 0 (low pass) or 1 (highpass)
  ;; Returns filter kernel as a sound.
  (let ((kernel (calc-kernel size fc)))
    (when (= type 1)
      ;; Convert kernel to high pass
      ;; https://tomroelandts.com/articles/how-to-create-a-simple-high-pass-filter
      (dotimes (i size kernel)
        (setf (aref kernel i)(* -1 (aref kernel i))))
      (incf (aref kernel halfk)))
    (snd-from-array 0 *sound-srate* kernel)))

(defun sinc-filter (sig start end impulse)
  (extract-abs start end (convolve sig impulse)))

(defmacro validate-low-hz (hz fmin fmax)
  ;; Discard if out of valid range.
  ;; Do NOT coerce into range if too high - if multiple tracks with
  ;; different sample rates, that could cause very unepected results.
  `(if (or (not ,hz) (< ,hz fmin) (> ,hz fmax))
       (setf ,hz nil)))

(defmacro validate-high-hz (hz fmin fmax)
  ;; Discard if too high. Coerce into range if too low.
  `(if (or (not ,hz) (>= ,hz fmax))
       (setf ,hz nil)
       (setf ,hz (max ,hz fmin))))

(defun dofilter (cf bw type)
  ;; type: 0 (low pass) or 1 (highpass)
  ;; Calculate kernel length (must be even)
  ;; http://www.dspguide.com/ch16/2.htm
  (setf klength (/ 4.0 bw))
  (setf halfk (round (/ klength 2)))
  (setf klength (* 2 halfk))
  (let ((imp (get-kernel klength cf type))
        (start (/ halfk *sound-srate*))
        (dur (get-duration 1)))
    (multichan-expand #'sinc-filter *track* start (+ start dur) imp)))

(defun bandwidth (hz)
  ;; Set bandwidth ratio of each filter as 1% of filter frequency.
  (* hz 0.01))

(defun bw-ratio (hz)
  ;; Bandwidth ratio is required as a fraction of the sampling rate
  (/ (bandwidth hz) *sound-srate*))

(defun filter ()
  (when (< *sound-srate* 100)
    (throw 'err (_ "Error.~%Track sample rate below 100 Hz is not supported.")))
  (let* ((f0 (get '*selection* 'low-hz))
         (f1 (get '*selection* 'high-hz))
         (fc (get '*selection* 'center-hz))
         ; If frequency too low, filter length is too large.
         (fmin (* 0.002 *sound-srate*))
         (fmax (* 0.498 *sound-srate*))
         (tn (truncate len))
         (transition (truncate (* 0.01 *sound-srate*))) ; 10 ms
         (t1 (min transition (/ tn 2)))        ; fade in length (samples)
         (t2 (max (- tn transition) (/ tn 2))) ; length before fade out (samples)
         (breakpoints (list t1 1.0 t2 1.0 tn))
         (env (snd-pwl 0.0 *sound-srate* breakpoints)))
    (validate-low-hz f0 fmin fmax)
    (validate-high-hz f1 fmin fmax)
    ;; Handle very narrow selections.
    ;; This may cause f0 or f1 to 'slightly' exceed fmin fmax.
    (when (and f0 f1 (< (- f1 f0) (* fc 0.02)))
      (setf f0 (* fc 0.99))
      (setf f1 (* fc 1.01)))
    (when f0
      (setf lp-width (bw-ratio f0))
      (setf f0 (/ f0 *sound-srate*)))
    (when f1
      (setf hp-width (bw-ratio f1))
      (setf f1 (/ f1 *sound-srate*)))
    ;(format t "Low: ~a    High: ~a" (if f0 (* f0 *sound-srate*) nil) (if f1 (* f1 *sound-srate*) nil))
    (if (not (or f0 f1))
        ""  ;may occur if multiple tracks with diferent sample rates
        (sim
          (mult env
              (if f0 (dofilter f0 lp-width 0) 0))
          (mult env
              (if f1 (dofilter f1 hp-width 1) 0))
          (mult (diff 1.0 env) *track*)))))


(catch 'err (filter))
