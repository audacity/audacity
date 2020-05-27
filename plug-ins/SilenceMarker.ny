$nyquist plug-in
$version 4
$type analyze
$debugbutton false
$debugflags trace
$name (_ "Silence Finder")
$manpage "Silence_Finder"
$author (_ "Steve Daulton")
$release 2.4.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; Original version by Alex S. Brown, PMP (http://www.alexsbrown.com)
;;
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

$control threshold (_ "Treat audio below this level as silence (dB)") float "" -30 -100 0
$control min-silence (_ "Minimum duration of silence (seconds)") float "" 1.0 0.1 5.0
$control label-position (_ "Label placement (seconds before silence ends)") float "" 0.3 0.0 1.0

(setf threshold (db-to-linear threshold))
; Label is usually offset to earlier time.
(setf label-position (- label-position))

;i18n-hint: Abbreviation of "Silence".
(setf *labeltxt* (_ "S"))
(setf *labels* NIL)


(defun to-mono (sig)
  ;;; coerce sig to mono.
  (if (arrayp sig)
    (s-max (s-abs (aref sig 0))
           (s-abs (aref sig 1)))
    sig))

(defun reduce-srate (sig)
  ;;; Reduce sample rate to (about) 100 Hz.
  (let ((ratio (round (/ *sound-srate* 100))))
    (snd-avg sig ratio ratio OP-PEAK)))

(defun add-label (samples srate offset)
  ;;; Add new label to *labels*
  (let ((time (+ (/ samples srate) offset)))
    (push (list time *labeltxt*) *labels*)))

(defun format-time (s)
  ;;; format time in seconds as h m s.
  (let* ((hh (truncate (/ s 3600)))
         (mm (truncate (/ s 60))))
  ;i18n-hint: hours minutes and seconds. Do not translate "~a".
  (format nil (_ "~ah ~am ~as")
      hh (- mm (* hh 60)) (rem (truncate s) 60))))

(defun label-silences (sig)
  ;;; Label silences that are longer than 'min-len' samples.
  (let* ((sample-count 0)
         (sil-count 0)
         (srate (snd-srate sig))
         (min-len (* min-silence srate)))
    (do ((val (snd-fetch sig) (snd-fetch sig)))
        ((not val) sil-count)
      (cond
        ((< val threshold)
            (incf sil-count))
        (t  (when (> sil-count min-len)
              (add-label sample-count srate label-position))
            (setf sil-count 0)))
      (incf sample-count))
    ;; If long trailing silence, add final label at 'min-silence' AFTER last sound.
    (when (> sil-count min-len)
      (setf final-silence (- sample-count sil-count))
      (add-label final-silence srate min-silence))
    *labels*))


;;  Bug 2352: Throw error if selection too long for Nyquist.
(let* ((dur (- (get '*selection* 'end)
               (get '*selection* 'start)))
       (samples (* dur *sound-srate*))
       (max-samples (1- (power 2 31))))
  (if (>= samples max-samples)
      (format nil "Error.~%Selection must be less than ~a."
              (format-time (/ max-samples *sound-srate*)))
      ;; Selection OK, so run the analyzer.
      (let ((sig (reduce-srate (to-mono *track*))))
        (setf *track* nil)  ;free *track* from memory
        (if (label-silences sig)
            *labels*
            (_ "No silences found.
Try reducing the silence level and
the minimum silence duration.")))))
