$nyquist plug-in
$version 4
$type analyze
;i18n-hint: Name of effect that labels sounds
$name (_ "Label Sounds")
$debugbutton false
$author (_ "Steve Daulton")
$release 3.0.4-2
$copyright (_ "GNU General Public License v2.0 or later")

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control THRESHOLD (_ "Threshold level (dB)") float "" -30 -100 0
$control MEASUREMENT (_ "Threshold measurement") choice (("peak" (_ "Peak level"))
                                                         ("avg" (_ "Average level"))
                                                         ("rms" (_ "RMS level"))) 0
$control SIL-DUR (_ "Minimum silence duration") time "" 1 0.01 3600
$control SND-DUR (_ "Minimum label interval") time "" 1 0.01 7200
$control TYPE (_ "Label type") choice (("before" (_ "Point before sound"))
                                       ("after" (_ "Point after sound"))
                                       ("around" (_ "Region around sounds"))
                                       ("between" (_ "Region between sounds"))) 2
$control PRE-OFFSET (_ "Maximum leading silence") time "" 0 0 nil
$control POST-OFFSET (_ "Maximum trailing silence") time "" 0 0 nil
;i18n-hint: Do not translate '##1'
$control TEXT (_ "Label text") string "" (_ "Sound ##1")


(setf thresh-lin (db-to-linear THRESHOLD))
(setf max-labels 10000)  ;max number of labels to return


(defun format-time (s)
  ;;; format time in seconds as h m s.
  ;;; (Only used for error message if selection > 2^31 samples.)
  (let* ((hh (truncate (/ s 3600)))
         (mm (truncate (/ s 60))))
  ;i18n-hint: hours minutes and seconds. Do not translate "~a".
  (format nil (_ "~ah ~am ~as")
      hh (- mm (* hh 60)) (rem (truncate s) 60))))


(defun parse-label-text ()
  ;;; Special character '#' represents an incremental digit.
  ;;; Return '(digits num pre-txt post-txt) for 
  ;;; (number-of-digits, initial-value, text-before-number, text-after-number),
  ;;;  or NIL.
  ;;; 'initial-value' is a positive integer or zero (default).
  ;;; Only the first instance of #'s are considered 'special'.
  (let ((hashes 0)
        (num nil)
        (negative nil)
        (pre-txt "")
        (post-txt "")
        ch)
    (dotimes (i (length TEXT))
      (setf ch (char TEXT i))
      (cond
        ((and (string= post-txt "") (char= ch #\#))
            (incf hashes))
        ((and (> hashes 0) (string= post-txt ""))
            (cond
              ((digit-char-p ch)
                (if num
                    (setf num (+ (* num 10) (digit-char-p ch)))
                    (setf num (digit-char-p ch))))
              ((and (not num)(char= ch #\-))
                (setf negative t))
              (t (setf post-txt (string ch)))))
        ((= hashes 0) ;special '#' not yet found
            (string-append pre-txt (string ch)))
        (t ;run out of #'s and digit characters.
            (string-append post-txt (string ch)))))
      (when negative
        (setf num (- num)))
      ;; Replace string literal hash characters.
      (when (and (> hashes 0) (not num))
        (dotimes (i hashes)
          (string-append pre-txt "#")))
      (list hashes num pre-txt post-txt)))


(defun pad (n d)
  ;;; Return string, int 'n' padded to 'd' digits, or empty string.
  ;;; Used in formatting label text.
  (cond
    (n
      (let ((negative (minusp n))
            (n (format nil "~a" (abs n))))
        (while (< (length n) d)
          (setf n (format nil "0~a" n)))
        (if negative
            (format nil "-~a" n)
            n)))
    (t "")))


(defun to-mono (sig)
  ;;; Coerce sig to mono.
  (if (arrayp sig)
      (s-max (s-abs (aref sig 0))
             (s-abs (aref sig 1)))
      sig))


(defun to-avg-mono (sig)
  ;;; Average of stereo channels
  (if (arrayp sig)
      (mult 0.5 (sum (aref sig 0)(aref sig 1)))
      sig))


(defun reduce-srate (sig)
  ;;; Reduce sample rate to (about) 100 Hz.
  (let ((ratio (round (/ *sound-srate* 100))))
    (cond
      ((= MEASUREMENT 0)  ;Peak
        (let ((sig (to-mono sig)))
          (snd-avg sig ratio ratio OP-PEAK)))
      ((= MEASUREMENT 1)  ;Average absolute level
        (let ((sig (to-avg-mono (s-abs sig))))
          (snd-avg sig ratio ratio OP-AVERAGE)))
      (t  ;RMS
        (if (arrayp sig)
            ;; Stereo RMS is the root mean of all (samples ^ 2) [both channels]
            (let* ((sig (mult sig sig))
                   (left-mean-sq (snd-avg (aref sig 0) ratio ratio OP-AVERAGE))
                   (right-mean-sq (snd-avg (aref sig 1) ratio ratio OP-AVERAGE)))
              (s-sqrt (mult 0.5 (sum left-mean-sq right-mean-sq))))
            (rms sig))))))


(defun find-sounds (sig srate)
  ;;; Return a list of sounds that are at least 'SND-DUR' long,
  ;;; separated by silences of at least 'SIL-DUR'.
  (let ((sel-start (get '*selection* 'start))
        (snd-list ())
        (sample-count 0)
        (sil-count 0)
        (snd-count 0)
        (snd-start 0)
        (label-count 0)
        (snd-samples (* SND-DUR srate))
        (sil-samples (* SIL-DUR srate)))
    ;;Ignore samples before time = 0
    (when (< sel-start 0)
      (setf sample-count (truncate (* (abs sel-start) srate)))
      (dotimes (i sample-count)
        (snd-fetch sig)))
    ;;Main loop to find sounds.
    (do ((val (snd-fetch sig) (snd-fetch sig)))
        ((not val) snd-list)
      (cond
        ((< val thresh-lin)
            (when (and (>= sil-count sil-samples)(>= snd-count snd-samples))
              ;convert sample counts to seconds and push to list.
              (push (list (/ snd-start srate)
                          (/ (- sample-count sil-count) srate))
                    snd-list)
              (incf label-count)
              (when (= label-count max-labels)
                (format t (_ "Too many silences detected.~%Only the first 10000 labels added."))
                (return-from find-sounds snd-list))
              (setf snd-count 0)) ;Pushed to list, so reset sound sample counter.
            (when (> snd-count 0) ;Sound is shorter than snd-samples, so keep counting.
              (incf snd-count))
            (incf sil-count))
        ;; Above threshold.
        (t  (when (= snd-count 0) ;previous sound was push, so this is a new sound.
              (setf snd-start sample-count))
            (setf sil-count 0)
            (incf snd-count)))
      (incf sample-count))
    ;; Check for final sound
    (when (> snd-count 0)
      (push (list (/ snd-start srate)
                  (/ (- sample-count sil-count) srate))
            snd-list))
    snd-list))


(defun return-labels (snd-list)
  (setf textstr (parse-label-text))
  ; Selection may extend before t=0
  ; Find t=0 relative to selection so we can ensure 
  ; that we don't create hidden labels.
  (setf t0 (- (get '*selection* 'start)))
  (setf t1 (- (get '*selection* 'end)))
  (let ((label-start t0)
        (label-end t1)
        (label-text "")
        (labels ())
        (final-sound (if (= TYPE 3) 1 0)) ;TYPE 3 = regions  between sounds.
        ;; Assign variables to parsed label text
        (digits (first textstr))
        (num (second textstr))
        (pre-txt (third textstr))
        (post-txt (fourth textstr)))
    ;snd-list is in reverse chronological order
    (do ((i (1- (length snd-list)) (1- i)))
        ((< i final-sound) labels)
      (case TYPE
        (3  ;;label silences.
            (setf start-time (second (nth i snd-list)))
            (setf end-time (first (nth (1- i) snd-list)))
            ;don't overlap next sound
            (setf label-start (min end-time (+ start-time PRE-OFFSET)))
            ;don't overlap previous sound
            (setf label-end (max start-time (- end-time POST-OFFSET)))
            ;ensure end is not before start
            (when (< (- label-end label-start) 0)
              (setf label-start (/ (+ label-end label-start) 2.0))
              (setf label-end label-start)))
        (t  ;; labelling sounds
            (setf start-time (first (nth i snd-list)))
            (setf end-time (second (nth i snd-list)))
            ;don't overlap t0 or previous sound.
            (setf label-start (max t0 label-start (- start-time PRE-OFFSET)))
            (setf label-end (+ end-time POST-OFFSET))
            ;; Don't overlap following sounds.
            (when (> i 0)
              (setf label-end (min label-end (first (nth (1- i) snd-list)))))))
      (setf label-text (format nil
                               "~a~a~a"
                               pre-txt
                               (pad num digits)
                               post-txt))
      (case TYPE
        (0 (push (list label-start label-text) labels)) ;point label before sound
        (1 (push (list label-end label-text) labels))   ;point label after sound
        (2 (push (list label-start label-end label-text) labels)) ;sound region
        (t (push (list label-start label-end label-text) labels)));silent region
      ;Earliest allowed start time for next label.
      (setf label-start end-time)
      ;num is either an int or nil
      (when num (incf num)))))


(let ((sig (reduce-srate *track*)))
  (setf *track* nil)
  (setf snd-list (find-sounds sig (snd-srate sig)))
  (cond
    ((= (length snd-list) 0)
      (format nil
              (_ "No sounds found.~%~
                 Try lowering 'Threshold level (dB)'.")))
    ((and (= TYPE 3)
          (= (length snd-list) 1))
      (format nil
              (_ "Labelling regions between sounds requires~%~
                 at least two sounds.~%~
                 Only one sound detected.")))
    (t
      (return-labels snd-list))))
