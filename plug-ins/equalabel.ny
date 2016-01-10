;nyquist plug-in
;version 4
;type analyze
;name "Regular Interval Labels..."
;action "Adding equally-spaced labels to the label track..."
;author "Steve Daulton"
;copyright "Released under terms of the GNU General Public License version 2"

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; Original version by David R. Sky (http://www.garyallendj.com/davidsky/) 2007.
;; Based on an idea by Sami Jumppanen, with contributions from
;; Alex S.Brown, Dominic Mazzoni, Pierre M.I., Gale Andrews, Steve Daulton.
;; This version by Steve Daulton (http://easyspacepro.com) 2016

;control mode "Use 'Number of labels' OR 'Label interval'" choice "Number of Labels,Label Interval" 0
;control number "Number of labels" int-text "" 10 1 1000
;control interval "Label interval (seconds)" float-text "" 60 0.001 1000
;control adjust "Adjust label interval to fit length" choice "No,Yes" 0
;control labeltext "Label text" string "" "Label" ""
;control zeros "Minimum number of digits in label" choice "None - text only,1 (before label),2 (before label),3 (before label),1 (after label),2 (after label),3 (after label)" 2
;control labelnum "Begin numbering from" int-text "" 1 0 nil


(defun make-labels ()
  (when (and (= mode 1)(= adjust 1)) ;adjust interval to fit
      (setf interval (get-interval)))
  (validate)
  (let ((labels ()))
    (cond
      ((= mode 0) ;number of labels
        (setf interval (/ (get-duration 1) number))
        (do* ((i 0 (1+ i))
              (labelnum labelnum (1+ labelnum)))
             ((= i number))
          (push (make-label (* i interval) labelnum) labels))
        ;print what we've done to debug window
        (if (= number 1)
            (format t "1 label requested.")
            (format t "~a labels at intervals of ~a seconds."
              number interval)))
      (t
        (setf counter 0)
        (do* ((i 0 (1+ i))
              (labelnum labelnum (1+ labelnum))
              (time 0 (* i interval)))
             ((>= (round-to-sample time) (get-duration 1)))
          (incf counter)
          (push (make-label time labelnum) labels))
        ;print what we've done to debug window
        (if (and (= adjust 0)(/= (* counter interval)(get-duration 1)))
            (if (= counter 1)
                (format t "1 label requested.")
                (format t "~a labels at regular intervals of ~a seconds.~%~
                  Final label at ~a seconds from end of selection."
                  counter
                  interval
                  (- (get-duration 1) (* (1- counter) interval))))
            (format t "~a labels at regular intervals of ~a seconds."
              counter interval))))
    ;return labels
    labels))

(defun validate ()
  (when (= mode 1)  ;Label interval
    (when (> (get-duration 1) (round-to-sample (* 1000 interval)))
      (throw 'err
        (format nil "Too many labels.~%~%~
          Selection length is ~a seconds and~%~
          Label interval is ~a seconds~%~
          giving a total of ~a labels.~%~
          Maximum number of labels from this effect is 1000.~%~
          Please use a shorter selection, or a longer Label interval."
          (trim-trailing-zeros (get-duration 1))
          (trim-trailing-zeros interval)
          (if (= adjust 1)
              (round (/ (get-duration 1) interval))
              (1+ (round (/ (get-duration 1) interval)))))))))

(defun round-to-sample (time)
"Round time in seconds to nearest sample period."
  (let ((samples (round (* time *sound-srate*))))
    (/ samples *sound-srate*)))

(defun make-label (time num)
"Make a single label"
  (let* ((num-text (format nil "~a" num))
         (non-zero-digits (length num-text)))
    (if (= zeros 0)
        (setf num-text "")
        (dotimes (i (max 0 (- zeros non-zero-digits)))
          (setf num-text (format nil "~a~a" "0" num-text))))
    (if num-before-text
      (setf text (format nil "~a~a" num-text labeltext))
      (setf text (format nil "~a~a" labeltext num-text)))
    (list time text)))

(defun get-interval ()
"Get adjusted interval to fit duration"
  (let* ((min-num (truncate (/ (get-duration 1) interval)))
         (max-num (1+ min-num)))
    (if (and (> min-num 0)
             (< (abs (- interval (/ (get-duration 1) min-num)))
                (abs (- interval (/ (get-duration 1) max-num)))))
        (/ (get-duration 1) min-num)
        (/ (get-duration 1) max-num))))

(defun lasttrackp ()
"true when processing the final selected track"
  (let ((index (get '*track* 'index))
        (num (length (get '*selection* 'tracks))))
    (= index num)))

(defun trim-trailing-zeros (num)
  ;; sometimes need more precission than "%g".
  (cond
   ((/= num (truncate num)) ; not integer
      (setf *float-format* "%.5f")
      (let ((numtxt (format nil "~a" num)))
        (do* ((i (1- (length numtxt)) (1- i))
              (ch (char numtxt i)(char numtxt i)))
             ((char/= ch #\0))
          (setf numtxt (subseq numtxt 0 i)))
        (setf *float-format* "%g")
        numtxt))
    (t num)))


(setf num-before-text (<= zeros 3))
(setf zeros (1+ (rem (1- zeros) 3)))

;; Analyze plug-ins may return text message per track
;; but we only want error messages once, and we only want
;; one set of labels.
(if (lasttrackp)
    (catch 'err (make-labels))
    nil)
