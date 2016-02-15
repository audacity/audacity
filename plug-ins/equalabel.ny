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

;control mode "Use 'Number of labels' OR 'Label interval'" choice "Number of labels,Label interval" 0
;control totalnum "Number of labels" int-text "" 10 1 1000
;control interval "Label interval (seconds)" float-text "" 60 0.001 3600
;control adjust "Adjust label interval to fit length" choice "No,Yes" 0
;control labeltext "Label text" string "" "Label" ""
;control zeros "Minimum number of digits in label" choice "None - text only,1 (before label),2 (before label),3 (before label),1 (after label),2 (after label),3 (after label)" 2
;control firstnum "Begin numbering from" int-text "" 1 0 nil


(defun make-labels ()
"Generate labels at regular intervals"
  (setf labels ())
  ;; Get parameters
  (case mode
    (0  ;Number of Labels
        (setf interval (/ (get-duration 1) totalnum)))
    (1  (setf totalnum (get-interval-count))
        (when (= adjust 1)
          (setf interval (/ (get-duration 1) totalnum)))
        (check-number-of-labels)))
  ;; Loop for required number of labels
  (do* ((count 0 (1+ count))
        (time 0 (* count interval)))
       ((= count totalnum) labels)
    (push (make-one-label time (+ firstnum count)) labels)))

(defun check-number-of-labels ()
"Throw error if excessive number of labels ('Interval' mode only)"
  (when (> totalnum 1000)
    (throw 'err
      (format nil "Too many labels.~%~%~
        Selection length is ~a seconds and~%~
        Label interval is ~a seconds~%~
        giving a total of ~a labels.~%~
        Maximum number of labels from this effect is 1000.~%~
        Please use a shorter selection, or a longer Label interval."
        (formatgg (get-duration 1))
        (formatgg interval)
        (if (= adjust 1)
            (round (/ (get-duration 1) interval))
            (1+ (round (/ (get-duration 1) interval))))))))

(defun get-interval-count ()
"Number of labels when interval is specified"
  (case adjust
    ;; Interval is user input value
    (0  (let ((n (truncate (/ (get-duration 1) interval))))
          (if (< (* n interval)(get-duration 1))
              (1+ n)
              n)))
    ;; Adjust interval to fit length
    (1  (let* ((min-num (truncate (/ (get-duration 1) interval)))
               (max-num (1+ min-num)))
          (if (and (> min-num 0)
                   (< (abs (- interval (/ (get-duration 1) min-num)))
                      (abs (- interval (/ (get-duration 1) max-num)))))
              min-num
              max-num)))))

(defun make-one-label (time num)
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

(defun lasttrackp ()
"true when processing the final selected track"
  (let ((index (get '*track* 'index))
        (num (length (get '*selection* 'tracks))))
    (= index num)))

(defun formatgg (num)
"Similar to float-format %g but more decimal places"
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

;; Analyze plug-ins may return text message per track but
;; we only want error messages once, and only one set of labels.
(if (lasttrackp)
    (catch 'err (make-labels))
    nil)
