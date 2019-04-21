$nyquist plug-in
$version 4
$type tool analyze
$debugbutton false
$debugflags trace
$name (_ "Regular Interval Labels")
$manpage "Regular_Interval_Labels"
$action (_ "Adding equally-spaced labels to the label track...")
$author (_ "Steve Daulton")
$release 2.3.1
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; TODO: Rewrite as an AUD-DO script so as to remove the requirement for an audio selection.

;; Original version by David R. Sky (http://www.garyallendj.com/davidsky/) 2007.
;; Based on an idea by Sami Jumppanen, with contributions from
;; Alex S.Brown, Dominic Mazzoni, Pierre M.I., Gale Andrews.

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


;i18n-hint: Refers to the controls 'Number of labels' and 'Label interval'.
$control mode (_ "Create labels based on") choice (("Both" (_ "Number & Interval"))
                                                   ("Number" (_ "Number of Labels"))
                                                   ("Interval" (_ "Label Interval"))) 0
$control totalnum (_ "Number of labels") int-text "" 10 1 1000
$control interval (_ "Label interval (seconds)") float-text "" 10 0.001 3600
$control region (_ "Length of label region (seconds)") float-text "" 0 0 3600
$control adjust (_ "Adjust label interval to fit length") choice ((_ "No")
                                                                  (_ "Yes")) 0
$control labeltext (_ "Label text") string "" (_ "Label")
$control zeros (_ "Minimum number of digits in label") choice (("TextOnly" (_ "None - Text Only"))
                                                               ("OneBefore" (_ "1 (Before Label)"))
                                                               ("TwoBefore" (_ "2 (Before Label)"))
                                                               ("ThreeBefore" (_ "3 (Before Label)"))
                                                               ("OneAfter" (_ "1 (After Label)"))
                                                               ("TwoAfter" (_ "2 (After Label)"))
                                                               ("ThreeAfter" (_ "3 (After Label)"))) 2
$control firstnum (_ "Begin numbering from") int-text "" 1 0 nil
$control verbose (_ "Message on completion") choice ((_ "Details")
                                                     ("Warnings" (_ "Warnings only"))
                                                     (_ "None")) 0

(defun make-labels (&aux labels)
"Generate labels at regular intervals"
  ;; Get parameters
  (case mode
    (1  ;Number
        (setf interval 
          (if (= region 0)
              (/ (- (get-duration 1) region) totalnum)
              (/ (- (get-duration 1) region) (1- totalnum)))))
    (2  ;Interval
        (setf totalnum (get-interval-count))
        (when (= adjust 1)
          (setf interval (/ (- (get-duration 1) region) totalnum))))
    (t  ;Number and Interval
        ))
  ;; Loop for required number of labels
  (do* ((count 0 (1+ count))
        (time 0 (* count interval)))
       ((= count totalnum))
    (push (make-one-label time (+ firstnum count)) labels))
  (when (and (> region 0)(= mode 2)(= adjust 1))
    (push (make-one-label (- (get-duration 1) region)
                          (+ firstnum totalnum))
          labels))
  ;; Create confirmation message
  (when (< verbose 2)
    (message totalnum interval))
  labels)


(defun message (number interval)
"Generate output message in debug window."
  (if (> region interval)
      (setf msg (format nil (_ "Warning: Overlapping region labels.~%")))
      (setf msg ""))
  (cond
  ((= verbose 1) ;Warnings only
      (format t msg))
  (t  (if (> region 0)
          ;i18n-hint:  Type of label
          (setf labeltype (_ "region labels"))
          (setf labeltype (_ "point labels")))
      (when (and (> region 0)(= mode 2)(= adjust 1))
        (setf number (1+ number)))
      (setf msg
          ;i18n-hint:  Number of labels produced at specified intervals.
          (format nil (_ "~a~a ~a at intervals of ~a seconds.~%")
                  msg number labeltype interval))
      (if (> region 0)
          (format t (_ "~aRegion length = ~a seconds.")
                  msg region)
          (format t msg)))))


(defun get-interval-count (&aux dur)
"Number of labels when interval is specified"
  (setf dur (- (get-duration 1) region))
  (case adjust
    ;; Interval is user input value
    (0  (let ((n (truncate (/ dur interval))))
          (if (< (* n interval) dur)
              (1+ n)
              n)))
    ;; Adjust interval to fit length
    (1  (let* ((min-num (truncate (/ dur interval)))
               (max-num (1+ min-num)))
          (if (and (> min-num 0)
                   (< (abs (- interval (/ dur min-num)))
                      (abs (- interval (/ dur max-num)))))
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
    (list time (+ time region) text)))

(defun lasttrackp ()
"True when processing the final selected track"
  (let ((index (get '*track* 'index))
        (num (length (get '*selection* 'tracks))))
    (= index num)))


(setf num-before-text (<= zeros 3))
(setf zeros (1+ (rem (1- zeros) 3)))

;; Analyze plug-ins may return text message per track but
;; we only want error messages once, and only one set of labels.
(if (lasttrackp)
    (make-labels)
    "") ;No-op
