$nyquist plug-in
$version 4
$type tool analyze
$debugbutton false
$debugflags trace
$name (_ "Regular Interval Labels")
$author (_ "Steve Daulton")
$release 2.3.1-2
$copyright (_ "GNU General Public License v2.0 or later")


;; Original version by David R. Sky (http://www.garyallendj.com/davidsky/) 2007.
;; Based on an idea by Sami Jumppanen, with contributions from
;; Alex S.Brown, Dominic Mazzoni, Pierre M.I., Gale Andrews.
;;
;; TODO: Rewrite as an AUD-DO script so as to remove the requirement for
;; an audio selection.

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


; i18n-hint: Refers to the controls 'Number of labels' and 'Label interval'.
$control MODE (_ "Create labels based on") choice (("Both" (_ "Number and Interval"))
                                                   ("Number" (_ "Number of Labels"))
                                                   ("Interval" (_ "Label Interval"))) 0
$control TOTALNUM (_ "Number of labels") int-text "" 10 1 1000
$control INTERVAL (_ "Label interval (seconds)") float-text "" 10 0.001 3600
$control REGION (_ "Length of label region (seconds)") float-text "" 0 0 3600
$control ADJUST (_ "Adjust label interval to fit length") choice ((_ "No")
                                                                  (_ "Yes")) 0
$control LABELTEXT (_ "Label text") string "" (_ "Label")
$control ZEROS (_ "Minimum number of digits in label") choice (("TextOnly" (_ "None - Text Only"))
                                                               ("OneBefore" (_ "1 (Before Label)"))
                                                               ("TwoBefore" (_ "2 (Before Label)"))
                                                               ("ThreeBefore" (_ "3 (Before Label)"))
                                                               ("OneAfter" (_ "1 (After Label)"))
                                                               ("TwoAfter" (_ "2 (After Label)"))
                                                               ("ThreeAfter" (_ "3 (After Label)"))) 2
$control FIRSTNUM (_ "Begin numbering from") int-text "" 1 0 nil
$control VERBOSE (_ "Message on completion") choice ((_ "Details")
                                                     ("Warnings" (_ "Warnings only"))
                                                     (_ "None")) 0


(defun make-labels (num-txt zeropad &aux labels)
"Generate labels at regular intervals"
  ;; Get parameters
  (case MODE
    (1  ; Based on Number
        (setf intervals
          (if (= REGION 0)
              (/ (get-safe-duration) TOTALNUM)
              (/ (get-safe-duration) (1- TOTALNUM))))
        (setf total TOTALNUM))
    (2  ; Based on Interval
        (setf total (get-interval-count))
        (if (= ADJUST 1)
            (setf intervals (/ (get-safe-duration) total))
            (setf intervals INTERVAL)))
    (t  ; Number and Interval
        (psetq total TOTALNUM
               intervals INTERVAL)))
  ;; Loop for required number of labels
  (do* ((count 0 (1+ count))
        (time 0 (* count intervals)))
       ((= count total))
    (push (make-one-label time (+ FIRSTNUM count) num-txt zeropad)
          labels))

  (when (and (> REGION 0)(= MODE 2)(= ADJUST 1))
    (push (make-one-label (get-safe-duration)
                          (+ FIRSTNUM total)
                          num-txt
                          zeropad)
          labels))
  ;; Create confirmation message
  (when (< VERBOSE 2)
    (message total intervals))
  labels)


(defun message (number intervals)
"Generate output message in debug window."
  (if (= number 0)
      (setf msg (format nil( _ "Error: There is insufficient space to create labels.~%")))
      (if (> REGION intervals)
         (setf msg (format nil (_ "Warning: Overlapping region labels.~%")))
         (setf msg "")))
  (cond
  ((= VERBOSE 1)  ; Warnings only
      (format t msg))
  (t  (if (> REGION 0)
          ; i18n-hint:  Type of label
          (setf labeltype (_ "region labels"))
          (setf labeltype (_ "point labels")))
      (when (and (> REGION 0)(= MODE 2)(= ADJUST 1))
        (setf number (1+ number)))
      (setf msg
          ; i18n-hint:  Number of labels produced at specified intervals.
          (format nil (_ "~a~a ~a at intervals of ~a seconds.~%")
                  msg number labeltype intervals))
      (if (> REGION 0)
          (format t (_ "~aRegion length = ~a seconds.")
                  msg REGION)
          (format t msg)))))


(defun get-interval-count (&aux dur)
"Number of labels when interval is specified"
  (setf dur (get-safe-duration))
  (case ADJUST
    ;; Interval is user input value
    (0  (let ((n (truncate (/ dur INTERVAL))))
          (if (< (* n INTERVAL) dur)
              (1+ n)
              n)))
    ;; Adjust space between labels to fit length
    (1  (let* ((min-num (truncate (/ dur INTERVAL)))
               (max-num (1+ min-num)))
          (if (and (> min-num 0)
                   (< (abs (- INTERVAL (/ dur min-num)))
                      (abs (- INTERVAL (/ dur max-num)))))
              min-num
              max-num)))))


(defun make-one-label (time num num-txt zeropad)
"Make a single label"
  (let* ((num-text (format nil "~a" num))
         (non-zero-digits (length num-text)))
    (if (= zeropad 0)
        (setf num-text "")
        (dotimes (i (max 0 (- zeropad non-zero-digits)))
          (setf num-text (format nil "~a~a" "0" num-text))))
    (if num-txt  ; Number before text in label.
      (setf text (format nil "~a~a" num-text LABELTEXT))
      (setf text (format nil "~a~a" LABELTEXT num-text)))
    (list time (+ time REGION) text)))


(defun lasttrackp ()
"True when processing the final selected track"
  (let ((index (get '*track* 'index))
        (num (length (get '*selection* 'tracks))))
    (= index num)))


(defun get-safe-duration ()
   "Returns a safe duration for the labels to be distributed in"
   (let ((duration (- (get-duration 1) REGION)))
      (if (< duration 0)
          0
          duration)))


(setf num-before-text (<= ZEROS 3))
(setf zeropad (1+ (rem (1- ZEROS) 3)))

;; Analyze plug-ins may return text message per track but
;; we only want error messages once, and only one set of labels.
(if (lasttrackp)
    (make-labels num-before-text zeropad)
    "") ; No-op
