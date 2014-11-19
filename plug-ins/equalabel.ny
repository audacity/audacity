;nyquist plug-in
;version 3
;type analyze
;name "Regular Interval Labels..."
;action "Adding equally-spaced labels to the label track..."
;author "David R. Sky"
;copyright "Released under terms of the GNU General Public License version 2"

;; by David R. Sky (http://www.garyallendj.com/davidsky/), June-October 2007.
;; Code for label placement based on silencemarker.ny by Alex S.Brown.
;; Updated by Steve Daulton (http://easyspacepro.com)

;; Released under terms of the GNU General Public License version 2
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .
;; Thanks Sami Jumppanen for plug-in suggestion.
;; Thanks Dominic Mazzoni, Pierre M.I., Gale Andrews 
;; for improvement suggestions.
;; Disallow labels before zero by Steve Daulton, September 2010.
;; Enhanced label numbering by Steve Daulton, April 17th 2011.
;; Error checking by Steve Daulton, April 18th 2011.
;; Final Label bug fixed and minor GUI modifications. SD Apr 2011
;; Requires Audacity 1.3.4 or later.

;control start "Time to place first label [seconds]" string " " "0.0"
;control placement "Label placement method" choice "Label interval,Number of labels" 0
;control time "Set either: Label interval [seconds]" string " " "60.0"
;control label-number "Or: Number of labels" int "" 10 2 100
;control text "Label text" string "" "Label"
;control labelnum "Minimum number of digits in label" choice "None - text only,1 (before label),2 (before label),3 (before label),1 (after label),2 (after label),3 (after label)" 2
;control countfrom "Begin numbering from" int "" 1 0 100
;control include "Add final label" choice "No,Yes" 0
;control t-choice "Adjust label interval to fit length" choice "No,Yes" 1

; function to convert string to list
(defun string-to-list (str)
	(read (make-string-input-stream (format nil "(~a)" str))))


; function to calculate new time value 
; if user value does not create equal duration final audio chunk
; returns new time value
(defun new-time (time dur check-labels)
	; calculate duration of last audio chunk
	(setf last-label (- dur (* time check-labels)))
	(if (< last-label (* time 0.5)) ; last chunk is less than 1/2 time
		(/ dur (- check-labels 1))
		(/ dur check-labels)))


; function to add labels to the label track
; from silencemarker.ny by Alex S. Brown
(defun add-label (l-time l-text)
 (setq label-list (cons (list l-time l-text) label-list)))


; function to add label number
(defun add-number (i text)
	(let* ((num (+ countfrom i))
				(appendt (if (> labelnum 3) t nil))
				(labelnum (if (> labelnum 3)(- labelnum 3) labelnum))
				(sign (if (< num 0) "-" ""))
				(num (abs num))
				(zeros (case (- labelnum (length (format nil "~a" num)))
					(1 "0")
					(2 "00")
					(t ""))))
		(if appendt
			(format nil "~a~a~a~a" text sign zeros num) ;append number to text
			(format nil "~a~a~a~a" sign zeros num text)))) ;prepend number


; function to calculate 'time' based on number of labels
(defun num-of-label-time ()
	(setf time (/ dur label-number)); set label interval
	(if (= include 0)
			(setf labels label-number)
			(setf labels (1+ label-number))))


; function to calculate 'time' based on user-selected time interval
; calculate number of labels in selection
; this includes label at start of selection
; which is label number 0 if numbers are prepended to label text
; if final label is unwanted, decrement number of labels by 1
(defun user-selected-time ()
	(setf labels  (if (= include 0)
			(truncate (/ dur time))
			(+ 1 (truncate (/ dur time)))))
	; setf check-labels: number of labels if user includes last label
	(setf check-labels (+ 1 (truncate (/ dur time))))
	; depending whether user wants specified time used 
	; or wants equal duration audio chunks including last chunk
	; user time may create equal audio chunks even for last chunk
	(setf time 
		(if (= t-choice 0) ; final segment need not be equal
			time ; use user time value...
			; ...otherwise calculate time for equal duration audio chunks
			; if user time value creates equal final audio segment duration anyway 
			; then use user interval
			(if (= dur (* time (- check-labels 1)))
				time
				; user time value does not create equal duration audio chunks
				(new-time time dur check-labels)))))
				

;; ERROR CHECKING AND INITIALISE VARIABLES

(setq err ""); initialise error message
(setq label-list nil); initialize blank label track
(setq time (first (string-to-list time))); get Interval Label time from string
(setq start (first (string-to-list start))); get 'start' from string

; check 'start' is a valid number, then calculate and check 'dur(ation)' 
(if (not(numberp start)) ; check that it is a number
	(setq err (strcat err (format nil "'Time to place first label [seconds]' must be a number.~%~%")))
	(progn
		(setq start (max 0 start)) ;disallow negative label time
		(setf dur (- (get-duration 1) start)) ;calculate duration
		(if (<= dur 0)
			(setq err (strcat err (format nil 
"'Time to place first label' (~a seconds) is greater than selection length (~a seconds).~%~%"
				start (get-duration 1)))))))

; If using 'Label Interval'
(if (= placement 0)
	(if (not(numberp time)); check time is a number
		(setq err (strcat err (format nil "'Label interval [seconds]' must be a number.~%~%")))
		(if (<= time 0); Label interval must be positive
			(setq err (strcat err (format nil "'Label Interval [seconds]' must be a positive number.~%~%")))
			(if (and (> dur 0)(> time dur))
				(setq err (strcat err (format nil
"Including your time offset of ~a seconds, your requested~%label interval of ~a seconds is greater than the duration~%of your selected audio (~a seconds). ~%~%" 
					start time dur))))))
	;else using 'Number of labels'
	(if (<= label-number 0)
		(setq err (strcat err (format nil
"Based on 'Number of labels' you have selected ~a labels.~%~%" label-number)))))

;; MAIN PROGRAM

(if (> (length err) 0) ; if errors
	(format nil "ERROR.~%~%~a" err) ; print error message(s)
	(progn ; otherwise run program
		(if (= placement 1) ; number of labels
			(num-of-label-time)
			(user-selected-time))
	; add the labels
	(dotimes (i labels)
		(if (> labelnum 0)
			(add-label (+ start (* i time)) (add-number i text))
			(add-label (+ start (* i time)) text)))
	; return label track
	label-list))
