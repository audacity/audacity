;nyquist plug-in
;version 3
;type analyze
;categories "http://audacityteam.org/namespace#TimeAnalyser"
;name "Regular Interval Labels..."
;action "Adding equally-spaced labels to the label track..."
;info "equalabel.ny by David R. Sky www.shellworld.net/~davidsky/ \nReleased under terms of the GNU General Public License version 2\nCreate equally spaced labels by choosing the number of labels or the interval\nbetween them. Making your final audio segment equal with others may slightly\nchange the label interval that you had set.\nNote: Equalabel.ny does not overwrite an existing label track, but adds to it.\nCode for label placement based on silencemarker.ny by Alex S.Brown."

;control start "Time to place first label [seconds]" string " " "0.0"
;control placement "Label placement method" choice "Label interval,Number of labels" 0
;control time "Set either: Label interval [seconds]" string " " "60.0"
;control label-number "Or: Number of labels" int "" 10 2 100
;control text "Label text" string "" "Label"
;control prepend "Prepend numbers to label text?" choice "No - just use text,Yes" 1
;control include "Include final label?" choice "No,Yes" 0
;control t-choice "Final audio segment equal with others?" choice "No,Yes" 1

; Regular interval labels by David R. Sky, June-October 2007.
; Released under terms of the GNU General Public License version 2
; http://www.gnu.org/copyleft/gpl.html
; Thanks Sami Jumppanen for plug-in suggestion.
; Thanks Alex S. Brown for code showing how to place labels.
; Thanks Dominic Mazzoni, Pierre M.I., Gale Andrews 
; for improvement suggestions.


; function to convert string to list
(defun string-to-list (str)
(read (make-string-input-stream (format nil "(~a)" str))))


; convert start string to a number
(setf start (first (string-to-list start)))

; convert time string to a number
(setf time (first (string-to-list time)))

; get selection duration in seconds,
; then subtract start offset
(setf dur (- (/ len *sound-srate*) start))

; give an error message 
; if user-set label interval is greater than selection duration,
; taking into accound start time offset
(cond ; outermost cond
((and (> time dur) (= placement 0))
(format nil
"Including your time offset of ~a seconds, your requested~%label interval of ~a seconds is greater than the duration~%of your selected audio (~a seconds). ~%
Please run this plug-in again using a smaller label interval. ~%" 
start time dur))

(t ; label interval time is equal to or less than selection duration

; choose between user-selected label interval or number of labels
(cond 
((= placement 1) ; number of labels
(setf time (if (= include 0)
(float (/ dur label-number)) (1+ (float (/ dur label-number)))))
(setf labels (if (= include 0) label-number (1+ label-number)))
) ; end placement 1

(t ; user-selected time interval
; calculate number of labels in selection
; this includes label at start of selection
; which is label number 0 if numbers are prepended to label text
; if final label is unwanted, decrement number of labels by 1
(setf labels  (if (= include 0)
(truncate (/ dur time))
(+ 1 (truncate (/ dur time)))
) ; end if
) ; end setf labels

; setf check-labels: number of labels if user includes last label
; this is for checking purposes in the [setf time ...] section of code
(setf check-labels (+ 1 (truncate (/ dur time))))


; function to calculate new time value 
; if user value does not create equal duration final audio chunk
; returns new time value
(defun new-time (time dur check-labels)
; calculate duration of last audio chunk
(setf last-label (- dur (* time check-labels)))
(if (< last-label (* time 0.5)) ; last chunk is less than 1/2 time
(/ dur (- check-labels 1))
(/ dur check-labels)
) ; end if
) ; end defun new-time


(setf time ; depending whether user wants specified time used 
; or wants equal duration audio chunks including last chunk
; user time may create equal audio chunks even for last chunk
(cond ; 1
((= t-choice 0) time) ; use user time value...

(t ; ...otherwise calculate time for equal duration audio chunks
(cond ; 2 
; if user time value creates equal final audio segment duration anyway 
; then use user interval
((= dur (* time (- check-labels 1))) time)

; user time value does not create equal duration audio chunks
(t 
(new-time time dur check-labels)
) ; end t
) ; end cond 2
) ; end of calculation for equal duration audio chunks
) ; end cond1
) ; end setf time

) ; end t
) ; end cond choosing between user-set label interval or number of labels


; function to add labels to the label track
; from silencemarker.ny by Alex S. Brown
(defun add-label (l-time l-text)
 (setq l (cons (list l-time l-text) l)))


; function to prepend label number before label text
(defun prepend-number (i text)
(format nil "~a~a" i text))


; initialize blank label track
(setq l nil)

; add the labels
(dotimes (i labels)
(if (= prepend 1) ; prepend numbers to label text?
(add-label (+ start (* i time)) (prepend-number i text)) ; yes
(add-label (+ start (* i time)) text) ; no
) ; end if
) ; end dotimes i

; return label track
l

) ; close t of outermost cond
) ; end outermost cond
