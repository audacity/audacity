$nyquist plug-in
$version 4
$type tool analyze
$name (_ "Sample Data Export")
$debugbutton false
$author (_ "Steve Daulton")
$release 3.0.4-2
$copyright (_ "GNU General Public License v2.0 or later")


;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


;; To enable L/R prefix before alternate L/R channels
;; (text output with header only)
;; remove the semicolon from the start of the next line:
;(setq LR-PREFIX '("L: " "R: "))

$control NUMBER (_ "Limit output to first") int-text (_ "samples") 100 1 1000000
$control UNITS (_ "Measurement scale") choice ((_ "dB") (_ "Linear")) 0
$control FILENAME (_ "Export data to") file (_ "Select a file") "*default*/sample-data.txt" (((_ "Text file") (txt TXT))
                           ((_ "CSV files") (csv CSV))
                           ((_ "HTML files") (html HTML htm HTM))
                           ((_ "All files") (""))) "save,overwrite"
$control FILEFORMAT (_ "Index (text files only)") choice ((_ "None")
                                                          ("Count" (_ "Sample Count"))
                                                          ("Time" (_ "Time Indexed")))
$control HEADER (_ "Include header information") choice ((_ "None")
                                                         (_ "Minimal")
                                                         (_ "Standard")
                                                         (_ "All")) 0
$control OPTEXT (_ "Optional header text") string "" ""
$control CHANNEL-LAYOUT (_ "Channel layout for stereo") choice (;i18n-hint: Left and Right
                                                                ("SameLine" (_ "L-R on Same Line"))
                                                                ("Alternate" (_ "Alternate Lines"))
                                                                ;i18n-hint: L for Left
                                                                ("LFirst" (_ "L Channel First"))) 0
$control MESSAGES (_ "Show messages") choice ((_ "Yes")
                                              ("Errors" (_ "Errors Only"))
                                              (_ "None")) 0


;; Global constants
(setf NUMBER (min (truncate len) NUMBER))
(when (not (boundp 'LR-PREFIX))(setq LR-PREFIX nil))
(setq *float-format* "%1.5f") ; 5 decimal places


;;; Return file extension or empty string
(defun get-extension (fname)
  (let ((n (1- (length fname)))
        (ext ""))
    (do ((i n (1- i)))
        ((= i 0) ext)
      (when (char= (char fname i) #\.)
        (setf ext (subseq fname (1+ i)))
        (return ext)))))


;;; stereo peak
(defun stereomax (snd)
  (if (arrayp *track*)
      (max (peak (aref *track* 0) NUMBER)
           (peak (aref *track* 1) NUMBER))
      (peak *track* NUMBER)))


;;; stereo rms
(defun srms (snd)
  (if (arrayp snd)
      (let* ((sql (mult (aref *track* 0)(aref *track* 0)))
             (sqr (mult (aref *track* 1)(aref *track* 1)))
             (avgsq (mult 0.5 (sum sql sqr)))
             (avgsq (snd-avg avgsq NUMBER NUMBER op-average)))
        (lin-to-db (peak (snd-sqrt avgsq) 1)))
      (let* ((sndsq (mult snd snd))
             (avgsq (snd-avg sndsq NUMBER NUMBER op-average)))
        (lin-to-db (peak (snd-sqrt avgsq) 1)))))


;;; DC off-set mono
(defun dc-off-mon (sig len)
  (let* ((total 0)
         (sig (snd-copy sig))
         (ln (truncate len)))
    (dotimes (num ln)
      (setq total (+ total (snd-fetch sig))))
    (/ total (float len))))


;;; DC offset (mono/stereo)
(defun dc-off (sig)
  (if (arrayp sig)
      (let ((lin0 (dc-off-mon (aref sig 0) NUMBER))
            (lin1 (dc-off-mon (aref sig 1) NUMBER)))
        (list lin0 (lin-to-db (abs lin0)) lin1 (lin-to-db (abs lin1))))
      (let ((lin (dc-off-mon sig NUMBER)))
        (list lin (lin-to-db (abs lin))))))


;;; Platform independent representation of negative infinity
(defun lin-to-db (val)
  (if (= val 0)
    ;i18n-hint abbreviates negative infinity
    (_ "[-inf]")
    (linear-to-db val)))


;;; Get sample and convert to dB if required
(defun snd-get (snd &optional (dB 0))
  (if (= dB 0)                              ; dB scale
      (lin-to-db (abs (snd-fetch snd)))
      (snd-fetch snd)))                     ; linear scale


;; FILEFORMAT  0=Text List, 1=Sample count index, 2=Time index, 3=CSV,
;; (4=html but not used here).
;; Optional 'same' [line] argument is either 'true' or 'nil'
(defun formatprint (val snd &optional same)
  (case FILEFORMAT
    (0 (format fp "~a~a"                    ; plain list
                  (snd-get snd UNITS)
                  (if same "\t" "\n")))
    (1 (format fp "~a\t~a~a"                ; count index
                  val
                  (snd-get snd UNITS)
                  (if same "\t" "\n")))
    (2 (format fp "~a\t~a~a"                ; time index
                  (/ (1- val) *sound-srate*)
                  (snd-get snd UNITS)
                  (if same "\t" "\n")))
    (3 (format fp "~a~a"                    ; csv
                  (snd-get snd UNITS)
                  (if (or (= CHANNEL-LAYOUT 2) same) "," "\n")))))


;;; Print sample data to file
(defun print-text (sig)
  (do ((n 1 (1+ n)))
      ((> n NUMBER))
    (if (arrayp sig)  ; Stereo (alternate lines)
        (progn
          ;; option to prefix alternate lines with L/R
          (when LR-PREFIX
            (unless (or (= HEADER 0)(= FILEFORMAT 3))
              (format fp "~a" (first LR-PREFIX))))
          (if (= CHANNEL-LAYOUT 0)  ; IF 'Same Line' then "True"
            (formatprint n (aref sig 0) T)
            (formatprint n (aref sig 0)))
          (when LR-PREFIX
            (unless (or (= HEADER 0)(= FILEFORMAT 3))
              (format fp "~a" (second LR-PREFIX))))
          (formatprint n (aref sig 1)))
        (formatprint n sig))))


;; Print to file
(defun printdata ()
  (case HEADER
    (0 (format t (normhead))(format fp (nohead)))
    (1 (format t (normhead))(format fp (minhead)))
    (2 (format t (normhead))(format fp (normhead)))
    (3 (format t (normhead))(format fp (fullhead))))
  (if (and (arrayp *track*)(= CHANNEL-LAYOUT 2))
      ;; Stereo and left channel first
      (progn
        (unless (= HEADER 0)                ; Don't print 'channel' if no header
          (format fp (_ "Left Channel.~%~%")))
        (print-text (aref *track* 0))
        (if (= HEADER 0)                    ; Don't print 'channel' if no header
            (format fp "~%")
            (format fp (_ "~%~%Right Channel.~%~%")))
        (print-text (aref *track* 1)))
      ;; mono or alternate
      (print-text *track*))
  (close fp)
  (if (= MESSAGES 0)
      (format nil (_ "~aData written to:~%~a") (normhead) FILENAME)
      (progn
        (format t (_ "~aData written to:~%~a") (normhead) FILENAME)
        "")))


;;; Header text

(defun nohead ()
  (if (> (length OPTEXT) 0)
      (format nil "~a~%~a~%"
              OPTEXT
              (get 'info 'chan-order))
      ""))


(defun minhead ()
  (format nil (_ "Sample Rate: ~a Hz.  Sample values on ~a scale.~%~a~%~a")
  (get 'info 'srate)                        ; sample rate
  (get 'info 'units)                        ; units
  (get 'info 'chan-order)                   ; Channel Order
  (if (> (length OPTEXT) 0)
      (format nil "~a~%~%~%" OPTEXT)        ; optional text
      (format nil "~%"))))                  ; no optional text


(defun normhead ()
  (if (= FILEFORMAT 4)  ; html
      (format nil (_ "~a   ~a~%~aSample Rate: ~a Hz.~%Length processed: ~a samples ~a seconds.~a")
              FILENAME                              ; file name
              (get 'info 'channels)                 ; mono/stereo
              (get 'info 'chan-order)               ; Channel Order
              (get 'info 'srate)                    ; sample rate
              NUMBER                                ; number of samples
              (get 'info 'duration)                 ; duration (seconds)
              (if (> (length OPTEXT)0)
                  (format nil "~%~a~%~%" OPTEXT)    ; optional text
                  (format nil "~%~%")))             ; no optional text
      (format nil (_ "~a   ~a~%~aSample Rate: ~a Hz. Sample values on ~a scale.~%~
                     Length processed: ~a samples ~a seconds.~a")
              FILENAME                              ; file name
              (get 'info 'channels)                 ; mono/stereo
              (get 'info 'chan-order)               ; Channel Order
              (get 'info 'srate)                    ; sample rate
              (get 'info 'units)                    ; units
              NUMBER                                ; number of samples
              (get 'info 'duration)                 ; duration (seconds)
              (if (> (length OPTEXT)0)
                  (format nil "~%~a~%~%" OPTEXT)    ; optional text
                  (format nil "~%~%")))))           ; no optional text


(defun fullhead ()
  (format nil (_ "~a~%Sample Rate: ~a Hz. Sample values on ~a scale. ~a.~%~aLength processed: ~a ~
                  samples, ~a seconds.~%Peak amplitude: ~a (linear) ~a dB.  Unweighted RMS: ~a dB.~%~
                  DC offset: ~a~a")
  FILENAME                                  ; file name
  (get 'info 'srate)                        ; sample rate
  (get 'info 'units)                        ; units
  (get 'info 'channels)                     ; mono/stereo
  (get 'info 'chan-order)                   ; Channel Order
  NUMBER                                    ; number of samples
  (get 'info 'duration)                     ; duration (seconds)
  (setq smax (stereomax *track*))           ; peak amplitude linear
  (lin-to-db smax)                          ; peak amplitude dB
  (srms *track*)                            ; rms
  (let ((vals (dc-off *track*)))            ; DC offset
    (if (= (length vals) 2) ; mono
        (format nil (_ "~a linear, ~a dB.")
                (first vals) (second vals))
        (format nil (_ "Left: ~a lin, ~a dB | Right: ~a lin, ~a dB.")
                (first vals) (second vals) (third vals) (fourth vals))))
  (if (> (length OPTEXT)0)
      (format nil "~%~a~%~%~%" OPTEXT)      ; optional text
      (format nil "~%~%~%"))))              ; no optional text


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        HTML Output         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun html-head () (strcat
"<!DOCTYPE html>
<html>
<head>
<meta name=\"generator\" content=
\"Sample Data Export by Steve Daulton, (https://www.audionyq.com). Released under GPL v2.0+\">
<meta name=\"description\" content=\"Sample Printer, Free Audacity plug-in\" />
<meta name=\"keywords\" content=\"sample printer,Audacity,plug-ins,plugins,effects,audio,audio processing,music,analyze\" />
<meta name=\"author\" content=\"Steve Daulton\" />
<meta charset=\"UTF-8\">

<style type=\"text/css\">
body {
  margin: 1em 5%;
  background-color: #dda;
  font-family:Arial,Helvetica,sans-serif;
  }
table,th,td {
  background-color: #fff;
  border:1px solid black;
  text-align: center;
}
table {
  width: auto;
  border: 2px;
  border-style:ridge;
  border-collapse:collapse;
}
td {
  text-align: right;
  padding-right: 0.5em;
}
tr:hover td {
  background-color:#fcd;
}
th {
  padding: 0 0.5em;
  background-color: #ddf;
  border-bottom-width: 2px;
  border-bottom-style:ridge;
}
h1 {
  font-size: 1.6em;
  color: #633;
}
h2 {
  font-size: 1.4em;
  color: #633;
}
h3 {
  font-size: 1em;
  color: #366;
}
h4 {
  font-size: 1em;
  color: #000;
}
ul {
  position:relative;
  top: -0.5em;
  }
#footer {
  font-size: 0.8em;
  position:relative;
  top: 0.5em;
  left: 2%;
  }
#footer span {
  font-style:italic;
  font-weight: bold;
  color: #633;
  }
#footer a:link,a:visited {
  color: #639;
  text-decoration: none;
  }
#footer a:hover,a:active {
  text-decoration: underline;
  color: blue;
  }
</style>
<title>" (_ "Sample Data Export") "</title>
</head>
"))


;;; document headings
(defun doc-head ()
  (format nil
(strcat "<body>
<h1>" (_ "Sample Data Export") " - ~a</h1>
~a
<h4>~a. &nbsp;&nbsp;" (_ "~a samples.") " &nbsp;&nbsp; " (_ "~a seconds.") "<br></h4>
<h3>" (_ "Audio data analysis:") "</h3>
<ul>
<li>" (_ "<b>Sample Rate:</b> &nbsp;&nbsp;~a Hz.") "</li>"
; i18n-hint: abbreviates "decibels"
"<li>" (_ "<b>Peak Amplitude:</b> &nbsp;&nbsp;~a (linear) &nbsp;&nbsp;~a dB.") "</li>"
; i18n-hint: RMS abbreviates root-mean-square, a method of averaging a signal; there also "weighted" versions of it but this isn't that
"<li>" (_ "<b>RMS</b> (unweighted): &nbsp;&nbsp;~a dB.") "</li>"
; i18n-hint: DC derives from "direct current" in electronics, really means the zero frequency component of a signal
"<li>" (_ "<b>DC Offset:</b> &nbsp;&nbsp;~a") "</li>
</ul>
") ; end concatenated format string with inserted translations
  (string-right-trim ".html" FILENAME)
  (format nil "<h2>~a</h2>" OPTEXT)         ; Optional heading
  (get 'info 'channels)                     ; mono/stereo
  NUMBER                                    ; number of samples
  (get 'info 'duration)                     ; duration (seconds)
  (get 'info 'srate)                        ; sample rate
  (setq smax (stereomax *track*))                 ; peak amplitude linear
  (lin-to-db smax)                          ; peak amplitude dB
  (srms *track*)                            ; rms
  (let ((vals (dc-off *track*)))            ; DC offset
    (if (= (length vals) 2) ; mono
        (format nil (_ "~a linear, &nbsp;&nbsp;~a dB.")
                (first vals)(second vals))
        (format nil (_ "Left: ~a lin, ~a dB | Right: ~a linear, &nbsp;&nbsp;~a dB.")
                (first vals)(second vals)(third vals)(fourth vals))))))


;;; table headings  (mono)
(defun table-head-mono ()
(strcat "<table title=\"" (_ "sample data") "\">
<tr>
<th>" (_ "Sample #") "</th>
<th>" (_ "Seconds") "</th>
<th>" (_ "Value (linear)") "</th>
<th>" (_ "Value (dB)") "</th>
</tr>"))


;;; table headings (stereo)
(defun table-head-stereo ()
(strcat "<table title=\"" (_ "audio sample value analysis") "\">
<tr>
<th>" (_ "Sample #") "</th>
<th>" (_ "Seconds") "</th>
<th>" (_ "Left (linear)") "</th>
<th>" (_ "Right (linear)") "</th>
<th>" (_ "Left (dB)") "</th>
<th>" (_ "Right (dB)") "</th>
</tr>"))


(defun html-foot ()
  (format nil (strcat
"</table>
<p id=\"footer\">" (_ "Produced with <span>Sample Data Export</span> for
<a href=\"~a\">Audacity</a> by Steve
Daulton") " (<a href=
\"https://audionyq.com\">audionyq.com</a>)</p>
</body>
</html>") "https://www.audacityteam.org/"))


;;; html generator
(defun make-htm (id val1 &optional val2)
  (if val2
      ;; stereo
      (let ((time (/ (1- id) *sound-srate*))
            (db1 (lin-to-db (abs val1)))
            (db2 (lin-to-db (abs val2))))
        (format fp
          "<tr>~%<td>~a</td>~%<td>~a</td>~%<td>~a</td>~%<td>~a</td>~%~
          <td>~a</td>~%<td>~a</td>~%</tr>~%"
          id time val1 val2 db1 db2))
      ;; mono
      (let ((time (/ (1- id) *sound-srate*))
            (db (lin-to-db (abs val1))))
        (format fp
          "<tr>~%<td>~a</td>~%<td>~a</td>~%<td>~a</td>~%<td>~a</td>~%</tr>~%"
          id time val1 db))))


(defun printhtml ()
  (format fp (html-head))
  (format fp (doc-head))
  (if (arrayp *track*)
      (progn
        (format fp (table-head-stereo))
        (do ((i 1 (1+ i)))
            ((> i NUMBER))
          (make-htm i
                    (snd-fetch (aref *track* 0))
                    (snd-fetch (aref *track* 1)))))
      (progn
        (format fp (table-head-mono))
        (do ((i 1 (1+ i)))
            ((> i NUMBER))
          (make-htm i (snd-fetch *track*)))))
  (format fp (html-foot))
  (close fp)
    (if (= MESSAGES 0)
        (format nil (_ "~aData written to:~%~a") (normhead) FILENAME)
        (progn
          (format t (_ "~aData written to:~%~a") (normhead) FILENAME)
          "")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       END OF HTML          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; basic info for headers
(defun put-head-info ()
  (putprop 'info (truncate *sound-srate*) 'srate)
  (putprop 'info (if (= UNITS 0) (_ "dB") (_ "linear")) 'units)
  (putprop 'info (/ NUMBER *sound-srate*) 'duration)
  (putprop 'info
    (if (arrayp *track*)
        (_ "2 channels (stereo)") (_ "1 channel (mono)"))
        'channels)
  ;; stereo sample order
  (putprop 'info
    (cond
      ((and (= FILEFORMAT 3)(= CHANNEL-LAYOUT 0))     ; csv, channel in column
        (format nil (_ "One column per channel.~%")))
      ((and (= FILEFORMAT 3)(= CHANNEL-LAYOUT 2))     ; csv, channel in row
        (format nil (_ "One row per channel.~%")))
      ((or (soundp *track*)(= FILEFORMAT 4))          ; mono sound or HTML
        "")
      ((= CHANNEL-LAYOUT 0) (format nil (_ "Left channel then Right channel on same line.~%")))
      ((= CHANNEL-LAYOUT 1) (format nil (_ "Left and right channels on alternate lines.~%")))
      ((= CHANNEL-LAYOUT 2) (format nil (_ "Left channel first then right channel.~%")))
      (T (_ "Unspecified channel order")))
    'chan-order))


;;; Specifying a CSV or HTML file overrides the (text only) format selection.
(let ((file-extension (get-extension FILENAME)))
  (cond
    ((string-equal file-extension "csv")
        (setf FILEFORMAT 3))
    ((string-equal file-extension "html")
        (setf FILEFORMAT 4))
    ((string-equal file-extension "htm")
        (setf FILEFORMAT 4))))


(setq fp (open FILENAME :direction :output))
(cond
  (fp (put-head-info)
      (if (= FILEFORMAT 4)
          (printhtml)       ; html output
          (printdata)))     ; text output
  (t  (if (= MESSAGES 2)
          (format t (_ "Error.~%\"~a\" cannot be written.") FILENAME)
          (format nil (_ "Error.~%\"~a\" cannot be written.") FILENAME))))
