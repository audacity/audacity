;nyquist plug-in
;version 3
;type analyze
$name (_"Sample Data Export")
;manpage "Sample_Data_Export"
$action (_"Analyzing...")
;maxlen 1000001
;categories "http://lv2plug.in/ns/lv2core#AnalyserPlugin"
$author (_"Steve Daulton")
$copyright (_"Released under terms of the GNU General Public License version 2")

;; sample-data-export.ny by Steve Daulton June 2012.
;; Updated July 16 2012.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

$control number (_"Limit output to first") string (_"samples") "100" 
$control units (_"Measurement scale") choice ((_"dB") (_"Linear")) 0 
$control fileformat (_"File data format") choice (
   (_"Sample List (txt)")
   (_"Indexed List (txt)")
   (_"Time Indexed (txt)")
   (_"Data (csv)")
   (_"Web Page (html)")
) 0 
$control header (_"Include header information") choice (
   (_"None")
   (_"Minimal")
   (_"Standard")
   (_"All")
) 2 
$control optext (_"Optional header text") string ""  
$control chan (_"Channel layout for stereo") choice (
   (_"L-R on Same Line")
   (_"Alternate Lines")
   (_"L Channel First")
) 0   
$control messages (_"Show messages") choice (
   (_"Yes")
   (_"Errors Only")
   (_"None")
) 0 
$control filename (_"File name") string "" (_"sample-data")
$control path (_"Output folder") string "" (_"Home directory")
$control owrite (_"Allow files to be overwritten") choice ((_"No") (_"Yes")) 0 


;; To enable L/R prefix before alternate L/R channels 
;; (text output with header only)
;; remove the semicolon from the start of the next line:
;(setq LR-prefix '("L: " "R: "))

(when (not (boundp 'LR-prefix))(setq LR-prefix nil))

(setq default-filename (_"sample-data"))       ; default filename
(setq err "")                               ; initialise error mesaage

(setq *float-format* "%1.5f")               ; 5 decimal places
(when (equal (string-trim " .,\/" number) "")
      (setq number "100"))                  ; default=100


(defun add-error (e-string)
  (setq err (strcat err e-string "\n")))


;;; stereo peak
(defun stereomax (snd)
  (if (arrayp s)
      (max (peak (aref s 0) number)(peak (aref s 1) number))
      (peak s number)))


;;; stereo rms
(defun srms (snd)
  (if (arrayp snd)
      (let* ((sql (mult (aref s 0)(aref s 0)))
             (sqr (mult (aref s 1)(aref s 1)))
             (avgsq (mult 0.5 (sum sql sqr)))
             (avgsq (snd-avg avgsq number number op-average)))
        (lin-to-db (peak (snd-sqrt avgsq) 1)))
      (let* ((sndsq (mult snd snd))
             (avgsq (snd-avg sndsq number number op-average)))
        (lin-to-db (peak (snd-sqrt avgsq) 1)))))
      

;;; dc off-set mono
(defun dc-off-mon (sig len)
(let* ((total 0)
       (sig (snd-copy sig)))
  (dotimes (num (truncate len))
    (setq total (+ total (snd-fetch sig))))
  (/ total (float len))))


;;; compute dc offsets (mono/stereo)
(defun dc-off (sig)
  (if (arrayp sig)
      (let ((lin0 (dc-off-mon (aref sig 0) number))
            (lin1 (dc-off-mon (aref sig 1) number)))
        (list lin0 (lin-to-db (abs lin0)) lin1 (lin-to-db (abs lin1))))
      (let ((lin (dc-off-mon sig number)))
        (list lin (lin-to-db (abs lin))))))


(defun checknumber ()
  (setq number (min number len))
  (if (< number 1)
      (add-error (_"No samples selected.")))
  (if (> number 1000000)
      (add-error (_"Cannot export more than 1 million samples.")))
  (setq number (truncate number)))


;;; home directory
(defun home ()
  (if (windowsp)
      (get-env "UserProfile")               ; Windows
      (get-env "HOME")))                    ; Mac / Linux
      

;;; Check if Windows
(defun windowsp ()
  (char= #\\ *file-separator*))


;;; Windows safe linear-to-db
(setf ln10over20 (/ (log 10.0) 20))
(defun lin-to-db (val)
  (if (= val 0)
    ;i18n-hint abbreviates negative infinity
    (_"[-inf]")
    (/ (log val) ln10over20)))


;;; Check if Mac
(defun macp ()
  (string-equal (subseq (get-env "HOME") 0 6) "/Users"))


;;; check if file exists
(defun filep (fname ext &optional (fnum ""))
  (let ((fname (format nil "~a~a~a" fname fnum ext)))
    (if (open fname) T nil)))


(defun makefilename (fname ext)
  ;; avoid overwriting files
  (if (and (= owrite 0)(filep fname ext))
      (do ((num 1 (1+ num)))
          ((not (filep fname ext num))
           (format nil "~a~a~a" fname num ext)))
      (strcat fname ext)))


;;; get sample and convert to dB if required
(defun snd-get (snd &optional (dB 0))
  (if (= dB 0)                              ; dB scale
      (lin-to-db (abs (snd-fetch snd)))
      (snd-fetch snd)))                     ; linear scale


;; fileformat  0=Text List, 1=Indexed List, 2=Time Indexed, 3=CSV, 
;; (4=html but not used here).
;; Optional 'same' [line] argument is either 'true' or 'nil'
(defun formatprint (val snd &optional same)
  (case fileformat
    (0 (format fp "~a~a"                    ; text list
                  (snd-get snd units)
                  (if same "\t" "\n")))
    (1 (format fp "~a\t~a~a"                ; indexed list
                  val 
                  (snd-get snd units)
                  (if same "\t" "\n")))
    (2 (format fp "~a\t~a~a"                ; time index
                  (/ (1- val) *sound-srate*)
                  (snd-get snd units)
                  (if same "\t" "\n")))
    (3 (format fp "~a~a"                    ; csv
                  (snd-get snd units)
                  (if (or (= chan 2) same) "," "\n")))))
  

;;; Print sample data to file
(defun print-text (s-in)
  (do ((n 1 (1+ n)))
      ((> n number))
    (if (arrayp s-in)                       ; Stereo (alternate lines)
        (progn
          ;; option to prefix alternate lines with L/R 
          (when LR-prefix
            (unless (or (= header 0)(= fileformat 3))
              (format fp "~a" (first LR-prefix))))
          (if (= chan 0)                    ; IF 'Same Line' then "True"
            (formatprint n (aref s-in 0) T)
            (formatprint n (aref s-in 0)))
          (when LR-prefix
            (unless (or (= header 0)(= fileformat 3))
              (format fp "~a" (second LR-prefix))))
          (formatprint n (aref s-in 1)))
        (formatprint n s-in))))


;; Print to file
(defun printdata ()
  (case header
    (0 (format t (normhead))(format fp (nohead)))
    (1 (format t (normhead))(format fp (minhead)))
    (2 (format t (normhead))(format fp (normhead)))
    (3 (format t (normhead))(format fp (fullhead))))
  ;; Stereo and left channel first
  (if (and (arrayp s)(= chan 2))
      (progn
        (unless (= header 0)                ; Don't print 'channel' if no header
          (format fp (_"Left Channel.~%~%")))
        (print-text (aref s 0))
        (if (= header 0)                    ; Don't print 'channel' if no header
            (format fp "~%")
            (format fp (_"~%~%Right Channel.~%~%")))
        (print-text (aref s 1))
        (close fp)
        (if (= messages 0)
            (format nil (_"~aData written to:~%~a~a~a") 
                    (normhead) path fileseparator filename)
            nil))
      ;; mono or alternate
      (progn
        (print-text s)
        (close fp)
        (if (= messages 0)
            (format nil (_"~aData written to:~%~a~a~a")
                    (normhead) path fileseparator filename)
            nil))))


;;; File destination processing
(defun filewriter ()
  ;; Set file extension
  (setq FileExt
    (case fileformat
      (3 ".csv")
      (4 ".html")
      (T ".txt")))
  ; file separator as string
  (setq fileseparator (format nil "~a" *file-separator*))
  ;; strip file separator and spaces
  (let ((stuff (format nil " ~a" *file-separator*)))
    (setq filename (string-left-trim stuff filename))
    (setq path (string-right-trim stuff path)))
  ;; strip file extension if present
  (if (and (>= (length filename)(length FileExt))
           (string-equal filename FileExt :start1 (- (length filename)(length FileExt))))
    (setq filename (subseq filename 0  (- (length filename)(length FileExt)))))
  ;; replace ~/ on Linux/Max
  (if (and (>= (length path) 2)
           (not (windowsp)))
      (if (string-equal path "~/" :end1 2)
          (setq path (strcat (home)(subseq path 1)))))
  ;; If path not set use home directory
  (if (or (string-equal path (_"Home directory"))
          (string-equal path ""))
      (setq path (home)))
  ;; if file name not set use default
  (if (string-equal filename "")
      (setq filename default-filename))
  (setdir (strcat path fileseparator))      ; set target directory
  ;; set file pointer or error
  (let ((realdir (string-right-trim fileseparator (setdir "."))))
    (if (or (string= path realdir)
            (and  (or (windowsp)            ; case insensitive
                      (macp))               ; assume case insensitive
                  (string-equal path realdir)))
        ;; makefilename or error
        (setq filename (makefilename filename FileExt))
        (add-error (format nil (_"Output folder \"~a~a\" cannot be accessed.") 
                           path fileseparator))))
  ;; check if file is writeable
  (when (= (length err) 0)
    ;Open file for output
    (setq fp (open filename :direction :output))
    ;check file is writeable
    (if (not fp)
        (add-error (format nil (_"\"~a~a~a\" cannot be written.")
                           path fileseparator filename)))))


;;; Header text

(defun nohead ()
  (if (> (length optext) 0)
      (format nil "~a~%~a~%" 
              optext 
              (get 'info 'chan-order))
      ""))


(defun minhead ()
  (format nil 
(_"Sample Rate: ~a Hz.  Sample values on ~a scale.~%~a~%~a") 
  (get 'info 'srate)                        ; sample rate
  (get 'info 'units)                        ; units
  (get 'info 'chan-order)                   ; Channel Order
  (if (> (length optext) 0)
      (format nil "~a~%~%~%" optext)        ; optional text
      (format nil "~%"))))                  ; no optional text


(defun normhead ()
  (if (= fileformat 4)                      ; html
    (format nil
(_"~a   ~a~%~aSample Rate: ~a Hz.~%Length processed: ~a samples ~a seconds.~a") 
      filename                              ; file name
      (get 'info 'channels)                 ; mono/stereo
      (get 'info 'chan-order)               ; Channel Order
      (get 'info 'srate)                    ; sample rate
      number                                ; number of samples
      (get 'info 'duration)                 ; duration (seconds)
      (if (> (length optext)0)
          (format nil "~%~a~%~%~%" optext)  ; optional text
          (format nil "~%~%~%")))           ; no optional text
    (format nil
(_"~a   ~a~%~aSample Rate: ~a Hz. Sample values on ~a scale.~%~
Length processed: ~a samples ~a seconds.~a") 
      filename                              ; file name
      (get 'info 'channels)                 ; mono/stereo
      (get 'info 'chan-order)               ; Channel Order
      (get 'info 'srate)                    ; sample rate
      (get 'info 'units)                    ; units
      number                                ; number of samples
      (get 'info 'duration)                 ; duration (seconds)
      (if (> (length optext)0)
          (format nil "~%~a~%~%~%" optext)  ; optional text
          (format nil "~%~%~%")))))         ; no optional text


(defun fullhead ()
  (format nil
(_"~a~%Sample Rate: ~a Hz. Sample values on ~a scale. ~a.~%~aLength processed: ~a ~
samples, ~a seconds.~%Peak amplitude: ~a (lin) ~a dB.  Unweighted RMS: ~a dB.~%~
DC offset: ~a~a") 
  filename                                  ; file name
  (get 'info 'srate)                        ; sample rate
  (get 'info 'units)                        ; units
  (get 'info 'channels)                     ; mono/stereo
  (get 'info 'chan-order)                   ; Channel Order
  number                                    ; number of samples
  (get 'info 'duration)                     ; duration (seconds)
  (setq smax (stereomax s))                 ; peak amplitude linear
  (lin-to-db smax)                          ; peak amplitude dB
  (srms s)                                  ; rms
  (let ((vals (dc-off s)))                  ; DC offset
    (if (= (length vals) 2) ; mono
        (format nil (_"~a linear, ~a dB.") 
                (first vals)(second vals))
        (format nil (_"Left: ~a lin, ~a dB | Right: ~a lin, ~a dB.")
                (first vals)(second vals)(third vals)(fourth vals))))
  (if (> (length optext)0)
      (format nil "~%~a~%~%~%" optext)      ; optional text
      (format nil "~%~%~%"))))              ; no optional text


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        HTML Output         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun html-head () (strcat
"<!DOCTYPE html>
<html>
<head>
<meta name=\"generator\" content=
\"Sample Data Export by Steve Daulton, (http://www.easyspacepro.com). Released under GPL v2.\">
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
<title>" (_"Sample Data Export") "</title>
</head>
"))


;;; document headings
(defun doc-head ()
  (format nil
(strcat "<body>
<h1>" (_"Sample Data Export") " - ~a</h1>
~a
<h4>~a. &nbsp;&nbsp;" (_"~a samples.") " &nbsp;&nbsp; " (_"~a seconds.") "<br></h4>
<h3>" (_"Audio data analysis:") "</h3>
<ul>
<li>" (_"<b>Sample Rate:</b> &nbsp;&nbsp;~a Hz.") "</li>"
; i18n-hint: abbreviates "linear" and "decibels"
"<li>" (_"<b>Peak Amplitude:</b> &nbsp;&nbsp;~a (lin) &nbsp;&nbsp;~a dB.") "</li>"
; i18n-hint: RMS abbreviates root-mean-square, a method of averaging a signal; there also "weighted" versions of it but this isn't that
"<li>" (_"<b>RMS</b> (unweighted): &nbsp;&nbsp;~a dB.") "</li>"
; i18n-hint: DC derives from "direct current" in electronics, really means the zero frequency component of a signal
"<li>" (_"<b>DC Offset:</b> &nbsp;&nbsp;~a") "</li>
</ul>
") ; end concatenated format string with inserted translations
  (string-right-trim ".html" filename)
  (format nil "<h2>~a</h2>" optext)         ; Optional heading
  (get 'info 'channels)                     ; mono/stereo
  number                                    ; number of samples
  (get 'info 'duration)                     ; duration (seconds)
  (get 'info 'srate)                        ; sample rate
  (setq smax (stereomax s))                 ; peak amplitude linear
  (lin-to-db smax)                          ; peak amplitude dB
  (srms s)                                  ; rms
  (let ((vals (dc-off s)))                  ; DC offset
    (if (= (length vals) 2) ; mono
        (format nil (_"~a linear, &nbsp;&nbsp;~a dB.") 
                (first vals)(second vals))
        (format nil (_"Left: ~a lin, ~a dB | Right: ~a linear, &nbsp;&nbsp;~a dB.")
                (first vals)(second vals)(third vals)(fourth vals))))))


;;; table headings  (mono)
(defun table-head-mono ()
(strcat "<table title=\"" (_"sample data") "\">
<tr>
<th>" (_"Sample #") "</th>
<th>" (_"Seconds") "</th>
<th>" (_"Value (linear)") "</th>
<th>" (_"Value (dB)") "</th>
</tr>"))


;;; table headings (stereo)
(defun table-head-stereo ()
(strcat "<table title=\"" (_"audio sample value analysis") "\">
<tr>
<th>" (_"Sample #") "</th>
<th>" (_"Seconds") "</th>
<th>" (_"Left (linear)") "</th>
<th>" (_"Right (linear)") "</th>
<th>" (_"Left (dB)") "</th>
<th>" (_"Right (dB)") "</th>
</tr>"))


(defun html-foot ()
  (format nil (strcat
"</table>
<p id=\"footer\">" (_"Produced with <span>Sample Data Export</span> for
<a href=\"~a\">Audacity</a> by Steve
Daulton") " (<a href=
\"http://www.easyspacepro.com\">www.easyspacepro.com</a>)</p>
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
  (format t (normhead))
  (format fp (html-head))
  (format fp (doc-head))
  (if (arrayp s)
      (progn
        (format fp (table-head-stereo))
        (do ((i 1 (1+ i)))
            ((> i number))
          (make-htm i
                    (snd-fetch (aref s 0))
                    (snd-fetch (aref s 1)))))
      (progn
        (format fp (table-head-mono))
        (do ((i 1 (1+ i)))
            ((> i number))
          (make-htm i (snd-fetch s)))))
  (format fp (html-foot))
  (close fp)
    (if (= messages 0)
        (format nil (_"~aData written to:~%~a~a~a")
               (normhead) path fileseparator filename)
        nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       END OF HTML          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; basic info for headers
(defun put-head-info ()
  (putprop 'info (truncate *sound-srate*) 'srate)
  (putprop 'info (if (= units 0) (_"dB") (_"linear")) 'units)
  (putprop 'info (/ number *sound-srate*) 'duration)
  (putprop 'info 
    (if (arrayp s) 
        (_"2 channels (stereo)") (_"1 channel (mono)"))
        'channels)
  ;; stereo sample order
  (putprop 'info
    (cond
      ((and (= fileformat 3)(= chan 0))     ; csv, channel in column
        (format nil (_"One column per channel.~%")))
      ((and (= fileformat 3)(= chan 2))     ; csv, channel in row
        (format nil (_"One row per channel.~%")))
      ((or (soundp s)(= fileformat 4))      ; mono soundor HTML
        "")
      ((= chan 0) (format nil (_"Left channel then Right channel on same line.~%")))
      ((= chan 1) (format nil (_"Left and right channels on alternate lines.~%")))
      ((= chan 2) (format nil (_"Left channel first then right channel.~%")))
      (T (_"Unspecified channel order")))
    'chan-order))


;;; get number from string
(setq number (read (make-string-input-stream number)))
(if (numberp number)
    (checknumber)
    (add-error (format nil (_"~a is not a number.") number)))

(filewriter)
(if (> (length err) 0)
  ;; output error message if enabled
  (if (= messages 2)
    nil                                     ; return nil
    (format nil (_"Error.~%~a") err))          ; return errors
  ;; else print to file
  (progn
    (put-head-info)                         ; put basic info for headers
    (if (= fileformat 4)
        (printhtml)                         ; html output
        (printdata))))                      ; text output
