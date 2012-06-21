;nyquist plug-in
;version 3
;type analyze
;name "Sample Data Export..."
;action "Processing..."
;categories "http://lv2plug.in/ns/lv2core#AnalyserPlugin"
;info "by Steve Daulton. Released under GPL v2" 

;; sample-data-export.ny by Steve Daulton June 2012.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html


;control help "Show Help File" choice "No,Overview,File Format,Header Text,Output Files,Save Help File" 0 
;control number "Limit output to first" string "samples" "100" 
;control units "Measurement scale" choice "dB,Linear" 0 
;control fileformat "File data format" choice "Sample List (txt),Indexed List (txt),Time Indexed (txt),Data (csv),Web Page (html)" 0 
;control header "Include header information" choice "None,Minimal,Standard,All" 2 
;control optext "Optional header text" string ""  
;control chan "Channel layout for stereo" choice "Alternate Lines,L Channel First" 1   
;control messages "Show messages" choice "Yes,Errors Only,None" 0 
;control filename "File name" string "" "sample-data"
;control path "Output folder" string "" "Home directory"
;control owrite "Allow files to be overwritten" choice "No,Yes" 0 


;; To enable L/R prefix before alternate L/R channels 
;; (text output with header only)
;; remove the semicolon from the start of the next line:
;(setq LR-prefix 1)

(when (not (boundp 'LR-prefix))(setq LR-prefix nil))

(setq default-filename "sample-data") ; default filename
(setq err "")                         ; initialise error mesaage

(setq *float-format* "%1.5f")         ; 5 decimal places
(when (equal (string-trim " .,\/" number) "")
      (setq number "100"))            ; default=100


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
      (linear-to-db (peak (snd-sqrt avgsq) 1)))
    (let* ((sndsq (mult snd snd))
           (avgsq (snd-avg sndsq number number op-average)))
      (linear-to-db (peak (snd-sqrt avgsq) 1)))))
      

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
      (list lin0 (linear-to-db (abs lin0)) lin1 (linear-to-db (abs lin1))))
    (let ((lin (dc-off-mon sig number)))
      (list lin (linear-to-db (abs lin))))))


(defun checknumber ()
  (setq number (min number len))
  (if (< number 1)
    (add-error "No samples selected."))
  (if (> number 1000000)
    (add-error "Too many samples selected.\nSet limit to less than 1 million"))
  (setq number (truncate number)))


;;; home directory
(defun home ()
  (if (windowsp)
    (get-env "UserProfile")             ; Windows
    (get-env "HOME")))                  ; Mac / Linux
      


;;; Check if Windows
(defun windowsp ()
  (char= #\\ *file-separator*))


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
  (if (= dB 0)                          ; dB scale
    (linear-to-db (abs (snd-fetch snd)))
    (snd-fetch snd)))                   ; linear scale


;; fileformat  0=Text List, 1=Indexed List, 2=Time Indexed, 3=CSV, (4=html but not used here)
(defun formatprint (val snd)
  (case fileformat
    (0 (format fp "~a~%"                ; text list
               (snd-get snd units)))
    (1 (format fp "~a\t~a~%"            ; indexed list
               val (snd-get snd units)))
    (2 (format fp "~a\t~a~%"            ; time index
               (/ (1- val) *sound-srate*)
               (snd-get snd units)))
    (3 (format fp "~a,"                 ; csv
               (snd-get snd units)))))
  

;;; Print sample data to file
(defun print-text (s-in)
  (do ((n 1 (1+ n)))
      ((> n number))
    (if (arrayp s-in)                 ; Stereo (alternate lines)
      (progn
        ;; option to prefix alternate lines with L/R 
        (when LR-prefix
          (unless (or (= header 0)(= fileformat 3))
            (format fp "L  ")))
        (formatprint n (aref s-in 0))
        ;; option to prefix alternate lines with L/R
        (when LR-prefix
          (unless (or (= header 0)(= fileformat 3))
            (format fp "R  ")))
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
  (if (and (arrayp s)(= chan 1))
    (progn
      (unless (= header 0)            ; Don't print 'channel' if no header
        (format fp "Left Channel.~%~%"))
      (print-text (aref s 0))
      (if (= header 0)                ; Don't print 'channel' if no header
        (format fp "~%")
        (format fp "~%~%Right Channel.~%~%"))
      (print-text (aref s 1))
      (close fp)
      (if (= messages 0)
        (format nil "~aData written to:~%~a~a~a" 
                (normhead) path fileseparator filename)
        (s-rest 0)))
    ;; mono or alternate
    (progn
      (print-text s)
      (close fp)
      (if (= messages 0)
        (format nil "~aData written to:~%~a~a~a"
               (normhead) path fileseparator filename)
        (s-rest 0)))))


;;; File destination processing
(defun filewriter ()
  ;; Set file extension
  (setq FileExt
    (case fileformat
      (3 ".csv")
      (4 ".html")
      (T ".txt")))

  ;; file separator as string
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
  (if (and (>= (length path) 2)(not (windowsp)))
    (if (string-equal path "~/" :end1 2)
      (setq path (strcat (home)(subseq path 1)))))

  ;; If path not set use home directory
  (if (or (string-equal path "Home directory")
          (string-equal path ""))
      (setq path (home)))

  ;; if file name not set use default
  (if (string-equal filename "")
    (setq filename default-filename))

  (setdir (strcat path fileseparator))    ; set target directory

  ;; set file pointer or error
  (let ((realdir (string-right-trim fileseparator (setdir "."))))
    (if (or
            (and  (or (windowsp)          ; case insensitive
                      (macp))             ; assume case insensitive
                  (string-equal path realdir))
            (string= path realdir))
      ;; makefilename or error
      (setq filename (makefilename filename FileExt))
      (add-error (format nil "Output folder \"~a~a\" cannot be accessed." 
                         path fileseparator))))
  ;; check if file is writeable
  (when (= (length err) 0)
    ;Open file for output
    (setq fp (open filename :direction :output))
    ;check file is writeable
    (if (not fp)
      (add-error (format nil "\"~a~a~a\" cannot be written."
                         path fileseparator filename)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       HELP FILES           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun help-page (page)
  (case page
    (1 
"OVERVIEW.
Sample Data Export reads the values of successive
samples from the selected audio and prints to a 
file. Additional information may be added as a 
'header' at the top of the page.\n
HELP SCREENS:
Select only one track before viewing to avoid 
repeated help screens. To run the plug-in set the
help option to \"No\".
Select \"Save Help File\" to write all help screens 
to a printable file.\n
LIMIT OUTPUT TO FIRST (maximum number of samples):
Enter a number to limit the number of samples 
processed from the selection. The maximum number
of samples is 1 million, but files this large may 
be hard to open. The track sample rate indicates 
the number of samples per second.\n
LINEAR/dB SCALE:
Sample values may be displayed on a linear scale
+/- 1 (as in the Audacity audio track \"Waveform\"
view) or on a dB scale relative to full scale (as
in the \"Waveform (dB)\" view).")

    (2 
"FILE FORMAT.\n
Following any header information:\n
SAMPLE LIST: produces a list of sample values.\n
INDEXED LIST: includes the sample number.\n
TIME INDEXED: includes the sample time.
Both types of index are relative to the start of
the selection.\n
DATA (csv): prints the sample values separated
by commas.\n
WEB PAGE (html): produces an HTML 5 document that
contains all of the header information and a table
of sample data with sample number, time, linear
and dB values. Browsers that are not HTML 5
compliant may not display the page correctly.\n
CHANNEL LAYOUT: for text/csv output, stereo tracks
may be printed alternate left/right samples or all 
of left channel then all of right channel.")

    (3 
"OPTIONAL HEADER TEXT:
This is provided for adding notes to the output
file. In text files, use ~~% to start a new line,
in HTML files use <br>.\n
NO HEADER: Prints only the optional header text 
(leave blank for none) followed by the sample data.\n
MINIMAL HEADER:
The sample rate.
Units (linear or dB).
Optional header text (leave blank for none).\n
STANDARD HEADER: minimal header plus:
File name.
Number of samples.
Duration (seconds).
Mono/Stereo.\n
FULL HEADER: standard header plus:
peak amplitude linear and dB.
Unweighted rms level (dB).
DC offset.")

    (4 (format nil
"OUTPUT FILES.\n
The default output folder is the \"home folder\":
~a
To select a different output folder, enter the 
full path name. The output folder must exist.\n
By default, files will not be overwritten. If you
select multiple tracks, they will be saved to
separate files with a number appended to the
name. If you set \"Allow files to be overwritten\"
to \"Yes\", only the last file for multiple tracks
will be retained.\n
A notification message is displayed on completion
indicating the name and location of the file.\n
If the plug-in is used in a Chain (Audacity 2.0.1
or later) it may be useful to disable messages.\n
For text/csv output the file header is shown in 
the debug window."
      (home)))))


(defun help (page)
  (let ((helptext (help-page page)))
    (format nil helptext)))

(defun printhelp ()
  (do ((page 1 (1+ page)))
      ((> page 4))
  (format fp "~a~%~%~%"(help-page page)))
  (close fp)
  (format nil "Help file written to:~%~a~a~a" 
        path fileseparator filename))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Header text         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nohead ()
  (if (> (length optext) 0)
    (format nil "~a~%~a~%" 
            optext 
            (get 'info 'chan-order))
    ""))


(defun minhead ()
  (format nil 
"Sample Rate: ~a Hz.  Sample values on ~a scale.~%~a~%~a" 
  (get 'info 'srate)                      ; sample rate
  (get 'info 'units)                      ; units
  (get 'info 'chan-order)                 ; Channel Order
  (if (> (length optext)0)
    (format nil "~a~%~%~%" optext)      ; optional text
    (format nil "~%~%"))))              ; no optional text


(defun normhead ()
  (format nil
"~a   ~a~%~aSample Rate: ~a Hz. Sample values on ~a scale.~%~
Length processed: ~a samples ~a seconds.~a" 
  filename                                ; file name
  (get 'info 'channels)                   ; mono/stereo
  (get 'info 'chan-order)                 ; Channel Order
  (get 'info 'srate)                      ; sample rate
  (get 'info 'units)                      ; units
  number                                  ; number of samples
  (get 'info 'duration)                   ; duration (seconds)
  (if (> (length optext)0)
    (format nil "~%~a~%~%~%" optext)      ; optional text
    (format nil "~%~%~%"))))              ; no optional text


(defun fullhead ()
  (format nil
"~a~%Sample Rate: ~a Hz. Sample values on ~a scale. ~a.~%~aLength processed: ~a ~
samples, ~a seconds.~%Peak amplitude: ~a (lin) ~a dB.  Unweighted rms: ~a dB.~%~
DC offset: ~a~a" 
  filename                                ; file name
  (get 'info 'srate)                      ; sample rate
  (get 'info 'units)                      ; units
  (get 'info 'channels)                   ; mono/stereo
  (get 'info 'chan-order)                 ; Channel Order
  number                                  ; number of samples
  (get 'info 'duration)                   ; duration (seconds)
  (setq smax (stereomax s))               ; peak amplitude linear
  (linear-to-db smax)                     ; peak amplitude dB
  (srms s)                                ; rms
  (let ((vals (dc-off s)))                ; DC offset
    (if (= (length vals) 2) ; mono
      (format nil "~a linear, ~a dB." 
              (first vals)(second vals))
      (format nil "Left: ~a lin, ~a dB | Right: ~a lin, ~a dB."
              (first vals)(second vals)(third vals)(fourth vals))))
  (if (> (length optext)0)
    (format nil "~%~a~%~%~%" optext)      ; optional text
    (format nil "~%~%~%"))))              ; no optional text


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        HTML Output         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun html-head ()
"<!DOCTYPE html>
<html>
<head>
<meta name=\"generator\" content=
\"Sample Data Export by Steve Daulton, (http://www.easyspacepro.com). Released under GPL v2.\">
<meta name=\"description\" content=\"Sample Printer, Free Audacity plug-in\" />
<meta name=\"keywords\" content=\"sample printer,Audacity,plug-ins,plugins,effects,audio,audio processing,music,analize,\" />
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
<title>Sample Data Export</title>
</head>
")


;;; document headings
(defun doc-head ()
  (format nil
"<body>
<h1>Sample Data Export - ~a</h1>
~a
<h4>~a. &nbsp;&nbsp;~a samples. &nbsp;&nbsp; ~a seconds.<br></h4>
<h3>Audio data analysis:</h3>
<ul>
<li><b>Sample Rate:</b> &nbsp;&nbsp;~a Hz.</li>
<li><b>Peak Amplitude:</b> &nbsp;&nbsp;~a (lin) &nbsp;&nbsp;~a dB.</li>
<li><b>RMS</b> (unweighted): &nbsp;&nbsp;~a dB.</li>
<li><b>DC Offset:</b> &nbsp;&nbsp;~a</li>
</ul>
"
  (string-right-trim ".html" filename)
  (format nil "<h2>~a</h2>" optext)       ; Optional heading
  (get 'info 'channels)                   ; mono/stereo
  number                                  ; number of samples
  (get 'info 'duration)                   ; duration (seconds)
  (get 'info 'srate)                      ; sample rate
  (setq smax (stereomax s))               ; peak amplitude linear
  (linear-to-db smax)                     ; peak amplitude dB
  (srms s)                                ; rms
  (let ((vals (dc-off s)))                ; DC offset
    (if (= (length vals) 2) ; mono
      (format nil "~a linear, &nbsp;&nbsp;~a dB." 
              (first vals)(second vals))
      (format nil "Left: ~a lin, ~a dB | Right: ~a linear, &nbsp;&nbsp;~a dB."
              (first vals)(second vals)(third vals)(fourth vals))))))


;;; table headings  (mono)
(defun table-head-mono ()
"<table title=\"sample data\">
<tr>
<th>Sample #</th>
<th>Seconds</th>
<th>Value (linear)</th>
<th>Value (dB)</th>
</tr>")


;;; table headings (stereo)
(defun table-head-stereo ()
"<table title=\"audio sample value analysis\">
<tr>
<th>Sample #</th>
<th>Seconds </th>
<th>Left (dB)</th>
<th>Right (dB)</th>
<th>Left (linear)</th>
<th>Right (linear)</th>
</tr>")


(defun html-foot ()
"</table>
<p id=\"footer\">Produced with <span>Sample Data Export</span> for
<a href=\"http://audacity.sourceforge.net/\">Audacity</a> by Steve
Daulton (<a href=
\"http://www.easyspacepro.com\">www.easyspacepro.com</a>)</p>
</body>
</html>")


;;; html generator
(defun make-htm (id val1 &optional val2)
  (if val2
    ;; stereo
    (let ((time (/ (1- id) *sound-srate*))
          (db1 (linear-to-db (abs val1)))
          (db2 (linear-to-db (abs val2))))
      (format fp 
        "<tr>~%<td>~a</td>~%<td>~a</td>~%<td>~a</td>~%<td>~a</td>~%~
        <td>~a</td>~%<td>~a</td>~%</tr>~%" 
        id time val1 val2 db1 db2))
    ;; mono
    (let ((time (/ (1- id) *sound-srate*))
           (db (linear-to-db (abs val1))))
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
      (format nil "~aData written to:~%~a~a~a"
             (normhead) path fileseparator filename)
      (s-rest 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       END OF HTML          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; get number from string
(setq number (read (make-string-input-stream number)))
(if (numberp number)
  (checknumber)
  (add-error (format nil "~a is not a number." number)))

(case help
  (0 (filewriter)
     (if (> (length err) 0)
       ;; output error message if enabled
       (unless (= messages 2)
         (format nil "Error.~%~a" err))
       ;; else print to file
       (progn
          ;; basic info for headers
          (putprop 'info (truncate *sound-srate*) 'srate)
          (putprop 'info (if (= units 0) "dB" "linear") 'units)
          (putprop 'info (/ number *sound-srate*) 'duration)
          (putprop 'info 
                    (if (arrayp s) "2 channels (stereo)""1 channel (mono)")
                    'channels)
          (putprop 'info
                    (if (arrayp s)
                      (if (= chan 0)
                        (if (= fileformat 3)
                          "Alternate Left/Right samples.\n"
                          "Left and right channels on alternate lines.\n")
                        "Left channel first then right channel.\n")
                      "")
                    'chan-order)
          (if (= fileformat 4)
            (printhtml)                   ; html output
            (printdata)))))               ; text output
  (1 (help 1))
  (2 (help 2))
  (3 (help 3))
  (4 (help 4))
  (5 (setq fileformat ".txt")
     (setq filename "SampleDataExport_Help")
     (filewriter)
     (if (> (length err) 0)
       (format nil "Error.~%~a" err)
       (printhelp))))
