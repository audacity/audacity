$nyquist plug-in
$version 4
$type tool analyze
$name (_ "Export Label to Subtitle")
;$manpage ""
$debugbutton disabled
;$action (_ "Performing ...")
$author (_ "Cheng Huaiyu")
$release 1.0.0
$copyright (_ "Released under terms of the GNU General Public License version 2")
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control filename (_ "Export Label Track to File:") file (_ "Select a file") "*default*/subtitles" "SRT file|*.srt;*.SRT|LRC files|*.lrc;*.LRC" "save,overwrite"


;;; Reurn number as string with at least 2 digits
(defun pad (num)
  (format nil "~a~a" (if (< num 10) "0" "") num))


;;; Reurn number as string with at least 3 digits
(defun pad3 (num)
  (format nil "~a~a" (if (< num 10) "00" (if (< num 100) "0" "")) num))


;;; Format time (seconds) as hh:mm:ss,xxx
(defun srt-time-format (sec)
  (let* ((seconds (truncate sec))
        (hh (truncate (/ seconds 3600)))
        (mm (truncate (/ (rem seconds 3600) 60)))
        (ss (rem seconds 60))
        (xxx (round (* (- sec seconds) 1000))))
    (format nil "~a:~a:~a,~a" (pad hh) (pad mm) (pad ss) (pad3 xxx))))


;;; Format time (seconds) as mm:ss.xx
(defun lrc-time-format (sec)
  (let* ((seconds (truncate sec))
         (mm (truncate (/ seconds 60)))
         (ss (rem seconds 60))
         (xx (round (* (- sec seconds) 100))))
    (format nil "~a:~a.~a" (pad mm) (pad ss) (pad xx))))


;;; generate srt format subtitle
;;;;;; srt sample text:
;;; 1
;;; 00:00:00,260 --> 00:00:00,990
;;; subtitle 1
;;; 
;;; 2
;;; 00:00:02,220 --> 00:00:06,410
;;; subtitle 2
(defun label-to-srt (labels)
  ;; subtitle index
  (let ((srt "") 
        (ind 0))
    (dolist (label labels)
      (setq ind (1+ ind))
      (setf timeS (srt-time-format (first label)))
      (setf timeE (srt-time-format (second label)))
      (string-append srt (format nil "~a~%~a --> ~a~%~a~%~%" ind timeS timeE (third label))))
    (format nil srt)))


;;; generate mp3 lyric
;;;;;; srt sample text:
;;; [00:00.26] subtitle 1
;;; [00:02.22] subtitle 2
(defun label-to-lrc (labels)
  (setf lrc "")
  (string-append lrc "[ar:Lyrics artist]\n"
                      "[al:Album where the song is from]\n"
                      "[ti:Lyrics (song) title]\n"
                      "[au:Creator of the Songtext]\n"
                      "[length:How long the song is]\n"
                      "[by:Creator of the LRC file]\n"
                      "[offset:+/- Overall timestamp adjustment in milliseconds, + shifts time up, - shifts down]\n"
                      "[re:The player or editor that created the LRC file]\n"
                      "[ve:version of program]\n\n")

  (dolist (label labels)
    (setf timeS (lrc-time-format (first label)))
    (string-append lrc (format nil "[~a] ~a~%" timeS (third label))))
  (format nil lrc))


;;; Return file extension or empty string
(defun get-file-extension (fname)
  (let ((n (1- (length fname)))
        (ext ""))
    (do ((i n (1- i)))
        ((= i 0) ext)
      (when (char= (char fname i) #\.)
        (setf ext (subseq fname i))
        (return ext)))))


(setf file-ext (string-upcase (get-file-extension filename)))

;; Get labels from first label track
(setf labels (second (first (aud-get-info "labels"))))

;; detect file extension to determine which format to export
(setf txt (if (string= ".LRC" file-ext)
            (label-to-lrc labels)
            (label-to-srt labels)))

(setf fp (open filename :direction :output))
(format fp txt)
(close fp)

(format nil txt)