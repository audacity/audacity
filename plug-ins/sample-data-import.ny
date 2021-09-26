$nyquist plug-in
$version 4
$type tool generate
$name (_ "Sample Data Import")
$manpage "Sample_Data_Import"
$action (_ "Reading and rendering samples...")
$author (_ "Steve Daulton")
$release 3.0.4
$copyright (_ "GNU General Public License v2.0 or later")

$control filename (_ "Select file") file "" "*default*/sample-data.txt" (((_ "Text file") (txt TXT))
                        ((_ "All files") (""))) "open,exists"
$control bad-data (_ "Invalid data handling") choice (("ThrowError" (_ "Throw Error"))
                                                      ("ReadAsZero" (_ "Read as Zero"))) 0


;; Released under terms of the GNU General Public License v2.0 or later:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


;; Check file can be opened
(defun fileopensp (fname)
  (cond
    ((not (setf fstream (open fname)))
        (throw 'err (format nil (_ "Error~%~
                        '~a' could not be opened.~%~
                        Check that file exists.")
                        fname)))
    ; File opened OK, so check for normal ASCII, then close it and return 'true'
    (t  (do ((j 0 (1+ j))(b (read-byte fstream)(read-byte fstream)))
            ((or (> j 100000)(not b)))
          (when (> b 127)
            (throw 'err (format nil (_ "Error:~%~
              The file must contain only plain ASCII text.~%~
              (Invalid byte '~a' at byte number: ~a)") b (1+ j) ))))
        (close fstream)
        t)))

;; ':new' creates a new class 'streamreader'
;; 'filestream' and 'chanel' are its instance variables.
;; (every object of class 'streamreader' has its own
;; copy of these variables)
(setq streamreader
  (send class :new '(filestream chanel)))

;; Initialize class 'streamreader'
(send streamreader :answer :isnew '(stream ch) '(
    (setq filestream stream)
    (setq channel ch)))

;; Create ':next' method.
;; Snd-fromobject calls this method to obtain the
;; next sound sample until it receives 'nil'
(send streamreader :answer :next '() '(
    (case channel
      (0  ;mono
        (read-and-verify filestream))
      (1  ;left channel
        ;Note, we still need to verify data even if skipping it.
        (let ((val (read-and-verify filestream)))
          (read-and-verify filestream) ;skip right channel sample
          val))
      (t  ;right channel
        (read-and-verify filestream) ;skip left channel sample
        (read-and-verify filestream)))))

(defun read-and-verify (stream)
"snd-fromobject requires float values, nil to terminate"
  (let ((val (read stream)))
    (cond
      ((not val) nil) ;end of file
      ((numberp val)  (float val)) ;valid.
      ((= bad-data 0) ;invalid. Throw error and quit
          (throw 'err (format nil (_ "Error~%~
              Data must be numbers in plain ASCII text.~%~
              '~a' is not a numeric value.") val)))
      (t  0.0)))) ;invalid. Replace with zero.

;; Instantiate a new sound object
(defun make-sound-object (stream chan)
  (send streamreader :new stream chan))

(defun sound-from-file (filename)
  ;; Set path. fileopenp should return 'true'
  (if (not (fileopensp filename))
      (throw 'err (format nil (_ "Error.~%Unable to open file"))))
  ; Note: we can't use (arrayp *track*) because
  ; *track* is nil in generate type plug-ins.
  (cond 
    ((= (get '*track*  'channels) 2)
        (let ((left-snd (get-sound filename 1))
              (right-snd (get-sound filename 2)))
          (vector left-snd right-snd)))
    (t  ;; Mono track
        (get-sound filename 0))))

(defun get-sound (fname chan)
  (let* ((stream (open fname :direction :input))
         (left (make-sound-object stream chan)))
    (setf audio-out (snd-fromobject 0 *sound-srate* left))
    (snd-play audio-out) ;force samples to be calculated now.
    (close stream)
    audio-out))

(catch 'err (sound-from-file filename))
