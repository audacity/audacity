;nyquist plug-in
;version 4
;type generate
;name "Sample Data Import..."
;action "Reading and rendering samples..."
;author "Steve Daulton"
;copyright "Released under terms of the GNU General Public License version 2"

;; sample-data-import.ny by Steve Daulton November 2016.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html


;control filename "File name" string "" "sample-data.txt"
;control path "File location (path to file)" string "" "Home directory"
;control bad-data "Invalid data handling" choice "Throw error,Read as zero" 0


;; Documentation.
;;
;; Introduction:
;; This plug-in reads numeric values from the specified
;; file and creates a PCM sample for each numeric value
;; read. The values are read as 32-bit float numbers,
;; where a range of +/- 1.0 represents 0 dB (full scale).
;; The file MUST be plain ASCII text, and values should
;; be separated by spaces, tabs or line-breaks.
;; The file name must have a '.txt' file extension.
;;
;; *** ATTENTION ***
;; The file to be imported must contain
;; plain ASCII text only.
;; Files other than plain ASCII text are NOT SUPPORTED.
;; ***************
;; 
;; Depending on the computer file system, the file name
;; may be case sensitive. The named file must be located
;; in the specified directory (the "File location").
;; The directory may be one of:
;; 1) "Home Directory" (without quotes. This is the default).
;;    The "Home Directory" is normally:
;;    C:\Users\<username>  (Windows)
;;    /Users/<username>    (Mac OS X)
;;    /home/<username>     (Linux)
;; 2) The fully qualified path, for example:
;;    C:\Users\(User_Name)\Documents
;; 3) Linux & Mac only:
;;    The fully qualified path using "~/" (tilde slash)
;;    as an abbreviation for the Home folder.
;; If the "File location" is not defined (empty), the
;; plug-in will look in the "home" folder for the named
;; file.
;; If the file cannot be found, the plug-in will abort
;; and an error will be shown.
;; 
;; Number of channels:
;; If a stereo track is selected, then the data is assumed
;; to be stereo, otherwise the data will be treated as
;; mono.
;; Stereo data must be interleaved (alternate left/right
;; sample values).
;; 
;; Data format:
;; Sample data must be plain ASCII text only and should
;; be numeric values only.
;; 
;; Values must be separated by one of:
;; 1) One or more spaces.
;; 2) One or more tabs.
;; 3) One or more line breaks.
;; 4) Any combination of the above.
;; 
;; Data comments:
;; Comments may be included in the file by preceding
;; the comment with a semicolon. All text between a
;; semicolon and the end of the line is ignored.
;;
;; Invalid data handling:
;; There are two options for handling invalid data:
;; 1) Throw error (default):
;;    Any text that cannot be read as a numeric value
;;    will abort the effect and display an error message.
;;    The error message will attempt to display the
;;    invalid text.
;; 2) Read as zero:
;;    Any text that cannot be read as a numeric value
;;    will produce a zero (silent) sample value.
;; 
;; Further information:
;; http://manual.audacityteam.org/man/sample_data_import.html


;; home directory
(defun home ()
  (if (windowsp)
      (get-env "UserProfile") ;Windows
      (get-env "HOME")));Mac / Linux

;; Check if OS is Windows
(defun windowsp ()
  (char= #\\ *file-separator*))

;; Check file can be opened
;; As Nyquist plug-ins do not (yet) have a file browser,
;; we need to be quite rigorous with error checking here.
(defun fileopensp (path fname)
  (let ((path (string-trim " " path)))
    (if (string-equal fname "")
        (throw 'err "Error\nNo file name."))
    (if (string-not-equal fname ".txt" :start1 (- (length fname) 4))
        (throw 'err "Error\nThe file must be a plain ASCII text file\nwith '.txt' file extension."))
    ;; Handle special 'path' formats:
    (cond
      ; "~" without "/" is not recommended (or documented)
      ; but more user friendly to allow it.
      ((string= path "~")
          (if (windowsp)
              "Error\n'~/' is not valid on Windows"
              (setq path (home))))
      ;; replace "~/" on Linux/Mac
      ((and (>= (length path) 2) (string= path "~/" :end1 2))
          (if (windowsp)
              "Error\n'~/' is not valid on Windows"
              (setq path (strcat (home)(subseq path 1)))))
      ((string-equal path "Home directory")
          (setf path (home)))
      ;; If empty, use 'Home'
      ((string-equal path "")
          (setf path (home))))
    ;; Now check that the file can be opened:
    (cond
      ((not (setdir path))
          (throw 'err (format nil "Error~%~
                          Directory '~a' could not be opened." path)))
      ((not (setf fstream (open fname)))
          (throw 'err (format nil "Error~%~
                          '~a~a~a' could not be opened.~%~
                          Check that file exists."
                          path *file-separator* fname)))
      ; File opened OK, so check for normal ASCII, then close it and return 'true'
      (t  (do ((j 0 (1+ j))(b (read-byte fstream)(read-byte fstream)))
              ((or (> j 100000)(not b)))
            (when (> b 127)
              (throw 'err (format nil "Error:~%~
                The file must contain only plain ASCII text.~%~
                (Invalid byte '~a' at byte number: ~a)" b (1+ j) ))))
          (close fstream)
          t))))

;; ':new' creates a new class 'streamreader'
;; 'filestream' and 'chanel' are its instance variables.
;; (every objet of class 'streamreader' has it's own
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
          (throw 'err (format nil "Error~%~
              Data must be numbers in plain ASCII text.~%~
              '~a' is not a numeric value." val)))
      (t  0.0)))) ;invalid. Replace with zero.

;; Instantiate a new sound object
(defun make-sound-object (stream chan)
  (send streamreader :new stream chan))

(defun sound-from-file (filename)
  ;; Set path. fileopenp should return 'true'
  (if (not (fileopensp path filename))
      (throw 'err "Error.\nUnable to open file"))
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
