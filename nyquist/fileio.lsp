;; fileio.lsp

;; if *default-sf-dir* undefined, set it to user's tmp directory
;;
(cond ((not (boundp '*default-sf-dir*))
       ;; it would be nice to use get-temp-path, but when running
       ;; the Java-based IDE, Nyquist does not get environment
       ;; variables to tell TMP or TEMP or USERPROFILE
       ;; We want to avoid the current directory because it may
       ;; be read-only. Search for some likely paths...
       ;; Note that since these paths don't work for Unix or OS X,
       ;; they will not be used, so no system-dependent code is 
       ;; needed
       (let ((current (setdir ".")))
         (setf *default-sf-dir*
               (or (setdir "c:\\tmp\\" nil)
                   (setdir "c:\\temp\\" nil)
                   (setdir "d:\\tmp\\" nil)
                   (setdir "d:\\temp\\" nil)
                   (setdir "e:\\tmp\\" nil)
                   (setdir "e:\\temp\\" nil)
	           (get-temp-path)))
         (format t "Set *default-sf-dir* to \"~A\" in fileio.lsp~%" 
		 *default-sf-dir*)
	 (setdir current))))

;; if the steps above fail, then *default-sf-dir* might be "" (especially
;; on windows), and the current directory could be read-only on Vista and
;; Windows 7. Therefore, the Nyquist IDE will subsequently call
;; suggest-default-sf-dir with Java's idea of a valid temp directory.
;; If *default-sf-dir* is the empty string (""), this will set the variable:
(defun suggest-default-sf-dir (path)
  (cond ((equal *default-sf-dir* "") (setf *default-sf-dir* path))))

;; s-save -- saves a file
(setf NY:ALL 1000000000)	; 1GIG constant for maxlen
(defmacro s-save (expression &optional (maxlen NY:ALL) filename 
                  &key (format '*default-sf-format*)
                  (mode '*default-sf-mode*) (bits '*default-sf-bits*)
                  (endian NIL) ; nil, :big, or :little -- specifies file format
                  (play nil))
  `(let ((ny:fname ,filename)
         (ny:maxlen ,maxlen)
         (ny:endian ,endian)
         (ny:swap 0))
     ; allow caller to omit maxlen, in which case the filename will
     ; be a string in the maxlen parameter position and filename will be null
     (cond ((null ny:fname)
                 (cond ((stringp ny:maxlen)
                            (setf ny:fname ny:maxlen)
                            (setf ny:maxlen NY:ALL))
                           (t
                            (setf ny:fname *default-sound-file*)))))
     
     (cond ((equal ny:fname "")
                 (cond ((not ,play)
                       (format t "s-save: no file to write! play option is off!\n"))))
           (t
            (setf ny:fname (soundfilename ny:fname))
            (format t "Saving sound file to ~A~%" ny:fname)))
	 (cond ((eq ny:endian :big)
	        (setf ny:swap (if (bigendianp) 0 1)))
		   ((eq ny:endian :little)
			(setf ny:swap (if (bigendianp) 1 0))))
     (snd-save ',expression ny:maxlen ny:fname ,format ,mode ,bits ny:swap ,play)))

;; MULTICHANNEL-MAX -- find peak over all channels
;;
(defun multichannel-max (snd samples)
  (cond ((soundp snd)
	 (snd-max snd samples))
	((arrayp snd) ;; assume it is multichannel sound
	 (let ((peak 0.0) (chans (length snd)))
	   (dotimes (i chans)
	     (setf peak (max peak (snd-max (aref snd i) (/ samples chans)))))
	   peak))
	(t (error "unexpected value in multichannel-max" snd))))



;; AUTONORM -- look ahead to find peak and normalize sound to 80%
;;
(defun autonorm (snd)
  (let (peak)
    (cond (*autonormflag*
	   (cond ((and (not (soundp snd))
		       (not (eq (type-of snd) 'ARRAY)))
		  (error "AUTONORM (or PLAY?) got unexpected value" snd))
		 ((eq *autonorm-type* 'previous)
		  (scale *autonorm* snd))
		 ((eq *autonorm-type* 'lookahead)
		  (setf peak (multichannel-max snd *autonorm-max-samples*))
		  (setf peak (max 0.001 peak))
                  (setf *autonorm* (/ *autonorm-target* peak))
		  (scale *autonorm* snd))
		 (t
		  (error "unknown *autonorm-type*"))))
	  (t snd))))
	

(init-global *clipping-threshold* (/ 127.0 128.0))

(defmacro s-save-autonorm (expression &rest arglist)
  `(let ((peak (s-save (autonorm ,expression) ,@arglist)))
     (when (and *clipping-error* (> peak *clipping-threshold*))
       (format t "s-save-autonorm peak ~A from ~A~%" peak ,expression)
       (error "clipping"))
     (autonorm-update peak)))

;; If the amplitude exceeds *clipping-threshold*, an error will
;; be raised if *clipping-error* is set.
;;
(init-global *clipping-error* nil)

;; The "AutoNorm" facility: when you play something, the Nyquist play
;; command will automatically compute what normalization factor you
;; should have used. If you play the same thing again, the normalization
;; factor is automatically applied.
;;
;; Call AUTONORM-OFF to turn off this feature, and AUTONORM-ON to turn
;; it back on.
;;
;; *autonorm-target* is the peak value we're aiming for (it's set below 1
;; so allow the next signal to get slightly louder without clipping)
;;
(init-global *autonorm-target* 0.9)
;;
;; *autonorm-type* selects the autonorm algorithm to use
;;   'previous means normalize according to the last computed sound
;;   'precompute means precompute *autonorm-max-samples* samples in
;;       memory and normalize according to the peak
;;
(init-global *autonorm-type* 'lookahead)
(init-global *autonorm-max-samples* 1000000) ; default is 4MB buffer

;;
(defun autonorm-on ()
  (setf *autonorm* 1.0)
  (setf *autonorm-previous-peak* 1.0)
  (setf *autonormflag* t)
  (format t "AutoNorm feature is on.~%"))

(if (not (boundp '*autonormflag*)) (autonorm-on))

(defun autonorm-off ()
  (setf *autonormflag* nil)
  (setf *autonorm* 1.0)
  (format t "AutoNorm feature is off.~%"))

(defun explain-why-autonorm-failed ()
  (format t "~A~A~A~A~A~A"
          "     *autonorm-type* is LOOKAHEAD and your sound got\n"
          "       louder after the lookahead period, resulting in\n"
          "       too large a scale factor and clipping. Consider\n"
          "       setting *autonorm-type* to 'PREVIOUS. Alternatively,\n"
          "       try turning off autonorm, e.g. \"exec autonorm-off()\"\n"
          "       or in Lisp mode, (autonorm-off), and scale your sound\n"
          "       as follows.\n"))


;; AUTONORM-UPDATE -- called with true peak to report and prepare
;;
;; after saving/playing a file, we have the true peak. This along
;; with the autonorm state is printed in a summary and the autonorm
;; state is updated for next time.
;;
;; There are currently two types: PREVIOUS and LOOKAHEAD
;; With PREVIOUS:
;;   compute the true peak and print the before and after peak
;;   along with the scale factor to be used next time
;; With LOOKAHEAD:
;;   compute the true peak and print the before and after peak
;;   along with the "suggested scale factor" that would achieve
;;   the *autonorm-target*
;;
(defun autonorm-update (peak)
  (cond ((> peak 1.0)
         (format t "*** CLIPPING DETECTED! ***~%")))
  (cond ((and *autonormflag* (> peak 0.0))
         (setf *autonorm-previous-peak* (/ peak *autonorm*))
         (setf *autonorm* (/ *autonorm-target* *autonorm-previous-peak*))
         (format t "AutoNorm: peak was ~A,~%" *autonorm-previous-peak*)
         (format t "     peak after normalization was ~A,~%" peak)
         (cond ((eq *autonorm-type* 'PREVIOUS)
                (cond ((zerop *autonorm*)
                       (setf *autonorm* 1.0)))
                (format t "     new normalization factor is ~A~%" *autonorm*))
               ((eq *autonorm-type* 'LOOKAHEAD)
                (cond ((> peak 1.0)
                       (explain-why-autonorm-failed)))
                (format t "     suggested manual normalization factor is ~A~%"
                          *autonorm*))
               (t
                (format t
                 "     unexpected value for *autonorm-type*, reset to LOOKAHEAD\n")
                (setf *autonorm-type* 'LOOKAHEAD))))
        (t
         (format t "Peak was ~A,~%" peak)
         (cond ((> peak 0.0)
                (format t "     suggested normalization factor is ~A~%"
                        (/ *autonorm-target* peak))))))
   peak
  )


;; s-read -- reads a file
(defun s-read (filename &key (time-offset 0) (srate *sound-srate*)
        (dur 10000.0) (nchans 1) (format *default-sf-format*)
        (mode *default-sf-mode*) (bits *default-sf-bits*) (endian NIL))
  (let ((swap 0))
    (cond ((eq endian :big)
           (setf swap (if (bigendianp) 0 1)))
          ((eq endian :little)
           (setf swap (if (bigendianp) 1 0))))
    (if (minusp dur) (error "s-read :dur is negative" dur))
    (snd-read (soundfilename filename) time-offset
            (local-to-global 0) format nchans mode bits swap srate
            dur)))

;; SF-INFO -- print sound file info
;;
(defun sf-info (filename)
  (let (s format channels mode bits swap srate dur flags)
    (format t "~A:~%" (soundfilename filename))
    (setf s (s-read filename))
    (setf format (car *rslt*))
    (setf channels (cadr *rslt*))
    (setf mode (caddr *rslt*))
    (setf bits (cadddr *rslt*))
    (setf *rslt* (cddddr *rslt*))
    (setf swap (car *rslt*))
    (setf srate (cadr *rslt*))
    (setf dur (caddr *rslt*))
    (setf flags (cadddr *rslt*))
    (format t "Format: ~A~%" 
            (nth format '("none" "AIFF" "IRCAM" "NeXT" "Wave" "PAF" "SVX"
                          "NIST" "VOC" "W64" "MAT4" "Mat5" "PVF" "XI" "HTK"
                          "SDS" "AVR" "SD2" "FLAC" "CAF")))
    (cond ((setp (logand flags snd-head-channels))
           (format t "Channels: ~A~%" channels)))
    (cond ((setp (logand flags snd-head-mode))
           (format t "Mode: ~A~%"
                   (nth mode '("ADPCM" "PCM" "uLaw" "aLaw" "Float" "UPCM"
                               "unknown" "double" "GSM610" "DWVW" "DPCM"
                               "msadpcm")))))
    (cond ((setp (logand flags snd-head-bits))
           (format t "Bits/Sample: ~A~%" bits)))
    (cond ((setp (logand flags snd-head-srate))
           (format t "SampleRate: ~A~%" srate)))
    (cond ((setp (logand flags snd-head-dur))
           (format t "Duration: ~A~%" dur)))
    ))

;; SETP -- tests whether a bit is set (non-zero)
;
(defun setp (bits) (not (zerop bits)))

;; IS-FILE-SEPARATOR -- is this a file path separation character, e.g. "/"?
;;
(defun is-file-separator (c)
  (or (eq c *file-separator*)
      (and (eq *file-separator* #\\) ;; if this is windows (indicated by "\")
           (eq c #\/)))) ;; then "/" is also a file separator

;; SOUNDFILENAME -- add default directory to name to get filename
;;
(defun soundfilename (filename)
  (cond ((= 0 (length filename))
         (break "filename must be at least one character long" filename))
        ((full-name-p filename))
        (t
         ; if sf-dir nonempty and does not end with filename separator,
         ; append one
         (cond ((and (< 0 (length *default-sf-dir*))
                     (not (is-file-separator
                           (char *default-sf-dir* 
                                 (1- (length *default-sf-dir*))))))
                (setf *default-sf-dir* (strcat *default-sf-dir* (string *file-separator*)))
                (format t "Warning: appending \"~A\" to *default-sf-dir*~%"
                        *file-separator*)))
         (setf filename (strcat *default-sf-dir* (string filename)))))
  ;; now we have a file name, but it may be relative to current directory, so 
  ;; expand it with the current directory
  (cond ((relative-path-p filename)
         ;; get current working directory and build full name
         (let ((path (setdir ".")))
           (cond (path
                  (setf filename (strcat path (string *file-separator*) 
                                         (string filename))))))))
  filename)


(setfn s-read-format car)
(setfn s-read-channels cadr)
(setfn s-read-mode caddr)
(setfn s-read-bits cadddr)
(defun s-read-swap (rslt) (car (cddddr rslt)))
(defun s-read-srate (rslt) (cadr (cddddr rslt)))
(defun s-read-dur (rslt) (caddr (cddddr rslt)))
(defun s-read-byte-offset (rslt) (car (cddddr (cddddr rslt))))

;; round is tricky because truncate rounds toward zero as does C
;; in other words, rounding is down for positive numbers and up
;; for negative numbers. You can convert rounding up to rounding
;; down by subtracting one, but this fails on the integers, so
;; we need a special test if (- x 0.5) is an integer
(defun round (x) 
  (cond ((> x 0) (truncate (+ x 0.5)))
        ((= (- x 0.5) (truncate (- x 0.5))) (truncate x))
        (t (truncate (- x 0.5)))))

;; change defaults for PLAY macro:
(init-global *soundenable* t)
(defun sound-on () (setf *soundenable* t))
(defun sound-off () (setf *soundenable* nil))

(defun coterm (snd1 snd2)
  (multichan-expand #'snd-coterm snd1 snd2))

(defmacro s-add-to (expr maxlen filename &optional (time-offset 0.0))
  `(let ((ny:fname (soundfilename ,filename))
         ny:peak ny:input (ny:offset ,time-offset))
    (format t "Adding sound to ~A at offset ~A~%" 
              ny:fname ,time-offset)
    (setf ny:peak (snd-overwrite '(let ((ny:addend ,expr))
                                   (sum (coterm
                                         (s-read ny:fname
                                          :time-offset ny:offset)
                                         ny:addend)
                                    ny:addend))
                   ,maxlen ny:fname ny:offset SND-HEAD-NONE 0 0 0 0.0))
    (format t "Duration written: ~A~%" (car *rslt*))
    ny:peak))


(defmacro s-overwrite (expr maxlen filename &optional (time-offset 0.0))
  `(let ((ny:fname (soundfilename ,filename))
         (ny:peak 0.0)
         ny:input ny:rslt (ny:offset ,time-offset))
    (format t "Overwriting ~A at offset ~A~%" ny:fname ny:offset)
    (setf ny:offset (s-read-byte-offset ny:rslt))
    (setf ny:peak (snd-overwrite `,expr ,maxlen ny:fname ny:offset
                   SND-HEAD-NONE 0 0 0 0.0))
    (format t "Duration written: ~A~%" (car *rslt*))
    ny:peak))




