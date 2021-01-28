;; spectral-analysis.lsp -- functions to simplify computing
;;   spectrogram data
;;
;; Roger B. Dannenberg and Gus Xia
;; Jan 2013, modified Oct 2017

;; API:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set sa-obj = sa-init(resolution: <nil or Hz>,
;;                      fft-dur: <nil or seconds>,
;;                      skip-period: <seconds>,
;;                      window: <window type>, 
;;                      input: <filename or sound>)
;; 
;; sa-init() creates a spectral-analysis object that can be used
;; to obtain spectral data from a sound.
;;
;; resolution is the width of each spectral bin in Hz. If nil of
;;     not specified, the resolution is computed from fft-dur. 
;;     The actual resolution will be finer than the specified 
;;     resolution because fft sizes are rounded to a power of 2.
;; fft-dur is the width of the FFT window in seconds. The actual
;;     FFT size will be rounded up to the nearest power of two
;;     in samples. If nil, fft-dur will be calculated from 
;;     resolution. If both fft-size and resolution are nil
;;     or not specified, the default value of 1024 samples,
;;     corresponding to a duration of 1024 / signal-sample-rate,
;;     will be used. If both resolution and fft-dur are
;;     specified, the resolution parameter will be ignored.
;;     Note that fft-dur and resolution are reciprocals.
;; skip-period specifies the time interval in seconds between 
;;     successive spectra (FFT windows). Overlapping FFTs are
;;     possible. The default value overlaps windows by 50%. 
;;     Non-overlapped and widely spaced windows that ignore 
;;     samples by skipping over them entirely are also acceptable.
;; window specifies the type of window. The default is raised
;;     cosine (Hann or "Hanning") window. Options include
;;     :hann, :hanning, :hamming, :none, nil, where :none and
;;     nil mean a rectangular window.
;; input can be a string (which specifies a sound file to read)
;;     or a Nyquist SOUND to be analyzed.
;; Return value is an XLISP object that can be called to obtain
;;     parameters as well as a sequence of spectral frames.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set sa-frame = sa-next(sa-obj)
;;
;; sa-next() fetches the next spectrum from sa-obj.
;;
;; sa-obj is a spectral-analysis object returned by sa-init().
;; Return value is an array of FLONUMS representing the discrete
;;     spectrum.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exec sa-info(sa-obj)
;;
;; sa-info prints information about the spectral computation.
;;
;; sa-obj is a spectral-analysis object returned by sa-init().
;; Return value is nil, but information is printed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set mag = sa-magnitude(frame)
;;
;; sa-magnitude computes the magnitude (amplitude) spectrum
;; from a frame returned by sa-frame.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exec sa-plot(sa-obj, sa-frame)
;;
;; sa-plot plots the amplitude (magnitude) spectrum of sa-frame.
;;
;; sa-obj is used to determine the bin width of data in sa-frame.
;;
;; sa-frame is a spectral frame (array) returned by sa-next()
;;
;; Return value is nil, but a plot is generated and displayed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set hz = sa-get-bin-width(sa-obj)
;; set n = sa-get-fft-size(sa-obj)
;; set secs = sa-get-fft-dur(sa-obj)
;; set window = sa-get-fft-window(sa-obj)
;; set skip-period = sa-get-skip-period(sa-obj)
;; set m = sa-get-fft-skip-size(sa-obj)
;; set sr = sa-get-sample-rate(sa-obj)
;;
;; These functions retrieve data from the sa-obj created by 
;; sa-init. The return values are:
;;   hz - the width of a frequency bin (also the separation
;;       of bin center frequencies). The center frequency of
;;       the i'th bin is i * hz.
;;   n - the size of the FFT, an integer, a power of two. The
;;       size of a spectral frame (an array returned by sa-next)
;;       is (n / 2) + 1.
;;   secs - the duration of an FFT window.
;;   window - the type of window used (:hann, :hamming, :none)
;;   skip-period - the time in seconds of the skip (the time
;;       difference between successive frames
;;   m - the size of the skip in samples.
;;   sr - the sample rate of the sound being analyzed (in Hz, a flonum)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; define the class of spectral analysis objects
(setf sa-class (send class :new '(sound length skip window window-type)))

(send sa-class :answer :next '() '(
    (snd-fft sound length skip window)))

(defun sa-raised-cosine (alpha beta)
  (sum (const alpha)
       (scale beta (lfo 1.0 1.0 *sine-table* 270))))

(defun sa-fft-window (frame-size alpha beta)
  (abs-env (control-srate-abs frame-size                
               (sa-raised-cosine alpha beta))))

(defun hann-window (frame-size) (sa-fft-window frame-size 0.5 0.5))
(defun hamming-window (frame-size) (sa-fft-window frame-size 0.54 0.46))

(defun sa-get-window-type (win-type)
  (case win-type
    ((:hann :hanning)    :hann)
    ((nil :none)         :none)
    (:hamming            :hamming)
    (t (print "Warning: invalid window-type parameter: ~A~%" win-type)
       (print "    Using :HAMMING instead.~%")
       :hamming)))


(defun sa-compute-window (len win-type)
  (case win-type
    (:hann        (hann-window len))
    (:none        nil)
    (:hamming     (hamming-window len))
    (t (print "Warning: invalid window-type parameter: ~A~%" win-type)
       (print "    Using :HAMMING instead.~%")
       (hamming-window len))))
  

(send sa-class :answer :isnew '(snd len skp win-type) '(
    (setf sound snd)
    (setf length len)
    (setf skip skp)
    (setf window-type (sa-get-window-type win-type))
    (setf window (sa-compute-window length window-type))))


;; sa-to-mono -- sum up the channels in an array
;;
(defun sa-to-mono (s)
  (let ((mono (aref s 0)))
    (dotimes (i (1- (length s)))
      (setf mono (sum mono (aref s (1+ i)))))
    mono))


(defun sa-init (&key resolution fft-dur skip-period window input)
  (let (len sr n skip)
    (cond ((stringp input)
           (setf input (s-read input))))
    (cond ((arrayp input)
           (format t "Warning: sa-init is converting stereo sound to mono~%")
           (setf input (sa-to-mono input)))
          ((soundp input) ;; so that variables are not "consumed" by snd-fft
           (setf input (snd-copy input))))
    (cond ((not (soundp input))
           (error
            (format nil
             "Error: sa-init did not get a valid :input parameter~%"))))
    (setf sr (snd-srate input))
    (setf len 1024)
    (cond (fft-dur
           (setf len (* fft-dur sr)))
          (resolution
           (setf len (/ sr resolution))))
    ;; limit fft size to between 4 and 2^16
    (cond ((> len 65536)
           (format t "Warning: fft-size reduced from ~A to 65536~%" len)
           (setf len 65536))
          ((< len 4)
           (format t "Warning: fft-size increased from ~A to 4~%" len)
           (setf len 4)))
    ;; round up len to a power of two
    (setf n 4)
    (while (< n len)
      (setf n (* n 2)))
    (setf length n) ;; len is now an integer power of 2
    ;(display "sa-init" length)
    ;; compute skip length - default is len/2
    (setf skip (if skip-period (round (* skip-period sr))
                               (/ length 2)))
    (send sa-class :new input length skip window)))


(defun sa-next (sa-obj)
  (send sa-obj :next))

(defun sa-info (sa-obj)
  (send sa-obj :info))

(send sa-class :answer :info '() '(
  (format t "Spectral Analysis object (instance of sa-class):~%")
  (format t "  resolution (bin width): ~A Hz~%" (/ (snd-srate sound) length))
  (format t "  fft-dur: ~A s (~A samples)~%" (/ length (snd-srate sound)) length)
  (format t "  skip-period: ~A s (~A samples)~%" (/ skip (snd-srate sound)) skip)
  (format t "  window: ~A~%" window-type)
  nil))


(defun sa-plot (sa-obj frame)
  (send sa-obj :plot frame))

(defun sa-magnitude(frame)
  (let* ((flen (length frame))
         (n (/ (length frame) 2)) ; size of amplitude spectrum - 1
         (as (make-array (1+ n))))  ; amplitude spectrum
    ;; first compute an amplitude spectrum
    (setf (aref as 0) (abs (aref frame 0))) ;; DC
    ;; half_n is actually length/2 - 1, the number of complex pairs
    ;;    in addition there is the DC and Nyquist terms, which are
    ;;    real and in the first and last slots of frame
    (setf half_n (1- n))
    (dotimes (i half_n)
      (let* ((i2 (+ i i 2))  ; index of the imag part
             (i2m1 (1- i2)) ; index of the real part
             (amp (sqrt (+ (* (aref frame i2m1) (aref frame i2m1))
                           (* (aref frame i2)   (aref frame i2))))))
        (setf (aref as (1+ i)) amp)))
    (setf (aref as n) (aref frame (1- flen)))
    as)) ;; return the amplitude spectrum
  

(send sa-class :answer :plot '(frame) '(
  (let* ((as (sa-magnitude frame))
         (sr (snd-srate sound)))
    (s-plot (snd-from-array 0 (/ length sr) as)
            sr (length as)))))

(defun sa-get-bin-width (sa-obj)
  (send sa-obj :get-bin-width))

(send sa-class :answer :get-bin-width '()
      '((/ (snd-srate sound) length)))

(defun sa-get-fft-size (sa-obj)
  (send sa-obj :get-fft-size))

(send sa-class :answer :get-fft-size '() '(length))

(defun sa-get-fft-dur (sa-obj)
  (send sa-obj :get-fft-dur))

(send sa-class :answer :get-fft-dur '() '(/ length (snd-srate sound)))

(defun sa-get-fft-window (sa-obj)
  (send sa-obj :get-fft-window))

(send sa-class :answer :get-fft-window '() '(window-type))

(defun sa-get-fft-skip-period (sa-obj)
  (send sa-obj :get-skip-period))

(send sa-class :answer :get-skip-period '() '((/ skip (snd-srate sound))))

(defun sa-get-fft-skip-size (sa-obj)
  (send sa-obj :get-skip-size))

(send sa-class :answer :get-fft-skip-size '() '(skip))

(defun sa-get-sample-rate (sa-obj)
  (send sa-obj :get-sample-rate))

(send sa-class :answer :get-sample-rate '() '((snd-srate sound)))


;;;;;;; TESTS ;;;;;;;;;;


(defun plot-test ()
  (let (frame)
    (setf sa (sa-init :input "./rpd-cello.wav"))
    (while t
      (setf frame (sa-next sa))
      (if (null sa) (return nil))
      (sa-plot sa frame))))

