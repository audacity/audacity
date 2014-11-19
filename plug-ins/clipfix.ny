;nyquist plug-in
;version 1
;type process
;preview enabled
;categories "http://audacityteam.org/namespace#NoiseRemoval"
;name "Clip Fix..."
;action "Reconstructing clips..."
;author "Benjamin Schwartz"
;copyright "Licensing confirmed under terms of the GNU General Public License version 2"

;; clipfix.ny by Benjamin Schwartz.
;; Licensing confirmed under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;; with kind agreement of Benjamin Schwartz, December 2011.
;; GUI updated by Steve Daulton July 2012
;;
;; For information about writing and modifying Nyquist plug-ins:
;; http://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;control thresh "Threshold of Clipping (%)" real "" 95 0 100

(setf largenumber 100000000) ;;Largest number of samples that can be imported
(setf blocksize 100000)

;;Clip Fix is a simple, stupid (but not blind) digital-clipping-corrector
;;The algorithm is fairly simple:
;;1. Find all clipped regions
;;2. Get the slope immediately on either side of the region
;;3. Do a cubic spline interpolation.
;;4. Go to next region

;;Coded from start (didn't know lisp (well, scheme, but not not lisp and certainly not
;;some XLISP 2.0 derivative)) to finish
;;(fully working, more or less) in one afternoon (and some evening).
;;Written by Benjamin Schwartz, MIT class of 2006, on May 25, 2004.
;;Explanatory text added by Gale Andrews, May 2008.

(defun declip (sin) ;;Central function
(let* ((threshold  (* (peak sin largenumber) thresh 0.01))
(s2 (snd-copy sin))
(samplerate (snd-srate s2))
(s2length (snd-length s2 largenumber)))

(seqrep (i (1+ (/ s2length blocksize)))
  (let ((l (min blocksize (- s2length (* i blocksize)))))
     ;;(print (list i t0 l samplerate))
     (snd-from-array 0 samplerate 
	(workhorse 
		;;(let () (print (list s2 (type-of s2) l (type-of l)))
			(snd-fetch-array s2 l l)
		;;)
			threshold))))

;;(setf r (snd-fetch-array (snd-copy s) (snd-length s largenumber) 1)) ;;Create a sound array
;;(snd-from-array (snd-t0 s) (snd-srate s) (workhorse r threshold))
))

(defun workhorse (r threshold)

(setf n (length r)) ;; Record its length

(setf exithigh ()) ;;Times when the wavefrom left the allowed region
(setf returnhigh ())  ;;Times when it returned to the allowed region

(setf drange 4)

(let ((i drange) (max (- n drange))) ;;Leave room at ends for derivative processing
  (while (< i max)
  (if (>= (aref r i) threshold) 
    (if (< (aref r (- i 1)) threshold)
      (setq exithigh (cons (- i 1) exithigh))) ;;We just crossed the threshold up
    (if (>= (aref r (- i 1)) threshold)
      (setq returnhigh (cons i returnhigh)))) ;;We just crossed the threshold down
  (setq i (1+ i))))

(setq exithigh (reverse exithigh)) ;;List comes out backwards
(setq returnhigh (reverse returnhigh))

(if (>= (aref r (1- drange)) threshold) ;;If the audio begins in a clipped region, ignore 
  (setq returnhigh (cdr returnhigh))) ;the extra return from threshold

(setf exitlow ())  ;; Same as above, but for the bottom threshold
(setf returnlow ())

(setf threshlow (* -1 threshold)) ;;Assumes your digital range is zero-centered


(let ((i drange) (max (- n drange)))
  (while (< i max)
  (if (<= (aref r i) threshlow)
    (if (> (aref r (- i 1)) threshlow)
      (setq exitlow (cons (- i 1) exitlow)))
    (if (<= (aref r (- i 1)) threshlow)
      (setq returnlow (cons i returnlow))))
  (setq i (1+ i))))

(setq exitlow (reverse exitlow))
(setq returnlow (reverse returnlow))

(if (<= (aref r (1- drange)) threshlow)
  (setq returnlow (cdr returnlow)))

(while (and exithigh returnhigh) ;;If there are more clipped regions
    (let* ((t1 (car exithigh))  ;;exit time
          (t2 (car returnhigh)) ;;return time
          (d1 (max 0 (/ (- (aref r t1) (aref r (- t1 (1- drange)))) (1- drange)))) ;;slope at exit
          (d2 (min 0 (/ (- (aref r (+ t2 (1- drange))) (aref r t2)) (1- drange)))) ;;slope at return
          (m (/ (+ d2 d1) (* (- t2 t1) (- t2 t1)))) ;;interpolation is by (t-t1)(t-t2)(mx+b)
          (b (- (/ d2 (- t2 t1)) (* m t2))) ;;These values of m and b make the cubic seamless
          (j (1+ t1))) ;; j is the index

     (while (< j t2)
         (setf (aref r j) (+ (aref r t1) (* (- j t1) (- j t2) (+ (* m j) b)))) 
	 (setf (aref r j) (+ (* (- t2 j) (/ (aref r t1) (- t2 t1))) (* (- j t1) (/ (aref r t2) (- t2 t1)))  (* (- j t1) (- j t2) (+ (* m j) b))))
         (setq j (1+ j)))) 
     (setq exithigh (cdr exithigh))
     (setq returnhigh (cdr returnhigh)))

(while (and exitlow returnlow) ;;Same for bottom
    (let* ((t1 (car exitlow))
          (t2 (car returnlow))
          (d1 (min 0 (/ (- (aref r t1) (aref r (- t1 (1- drange)))) (1- drange)))) ;;slope at exit
          (d2 (max 0 (/ (- (aref r (+ t2 (1- drange))) (aref r t2)) (1- drange)))) ;;slope at return
          (m (/ (+ d2 d1) (* (- t2 t1) (- t2 t1))))
          (b (- (/ d2 (- t2 t1)) (* m t2)))
	  (a (/ (+ (aref r t1) (aref r t2)) 2))
          (j (1+ t1)))
     (while (< j t2)
         (setf (aref r j) (+ (* (- t2 j) (/ (aref r t1) (- t2 t1))) (* (- j t1) (/ (aref r t2) (- t2 t1)))  (* (- j t1) (- j t2) (+ (* m j) b))))
         (setq j (1+ j))))
     (setq exitlow (cdr exitlow))
     (setq returnlow (cdr returnlow)))

r)

(if (arrayp s)
  (dotimes (j (length s))
	(setf (aref s j) (declip (aref s j))))
  (setq s (declip s)))

s
