;; generate-defines.lsp
;;
;; Roger B. Dannenberg
;; July 2012
;;
;; 


;; GENERATE-DEFINES -- generate INTERP_xxx defines for sound.h
;;
;; n is number of parameters
;;
(defun generate-defines (n)
  (let ((count (int-power 2 n))
        sum code pos)
    (dotimes (i count)
      (format t "#define INTERP_")
      (setf sum 0)
      (dotimes (j n)
        (setf pos (- n 1 j)) ; bit position
        (cond ((testbit i pos)
               (setf code "s")
               (setf sum (+ sum (int-power 4 pos))))
              (t
               (setf code "n")))
        (format t "~A" code))
      (format t " ~A~%" sum))))


(defun get-int-field (n pos len)
;; from integer n, extract len bits starting at pos,
;; e.g. (get-int-field 0x1234 8 4) -> 2
;; pos for low-order bit is 0; the low order bit returned
;; is at pos; the high-order bit is at pos + len - 1
  (setf n (/ n (int-power 2 pos)))
  (setf n (rem n (int-power 2 len)))
  n)


(defun generate-full-defines (n)
  (let ((count (int-power 4 n))
        code pos fld)
    (dotimes (i count)
      (format t "#define INTERP_")
      (setf sum 0)
      (dotimes (j n)
        (setf pos (* 2 (- n 1 j))) ; bit position
        (setf fld (get-int-field i pos 2))
        (setf code (char "nsir" fld))
        (format t "~A" code))
      (format t " ~A~%" i))))


(defun int-power (i exponent) (truncate (+ 0.5 (power i exponent))))
(defun testbit (n pos) (/= 0 (logand n (int-power 2 pos))))

(generate-full-defines 4)
