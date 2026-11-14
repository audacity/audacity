;; velocity.lsp -- conversion routines for MIDI velocity
;;
;; Roger B. Dannenberg
;; July, 2012


(defun db-to-vel (x &optional float)
  (linear-to-vel (db-to-linear x) float))


(defun linear-to-vel (x &optional float)
  (setf x (/ (- (sqrt (abs x)) 0.0239372) 0.00768553))
  (cond (float x)
        (t
         (setf x (round x))
         (max 1 (min 127 x)))))


(defun vel-to-db (v)
  (linear-to-db (vel-to-linear v)))


(defun vel-to-linear (v)
  (power (+ (* v 0.00768553) 0.0239372) 2))
