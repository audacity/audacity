;; nyqmisc.lsp -- misc functions for nyquist

(init-global *snd-display-max-samples* 10000)
(init-global *snd-display-print-samples* 100)


; (snd-display sound) -- describe a sound
(defun snd-display (sound)
  (let (t0 srate len extent dur samples)
    (setf srate (snd-srate sound))
    (setf t0 (snd-t0 sound))
    (setf len (snd-length sound *snd-display-max-samples*))
    (cond ((= len *snd-display-max-samples*)
                 (setf extent (format nil ">~A" (+ t0 (* srate *snd-display-max-samples*))))
           (setf dur (format nil ">~A" (* srate *snd-display-max-samples*))))
          (t
           (setf extent (cadr (snd-extent sound *snd-display-max-samples*)))
           (setf dur (/ (snd-length sound *snd-display-max-samples*) srate))))
    (cond ((> len 100)
           (setf samples (format nil "1st ~A samples" *snd-display-print-samples*))
           (setf nsamples *snd-display-print-samples*))
          (t
           (setf samples (format nil "~A samples" len))
           (setf nsamples len)))
    (format t "~A: srate ~A, t0 ~A, extent ~A, dur ~A, ~A: ~A"
      sound srate t0 extent dur samples (snd-samples sound nsamples))))

