; 6 -32 8 -32 pumped too much noise, picked up student answer too
; 3 -30 4 -30 pumped too much noise too
(setf m (compress-map 2 -12 2 -24 :limit t :transition 2))
(s-save (scale 0.005 m) ny:all "map.wav")

(defun t1 () (print (s-save (clip (let (y)
  (setf y (compress (s-read "c:\\rbd\\garlan.aif") m 0.1 0.1))
  (setf y (agc y 6.0 2.0 2.0))
  y) 1.0) ny:all "compress.wav" :bits 8)))

(defun t2 () (print (s-save (clip (let (y)
  (setf y (compress (s-read "denoise.wav") m 0.1 0.1))
  (setf y (agc y 6.0 2.0 2.0))
  y) 1.0) ny:all "compden8.wav" :bits 8)))

;(print (play (clip (scale 1.0 (compress (s-read "c:\\rbd\\garlan.aif") m 0.1 0.1)) 1.0)))
;(print (play (clip (agc (s-read "c:\\rbd\\garlan.aif") 6.0 2.0 2.0) 1.0)))
;(setf sil (s-read "..\\..\\garlan.aif" :time-offset 7.655 :dur 1.165))
;(setf soft (s-read "..\\..\\garlan.aif" :time-offset 15.64 :dur .11))

; (play (compress sil m 0.1 0.1))

; (s-save (snd-oneshot (s-read ".\\orig.wav") 0.990 0.1) ny:all "oneshot.wav")

(defun square (x) (* x x))

;; region for low-pass will be *soften-width* wide, with
;; *soften-crossfade* seconds of cross-fade
(setf *soften-width* 0.02)
(setf *soften-crossfade* 0.002)

(defun soften-clipping (snd)
  (let (clip-region)
    (setf clip-region (snd-oneshot (prod snd snd) 
                    (square (/ 126.0 127.0)) *soften-width*))
    (setf clip-region (snd-chase clip-region 
                     *soften-crossfade* *soften-crossfade*))
    (setf snd (seq (s-rest 0.01) (cue (scale 0.99 snd))))
    ; (vector (prod snd clip-region) snd)
    (prod snd clip-region)	    
  ))

(sound-off)
    
(defun tes ()
  (let (snd)
    (setf snd (s-read "..\\..\\intro.aif"))
    (play (soften-clipping snd))))

(tes)





