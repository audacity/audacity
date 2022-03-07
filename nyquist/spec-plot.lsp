;; spec-plot.lsp -- spectral plot function
;;
;; Roger B. Dannenberg, May 2016
;;

(setf *spec-plot-bw* 8000.0) ;; highest frequency to plot (default)
(setf *spec-plot-res* 20.0) ;; bin size (default)
(setf *spec-plot-db* nil) ;; plot dB? (default)

;; We want to allow round-number bin-sizes so plot will be more readable
;; Assuming 20Hz as an example, the FFT size would have to be
;; 44100/20 = 2205, but that's not a power of 2, so we should resample
;; the signal down so that the FFT size is 2048 (or up to 4096). This
;; would result in sample rates of 2048*20 = 40960 or 81120. We should
;; pick the smaller one if it is at least 2x *spec-plot-bw*.

(defun spec-plot (sound &optional offset &key (res *spec-plot-res*)
                                              (bw *spec-plot-bw*)
                                              (db *spec-plot-db*))
  (ny:typecheck (not (soundp sound))
    (ny:error "SPEC-PLOT" 1 '((SOUND) nil) sound))
  (ny:typecheck (not (or (null offset) (numberp offset)))
    (ny:error "SPEC-PLOT" 2 '((NUMBER NULL) nil) offset))
  (let (newsr sa fft-size power2)
    (setf fft-size (/ (snd-srate sound) res))
    (setf power2 8) ;; find integer size for FFT
    (while (< power2 fft-size)
      (setf power2 (* 2 power2)))
    ;; now power2 >= fft-size
    (cond ((> power2 fft-size) ;; not equal, must resample
           ;; if half power2 * res is above 2 * bw,
           ;; use half power2 as fft size
           (cond ((> (* power2 res) (* 4 bw))
                  (setf power2 (/ power2 2))))
           (setf sound (snd-resample sound (* power2 res)))
           (setf fft-size power2)))
    ;; we only need fft-dur samples, but allow an extra second just to
    ;; avoid any rounding errors
    (if offset
        (setf sound (extract offset (+ 1.0 offset (/ (snd-srate sound)
                                                     fft-size)) sound)))
    (setf sa (sa-init :resolution res :input sound))
    (setf mag (sa-magnitude (sa-next sa)))
    (setf mag (snd-from-array 0 (/ 1.0 res) mag))
    (if db (setf mag (linear-to-db mag)))
    (s-plot mag bw (round (/ (float bw) res)))))
            
