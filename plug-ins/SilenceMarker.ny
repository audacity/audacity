;nyquist plug-in
;version 1
;type analyze
;categories "http://lv2plug.in/ns/lv2core#AnalyserPlugin"
;name "Silence Finder..."
;action "Finding silence..."
;info "Adds point labels in areas of silence according to the specified\nlevel and duration of silence. If too many silences are detected,\nincrease the silence level and duration; if too few are detected,\nreduce the level and duration."
;author "Alex S. Brown"
;copyright "Released under terms of the GNU General Public License version 2"

;; by Alex S. Brown, PMP (http://www.alexsbrown.com)
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html

;control sil-lev "Treat audio below this level as silence [ -dB]" real "" 26 0 100
;control sil-dur "Minimum duration of silence [seconds]" real "" 1.0 0.1 5.0
;control labelbeforedur "Label placement [seconds before silence ends]" real "" 0.3 0.0 1.0

;Create a function to make the sum the two channels if they are stereo
(defun mono-s (s-in) (if (arrayp s-in) (snd-add (aref s-in 0) (aref s-in 1))
s-in))

;Create a function to reduce the sample rate and prepare the signal for
;analysis. RMS is good to monitor volume the way humans hear it, but is not
;available in Audacity. Used a peak-calculating function instead.
;NOTE: this is the place to add any processing to improve the quality of the
;signal. Noise filters could improve the quality of matches for noisy signals.
;PERFORMANCE vs. ACCURACY
;Reducing the samples per second should improve the performance and decrease
;the accuracy of the labels. Increasing the samples per second will do the
;opposite. The more samples checked, the longer it takes. The more samples
;checked, the more precisely the program can place the silence labels.
;my-srate-ratio determines the number of samples in my-s. Set the number after (snd-srate s)
;higher to increase the number of samples.

(defun my-s (s-in)
 (setq my-srate-ratio (truncate (/ (snd-srate (mono-s s-in)) 100)))
 (snd-avg (mono-s s-in) my-srate-ratio my-srate-ratio OP-PEAK)
)

;Set the silence threshold level (convert it to a linear form)
(setq thres (db-to-linear (* -1 sil-lev)))
;Store the sample rate of the sound
(setq s1-srate (snd-srate (my-s s)))
;Initialize the variable that will hold the length of the sound.
;Do not calculate it now with snd-length, because it would waste memory.
;We will calculate it later.
(setq s1-length 0)
;Initialize the silence counter and the labels variable
(setq sil-c 0)
(setq l NIL)
;Convert the silence duration in seconds to a length in samples
(setq sil-length (* sil-dur s1-srate))

;Define a function to add new items to the list of labels
(defun add-label (l-time l-text)
 (setq l (cons (list l-time l-text) l))
)

;The main working part of the program, it counts
;the number of sequential samples with volume under
;the threshold. It adds to a list of markers ever time
;there is a longer period of silence than the silence
;duration amount.

;It runs through a loop, adding to the list of markers (l)
;each time it finds silence.
(let (s1) ;Define s1 as a local variable to allow efficient memory use
 ; Get the sample into s1, then free s to save memory
 (setq s1 (my-s s))
 (setq s nil)
 ;Capture the result of this "do" loop, because we need the sountd's legnth
 ;in samples.
 (setq s1-length
  ;Keep repeating, incrementing the counter and getting another sample
  ;each time through the loop.
  (do ((n 1 (+ n 1)) (v (snd-fetch s1) (setq v (snd-fetch s1))))
   ;Exit when we run out of samples (v is nil) and return the number of
   ;samples processed (n)
   ((not v) n)
   ;Start the execution part of the do loop
   ;if found silence, increment the silence counter
   (if (< v thres) (setq sil-c (+ sil-c 1)))

   ;If this sample is NOT silent and the previous samples were silent
   ;then mark the passage.
   (if (and (> v thres) (> sil-c sil-length))
 	  ;Mark the user-set number of seconds BEFORE this point to avoid clipping the start
 	  ;of the material.
    (add-label (- (/ n s1-srate) labelbeforedur) "S")
   )
   ;If this sample is NOT silent, then reset the silence counter
   (if (> v thres)
    (setq sil-c 0)
   )
  )
 )
)

;Check for a long period of silence at the end
;of the sample. If so, then mark it.
(if (> sil-c sil-length)
 ;If found, add a label
 ;Label time is the time the silence began plus the silence duration target
 ;amount. We calculate the time the silence began as the end-time minus the
 ;final value of the silence counter
 (add-label (+ (/ (- s1-length sil-c) s1-srate) sil-dur) "S")
)

;If no silence markers were found, return a message
(if (null l)
 (setq l "No silences found. Try reducing the silence\nlevel and minimum silence duration.")
)
l
