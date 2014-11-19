;nyquist plug-in
;version 1
;type analyze
;categories "http://lv2plug.in/ns/lv2core#AnalyserPlugin"
;name "Sound Finder..."
;action "Finding sound..."
;info "Adds region labels for areas of sound according to the specified level\nand duration of surrounding silence. If too many labels are produced,\nincrease the silence level and duration; if too few are produced,\nreduce the level and duration."
;author "Jeremy R. Brown"
;copyright "Released under terms of the GNU General Public License version 2"

;; by Jeremy R. Brown (http://www.jeremy-brown.com/)
;; based on the Silence Finder script by Alex S. Brown (http://www.alexsbrown.com)
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html

;control sil-lev "Treat audio below this level as silence [ -dB]" real "" 26 0 100
;control sil-dur "Minimum duration of silence between sounds [seconds]" real "" 1.0 0.1 5.0
;control labelbeforedur "Label starting point [seconds before sound starts]" real "" 0.1 0.0 1.0
;control labelafterdur "Label ending point [seconds after sound ends]" real "" 0.1 0.0 1.0
;control finallabel "Add a label at the end of the track? [No=0, Yes=1]" int "" 0 0 1

;30Dec09: couple of changes made to default control values by Gale Andrews 

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
;Initialize the silence counter
(setq sil-c 0)
;Initialize the labels variable
(setq l NIL)
;Convert the silence duration in seconds to a length in samples
(setq sil-length (* sil-dur s1-srate))

;Set the sound-start marker to -1, indicating no sound has been found yet
(setq sound-start -1)
(setq silence-start -1)
;Set the flag that says we are looking for the start of a sound (as opposed to the start of a silence)
(setq sound-search 1)
;Set the counter that counts sounds
(setq sound-count 0)

(setq label-string "")

;Define a function to add new items to the list of labels
(defun add-label (l-starttime l-endtime l-text)
 (setq l (cons (list l-starttime l-endtime l-text) l))
)

;The main working part of the program, it counts
;the number of sequential samples with volume under
;the threshold. It adds to a list of markers every time
;there is a longer period of silence than the silence
;duration amount.

;It runs through a loop, adding to the list of markers (l)
;each time it finds silence.
(let (s1) ;Define s1 as a local variable to allow efficient memory use
 ; Get the sample into s1, then free s to save memory
 (setq s1 (my-s s))
 (setq s nil)
 ;Capture the result of this "do" loop, because we need the sound's length
 ;in samples.
 (setq s1-length
  ;Keep repeating, incrementing the counter and getting another sample
  ;each time through the loop.
  (do ((n 1 (+ n 1)) (v (snd-fetch s1) (setq v (snd-fetch s1))))
   ;Exit when we run out of samples (v is nil) and return the number of
   ;samples processed (n)
   ((not v) n)
   ;Start the execution part of the do loop
   
   ;if found silence, increment the silence counter; if silence-start is not already > -1, set the start of silence to the current sample number (n)
   (if (< v thres) 
      (progn
	     (setq sil-c (+ sil-c 1))
		 (if (= silence-start -1) (setq silence-start n))
      )
   )
   
   ;if found sound, and sound-search is 1, mark the start of the sound and change sound-search to 0 (look for silence next)
   (if (and (>= v thres) (= sound-search 1))
      (progn
         (setq sound-search 0)
	     (setq sound-start n)
	     (setq sound-count (1+ sound-count))
      )
   )

   ;if found silence, and silence-counter is long enough, and sound-search is 0, and sound-start is not -1, and silence-start is not -1, that indicates the end of a sound (for which we have already found the beginning), which we should now label
   (if (and (< v thres) (= sound-search 0) (/= sound-start -1) (> sil-c sil-length) (/= silence-start -1))
      (progn
         (setq sound-search 1)
		 (setq sil-c 0)
         ;Create the label text
         (setq label-string (strcat label-string (format nil "~A" sound-count)))
         (add-label (- (/ sound-start s1-srate) labelbeforedur) (+ (/ silence-start s1-srate) labelafterdur) label-string)
		 (setq label-string "")
		 (setq silence-start -1)
      )
   )
   
   ;if found sound, reset the silence-counter and silence-start
   (if (>= v thres) 
      (progn
	     (setq sil-c 0)
		 (setq silence-start -1)
      )
   )
  )
 )
)

;if we're still looking for the end of a sound at the end of the file, end the sound at the end of the file
(if (= sound-search 0)
   (progn
      (setq label-string (strcat label-string (format nil "~A" sound-count)))
      (add-label (- (/ sound-start s1-srate) labelbeforedur) (/ s1-length s1-srate) label-string)
   )
)

;If no sound markers were found, return a message
;Otherwise, if some sounds were found, also optionally place a label at the end of the file.
(if (null l)
 (setq l "No sounds found. Try reducing the silence\nlevel and minimum silence duration.")
 (if (= finallabel 1) (add-label (/ s1-length s1-srate) (/ s1-length s1-srate) "[End]"))
)
l
