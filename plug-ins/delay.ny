;nyquist plug-in
;version 3
;type process
;categories "http://lv2plug.in/ns/lv2core#DelayPlugin"
;name "Delay..."
;action "Performing Delay Effect..."
;info "by Roger Dannenberg, modified by David R. Sky\nReleased under terms of the GNU General Public License Version 2 \nDelay type: 'bouncing ball' makes the echoes occur increasingly close\ntogether (faster); 'reverse bouncing ball' makes them occur increasingly far\napart (slower). In either bouncing ball effect, delay time is not time between\nechoes but the * maximum * delay time in bounces.\nApplying delay can cause clipping (distortion), especially when reducing the\ndecay value and using more echoes. It's normally best to use Effect > Amplify\nto set the peak amplitude to -6 dB before using delay.     

;control delay-type "Delay type" choice "regular,bouncing ball,reverse bouncing ball" 0
;control decay "Decay amount [dB; negative value increases volume]" real "" 6 -1 24
;control delay "Delay time [seconds]" real "" 0.5 0 5
;control shift "Pitch change per echo [semitones; negative value = lower, 0 is off]" real "" 0 -2 2
;control count "Number of echoes" int "" 5 1 30

; Delay by Roger B. Dannenberg
; modified by David R. Sky October 2007, 
; adding negative decay values, which gives delay effect
; increasing volume with each delay; and adding
; bouncing ball and reverse bouncing ball delay effects
; and pitch [semitone] change with each echo.
; modified by Richard Ash January 2008, removing all 
; normalization functions. 

; Note by Roger Dannenberg: this effect will use up memory proportional to
; delay * count, since that many samples must be buffered
; before the first block is freed.

; initialize empty error message
(setf error-msg "")


; check function: returns 1 on error
(defun check (arg min max)
(if (and (>= arg min) (<= arg max))
0 1))


; checking for erroneous user-input values:
(setf error-msg (if (= (check decay -1 24) 0)
error-msg 
(strcat error-msg 
(format nil "Decay value '~a' outside valid range -1.0 to 24.0 dB. 
" decay))))

(setf error-msg (if (= (check delay 0 5) 0)
error-msg 
(strcat error-msg 
(format nil "Delay value '~a' outside valid range 0.0 to 5.0 seconds. 
" delay))))

(setf error-msg (if (= (check shift -2 2) 0)
error-msg 
(strcat error-msg 
(format nil "Pitch change value '~a' outside valid range -2.0 to 2.0 semitones. 
" shift))))

(setf error-msg (if (= (check count 1 30) 0)
error-msg 
(strcat error-msg 
(format nil "Number of echoes '~a' outside valid range 1 to 30 echoes. 
" count))))
; finished error-checking

; if error-msg is longer than 0 characters,
; prepend opening message
(setf error-msg (if (> (length error-msg) 0)
(strcat "Error -\n\nYou have input at least one invalid value:
" error-msg)
error-msg))

(cond ; 1
((> (length error-msg) 0)
(format nil "~a" error-msg))

(t ; no input errors, perform delay

; convert shift value to a value the Nyquist code understands
(setf shift (expt 0.5 (/ shift 12.0)))

; for bouncing ball effects, set delay time to delay time/count
(setf delay (if (> delay-type 0)
(/ delay count) delay))



; function to stretch audio 
(defun change (sound shift)
(if (arrayp sound)
(vector
(force-srate 44100 (stretch-abs shift (sound (aref sound 0))))
(force-srate 44100 (stretch-abs shift (sound (aref sound 1)))))
(force-srate 44100 (stretch-abs shift (sound sound)))))


(cond ; 2
((= delay-type 0) ; regular delay
; delay function
(defun delays (s decay delay count shift)
  (if (= count 0) (cue s)
	 (sim (cue s)
			(loud decay (at delay (delays (change s shift) 
decay delay (- count 1) shift))))))

(stretch-abs 1 (delays s (- 0 decay) delay count shift)))


((= delay-type 1) ; bouncing ball delay
; bouncing ball delay function
(defun bounces (s decay delay count shift)
  (if (= count 0) (cue s)
      (sim (cue s)
               (loud decay (at (mult delay count) 
(bounces (change s shift) decay delay 
(- count 1) shift)))))) 

(stretch-abs 1 (bounces s (- 0 decay) delay count shift)))


((= delay-type 2) ; reverse bouncing ball delay
; reverse bouncing ball delay function
(defun revbounces (s decay delay count revcount shift)
  (if (= count 0) (cue s)
      (sim (cue s)
               (loud decay (at (mult delay (- revcount count))
(revbounces (change s shift) decay delay 
(- count 1 ) revcount shift)))))) 

(setf revcount (1+ count))
(stretch-abs 1 (revbounces s (- 0 decay) delay count revcount shift)))
) ; end cond2, different delay effect choices
) ; end cond1 t
) ; end cond 1
