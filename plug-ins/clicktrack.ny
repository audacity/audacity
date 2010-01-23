;nyquist plug-in
;version 3
;type generate
;categories "http://lv2plug.in/ns/lv2core#GeneratorPlugin"
;name "Click Track..."
;action "Generating Click Track..."
;info "by Dominic Mazzoni, modified by David R. Sky\nReleased under terms of the GNU General Public License version 2\nFor help, select one of two help screens in 'Action choice' below."

;control action "Action choice" choice "Generate track, help screen 1, help screen 2" 0
;control tempo "Tempo [beats per minute]" int "30 - 300 beats/minute" 120 30 300
;control sig "Beats per measure [bar]" int "1 - 20 beats/measure" 4 1 20
;control measures "Number of measures [bars]" int "1 - 1000 bars" 16 1 1000
;control click-track-dur "Optional click track duration [minutes seconds]" string "Whole numbers only" ""  
;control ticklen "Individual click duration [milliseconds]" int "1 - 100 ms" 10 1 100
;control offset "Start time offset [seconds]" real "0 - 30 seconds" 0 0 30
;control click-type "Click sound" choice "ping,noise,tick" 0
;control q "Noise click resonance - discernable pitch [q]" int "1 - 20" 1 1 20
;control high "MIDI pitch of strong click" int "18 - 116" 92 18 116
;control low "MIDI pitch of weak click" int "18 - 116" 80 18 116

; original clicktrack.ny by Dominic Mazzoni,
; modified by David R. Sky: 
; string input verification added by Steve Daulton, 2009.
; added click pitch [user request] and sound types fields September 2007
; added optional total click track duration field [requested by Mike Mullins] 
; June 2009
; added individual click duration field [requested by Quinto Milana]
; June 2009]
; original code kept 'as is'.
; now includes:
; choice between click sounds [ping {sinewave}, noise or tick],
; user-set MIDI pitch values for strong and weak clicks,
; resonance of noise clicks 
; [higher resonance gives them more discernable pitch],
; time offset for start of click track,
; and error-checking code to generate error message
; for such things as negative value inputs
; Drip sound generator by Paul Beach,
; used with permission.
;
; Thanks very much to Gale Andrews, who gave extensive visual feedback
; and suggestions!


; view 1 of 2 help screens, or generate click track
(cond ; 'master' cond
((= action 1) ; display help screen 1
(format nil
"Click Track Generator help - screen 1 of 2

Generates a click track at the selected tempo, beats per\nmeasure, and either number of measures or track duration,\nusing selected click sound.

'Tempo': number of beats (clicks) per minute.

'Beats per measure (bar)': For example, 3/4 time means one\nstrong click then two others to form one bar, repeated\ndepending on 'number of measures' or 'click track duration'.

'Optional click track duration': If you enter a value into this\nfield, either [minutes seconds] (separated by a space), or\n[seconds], the generated click track will be at or slightly\nlonger than this duration: the end of the track is extended\ninto a whole measure if the entered duration does not\nproduce this. Use whole numbers only.

If you enter a value into this field, the 'number of measures'\nvalue will be ignored.

      To generate click track or view help screen 2,\n      restart Click Track and select from 'Action choice'.") ; end format
) ; end display help screen 1

((= action 2) ; display help screen 2
(format nil
"Click Track Generator help - screen 2 of 2

'Individual click duration': the duration of each individual\nclick, minimum of 1 millisecond (ms) to maximum of 100 ms.

'Start time offset': makes the click track start at a later\ntime than the very beginning (zero seconds), maximum\nof 30 seconds.

'Click sound': choose between ping, noise or tick sound\nfor clicks.

'Noise click resonance': the higher this value, the more\nclearly noise clicks have a tone.

'MIDI pitch of strong/weak click': MIDI values indicate\nwhat pitch to use. C-notes are:

24, 36, 48, 60 (middle C), 72, 84, 96, 108.\nC# (C-sharp) above middle C is 61.

      To generate click track or view help screen 1,\n      restart Click Track and select from 'Action choice'.") ; end format
) ; end display help screen 2

(t ; perform clicktrack.ny
(setf click-type (+ click-type 1))


; check function: returns 1 on error
; min and max are allowable min and max values for arg
(defun check (arg min max)
(if (and (>= arg min) (<= arg max))
0 1))


; function to convert a string into a list
(defun string-to-list (string)
(read (make-string-input-stream (format nil "(~a)" string))))


; convert minutes-seconds string to a list
; for example, "4 30" becomes (4 30)
(setf m-s (string-to-list click-track-dur)) 


; initialize blank error-msg
(setf error-msg "")

; input values error checks

; tempo
(setf error-msg (if 
(= (check tempo 30 300) 0)
error-msg
(strcat error-msg (format nil
"Tempo ~a outside valid range 30 to 300 bpm
" tempo))))
; beats per measure
(setf error-msg (if
(= (check sig 1 20) 0)
error-msg
(strcat error-msg (format nil
"Beats per measure ~a outside valid range 1 to 20
" sig))))
; number of measures
(setf error-msg (if
(= (check measures 1 1000) 0)
error-msg
(strcat error-msg (format nil
"Number of measures ~a outside valid range 1 to 1000
" measures))))
; time start offset
(setf error-msg (if
(= (check offset 0 30) 0)
error-msg
(strcat error-msg (format nil
"Time offset ~a outside valid range 0 to 30 seconds
" offset))))
; q
(setf error-msg (if
(= (check q 1 20) 0)
error-msg
(strcat error-msg (format nil
"Filter quality q ~a outside valid range 1 to 20
" q))))
; high MIDI pitch
(setf error-msg (if
(= (check high 18 116) 0)
error-msg
(strcat error-msg (format nil
"High MIDI pitch ~a outside valid range 18 to 116
" high))))
; low MIDI pitch
(setf error-msg (if
(= (check low 18 116) 0)
error-msg
(strcat error-msg (format nil
"Low MIDI pitch ~a outside valid range 18 to 116
" low))))

; validate string
(if (not (null m-s)) ; don't test if not set
   (if (= (length m-s) 1) ; if there is only one item
      (setf error-msg (if 
      (integerp (first m-s)) ; first is number
   error-msg
   (strcat error-msg (format nil 
"If used, 'Optional click track duration' must be 
entered as either one number [seconds], or two 
numbers [minutes seconds] separated by a space.
Use whole numbers only.
"))))
; else if there is more than one item
   (setf error-msg (if (and
      (<= (length m-s) 2) ; there are no more than 2 items and
      (integerp (first m-s)) ; first is number
      (integerp (second m-s))) ; second is number
      error-msg
      (strcat error-msg (format nil 
"If used, 'Optional click track duration' must be 
entered as either one number [seconds], or two 
numbers [minutes seconds] separated by a space.
Use whole numbers only.
"))))))

; optional click track length
; one number entered
(if (and (integerp (first m-s))(= (length m-s) 1))
   (setf error-msg (if 
   (= (check (first m-s) 0 3660) 0)
   error-msg
   (strcat error-msg (format nil 
"~a seconds is outside valid range 0 to 3660"
(first m-s))))))
; two numbers entered
(if (and 
(integerp (first m-s))
(integerp (second m-s))
(= (length m-s) 2))
   (setf error-msg (if (and
   (= (check (first m-s) 0 60) 0)
   (= (check (second m-s) 0 59) 0))
   error-msg
   (strcat error-msg (format nil 
"~a is outside valid range 0 to [60 59]"
m-s)))))


(cond
; if error-msg is not blank, give error msg
((> (length error-msg) 0)
(setf error-msg (strcat (format nil
"Error - \n\nYou have entered at least one invalid value:\n
") error-msg))) ; end error msg

; no error so generate click track
(t
; duration of 1 click, originally statically 0.01 s
(setf ticklen (* (max 1 (min 100 ticklen)) 0.001))
(setf beatlen (/ 60.0 tempo))


; function to generate drip sound clicks
; code by Paul Beach www.proviewlandscape.com/liss/
; stretch-abs function makes this sound more like 'tick' sounds
(defun drip (p) ; p is pitch in hz
(lp 
(stretch 1
(mult (exp-dec 0 0.015 0.25) 
( sim
(mult (hzosc (*  2.40483  p))  0.5 )
(mult (hzosc (*  5.52008  p))  0.25 )
(mult (hzosc (* 8.653  p))  0.125 )
(mult (hzosc (* 11.8  p))  0.0625 )
)
)
) 
440))


; function used to normalize noise and tick clicks
; this function is necessary because filtering 
; changes amplitude of filtered noise clicks
(defun normalize (sound)
(setf peak-level (peak sound ny:all))
(scale (/ 1.0 peak-level) sound))


; make one measure
(setf measure (stretch-abs ticklen (mult 0.75 
; pwl is used to add fast [5ms] fade-in and fade-out of clicks
(pwl 0 0 0.005 1 0.995 1 1 0 1)
(cond
((= click-type 1) ; ping accented clicks
(osc high))
((= click-type 2) ; noise accented clicks
(normalize (lowpass2 (noise 1) (step-to-hz high) q)))
((= click-type 3) ; tick accented clicks
(normalize (drip (step-to-hz high)))) ))))
(dotimes (x (- sig 1))
  (setf measure (sim measure
                     (at (* beatlen (+ x 1))                 
                         (stretch-abs ticklen (mult 0.5 
; again, pwl adds 5ms fade-in and fade-out to clicks
(pwl 0 0 0.005 1 0.995 1 1 0 1)
(cond
((= click-type 1) ;ping tone unaccented clicks
(osc low))
((= click-type 2) ; noise unaccented clicks
(normalize (lowpass2 (noise 1) (step-to-hz low) q)))
((= click-type 3) ; tick unaccented clicks
(normalize (drip (step-to-hz low)))) )))))))
; make the measure exactly the right length
(setf measure (sim measure
                   (stretch-abs (* sig beatlen) (const 0.0))))


; convert click-track-dur to number of measures,
; otherwise use user's measures value 
(setf measures (if (null m-s)
measures
(/
(if ;2
(= (length m-s) 1)
(first m-s) ; just seconds
(+ (* 60 (first m-s)) (second m-s) ; minutes and seconds
) ; end +
) ; end if2
(* sig beatlen)) ; end /
) ; end if
) ; end setf measures


; round up to next whole number of measures only if 
; measures > (truncate measures)
(setf measures (if 
(> measures (truncate measures))
(truncate (1+ measures))
(truncate measures)
)) ; end if, setf measures


; loop measure n [measures-1] times
(setf result measure)
(dotimes (x (- measures 1))
  (setf result (seq result measure)))
; add time offset to result,
; if offset > 0 seconds
(setf result (if (= offset 0) result
(sim (s-rest offset) (at-abs offset (cue result)))))

; return [click track] result
result

) ; end t
) ; end cond

) ; end t perform clicktrack.ny
) ; end 'master' cond
