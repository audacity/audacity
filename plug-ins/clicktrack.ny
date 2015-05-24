;nyquist plug-in
;version 4
;type generate
;categories "http://lv2plug.in/ns/lv2core#GeneratorPlugin"
;name "Click Track..."
;preview linear
;action "Generating Click Track..."
;info "For help, select one of two help screens in 'Action choice' below."
;author "Dominic Mazzoni"
;copyright "Released under terms of the GNU General Public License version 2"

;; by Dominic Mazzoni, modified by David R. Sky and Steve Daulton.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .
;; original clicktrack.ny by Dominic Mazzoni,
;; modified by David R. Sky and Steve Daulton.
;;
;; Updated to v4 by Steve Daulton May 2015
;; bug fixes and restructured by Steve Daulton Sept 2011.
;; string input verification added by Steve Daulton, 2009.
;; added click pitch [user request] and sound types fields September 2007 (D.R.Sky).
;; added optional total click track duration field [requested by Mike Mullins]
;; June 2009 (D.R.Sky).
;; added individual click duration field [requested by Quinto Milana]
;; June 2009 (D.R.Sky).
;;
;; Drip sound generator by Paul Beach,
;; used with permission.
;;
;; Thanks very much to Gale Andrews, who gave extensive visual feedback
;; and suggestions!

;control action "Action choice" choice "Generate track, help screen 1, help screen 2" 0
;control tempo "Tempo [beats per minute]" real "30 - 300 beats/minute" 120 30 300
;control sig "Beats per measure [bar]" int "1 - 20 beats/measure" 4 1 20
;control measures "Number of measures [bars]" int "1 - 1000 bars" 16 1 1000
;control click-track-dur "Optional click track duration [minutes seconds]" string "Whole numbers only" ""  
;control ticklen "Individual click duration [milliseconds]" int "1 - 100 ms" 10 1 100
;control offset "Start time offset [seconds]" real "0 - 30 seconds" 0 0 30
;control click-type "Click sound" choice "ping,noise,tick" 0
;control q "Noise click resonance - discernable pitch [q]" int "1 - 20" 1 1 20
;control high "MIDI pitch of strong click" int "18 - 116" 92 18 116
;control low "MIDI pitch of weak click" int "18 - 116" 80 18 116


(defun help1 ()
  (format nil
"Click Track Generator help - screen 1 of 2

Generates a click track at the selected tempo, beats per
measure, and either number of measures or track duration,
using selected click sound.

'Tempo': number of beats (clicks) per minute.

'Beats per measure (bar)': For example, 3/4 time means one
strong click then two others to form one bar, repeated
depending on 'number of measures' or 'click track duration'.

'Optional click track duration': If you enter a value into this
field, either [minutes seconds] (separated by a space), or
[seconds], the generated click track will be at or slightly
longer than this duration: the end of the track is extended
into a whole measure if the entered duration does not
produce this. Use whole numbers only.

If you enter a value into this field, the 'number of measures'
value will be ignored.

To generate click track or view help screen 2,
restart Click Track and select from 'Action choice'.")) ;end of help1


(defun help2 ()
  (format nil
"Click Track Generator help - screen 2 of 2

'Individual click duration': the duration of each individual
click, minimum of 1 millisecond (ms) to maximum of 100 ms.

'Start time offset': makes the click track start at a later
time than the very beginning (zero seconds), maximum
of 30 seconds.

'Click sound': choose between ping, noise or tick sound
for clicks.

'Noise click resonance': the higher this value, the more
clearly noise clicks have a tone.

'MIDI pitch of strong/weak click': MIDI values indicate\nwhat pitch to use. C-notes are:

24, 36, 48, 60 (middle C), 72, 84, 96, 108.
C# (C-sharp) above middle C is 61.

To generate click track or view help screen 1,
restart Click Track and select from 'Action choice'.")) ;end of help 2


;Check function: returns 1 on error
;min and max are allowable min and max values for arg
(defun check (arg min max)
  (if (and (>= arg min) (<= arg max)) 0 1))


;Function to convert a string into a list
(defun string-to-list (string)
  (read (make-string-input-stream (format nil "(~a)" string))))


(defun round-up (x)
  (if (> x (truncate x))
    (truncate (1+ x))
    (truncate x)))


(defun m-s-to-seconds (mslist)
  (if (= (length mslist) 1)
    ;just seconds
    (first mslist)
    ;minutes and seconds
    (+ (* 60 (first mslist))(second mslist))))


;Function to generate drip sound clicks
;code by Paul Beach www.proviewlandscape.com/liss/
;stretch-abs function makes this sound more like 'tick' sounds.
;limit hz to nyquist frequency.
(defun drip (p) ;p is pitch in hz
  (let* ((maxhz (/ *sound-srate* 2.1))
        (hz1 (min maxhz (* 2.40483  p)))
        (hz2 (min maxhz (* 5.52008  p)))
        (hz3 (min maxhz (* 8.653  p)))
        (hz4 (min maxhz (* 11.8  p))))
    (lp 
      (stretch-abs 1
        (mult (exp-dec 0 0.015 0.25) 
          (sim
            (mult (hzosc hz1) 0.5)
            (mult (hzosc hz2)  0.25)
            (mult (hzosc hz3)  0.125)
            (mult (hzosc hz4)  0.0625))))
    440)))


;Function used to normalize noise and tick clicks
;this function is necessary because filtering 
;changes amplitude of filtered noise clicks
(defun normalize (sound)
  (scale (/ (peak sound ny:all)) sound))


;; Error Checking
(defun errorcheck ()
  ; tempo
  (if (= (check tempo 30 300) 1)
    (setq error-msg (strcat error-msg (format nil
"Tempo ~a outside valid range 30 to 300 bpm
" tempo))))

  ;beats per measure
  (if (= (check sig 1 20) 1)
    (setq error-msg (strcat error-msg (format nil
"Beats per measure ~a outside valid range 1 to 20
" sig))))

  ;number of measures
  (if (= (check measures 1 1000) 1)
    (setq error-msg (strcat error-msg (format nil
"Number of measures ~a outside valid range 1 to 1000
" measures))))

  ;time start offset
  (if (= (check offset 0 30) 1)
    (setq error-msg (strcat error-msg (format nil
"Time offset ~a outside valid range 0 to 30 seconds
" offset))))

  ; q
  (if (= (check q 1 20) 1)
    (setq error-msg (strcat error-msg (format nil
"Filter quality q ~a outside valid range 1 to 20
" q))))

  ;high MIDI pitch
  (if (= (check high 18 116) 1)
    (setq error-msg (strcat error-msg (format nil
"High MIDI pitch ~a outside valid range 18 to 116
" high))))

  ;low MIDI pitch
  (if (= (check low 18 116) 1)
    (setq error-msg (strcat error-msg (format nil
"Low MIDI pitch ~a outside valid range 18 to 116
" low))))

  ;validate string
  (if (> (length m-s) 0) ;if at least one item
    (cond
      ((or 
        (> (length m-s) 2) ;there are more than 2 items or
        (not (integerp (first m-s))) ;first is not an integer or
        (and (= (length m-s) 2)(not (integerp (second m-s))))) ;second is not an integer
      (setq error-msg (strcat error-msg (format nil 
"If used, 'Optional click track duration' must be 
entered as either one number [seconds], or two 
numbers [minutes seconds] separated by a space.
Use whole numbers only.~%"))))
      ;one or two integers
      ((and 
          (= (length m-s) 1) ;one integer and
          (= (check (first m-s) 0 3660) 1)) ;outside valid range
        (setq error-msg (strcat error-msg (format nil 
"~a seconds is outside valid range 0 to 3660~%" (first m-s)))))
      ;one or two integers
      ((and
        (= (length m-s) 2) ;2 integers and
        (or
          (= (check (first m-s) 0 60) 1) ;1st is outside valid range or
          (= (check (second m-s) 0 59) 1))) ;2nd is outside valid range or
        (setq error-msg (strcat error-msg (format nil 
"~a is outside valid range 0 to [60 59]~%"
    m-s))))))) ;end of error checking


;Function to make click
(defun click (type accent)
  (setq pitch (if (= accent 1) high low))
  (setq amp (if (= accent 1) 0.75 0.5))
  (stretch-abs ticklen 
    ;pwl is used to add 5ms fade-in and fade-out of clicks
    (mult 
      (control-srate-abs *sound-srate*
        (pwl 0.005 amp 0.995 amp 1))
      (case type
        (0 (osc pitch)) ;ping click
        (1 (normalize (lowpass2 (noise 1) (step-to-hz pitch) q))) ;noise click
        (2 (normalize (drip (step-to-hz pitch)))))))) ;tick click
        

;Function to make one measure
(defun makemeasure ()
  (setf measure (click click-type 1)) ;accented click
  (dotimes (x (- sig 1))
    (setf measure (sim measure
      (at (* beatlen (+ x 1))                 
        (click click-type 0)))))) ;unaccented clicks
        

;Function to loop measures
(defun loop (bar number sig beatlen)
  (do* ((result bar) ;initialise result
        (count 1 (setq count (1+ count))))
        ((= count number) result) ;up to number of measures and return result
    (setq mtime (* count sig beatlen)) ;set time for next measure				
    (setf result (sim result (at mtime (cue bar))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
;view 1 of 2 help screens, or generate click track
(cond ;'master' cond
  ((= action 1)(help1)); display help screen 1
  ((= action 2)(help2)) ; display help screen 2
  (t ;Run Program
    (setq len (/ (* 60.0 *sound-srate* sig measures) tempo))
    (setq error-msg "") ;initialize error-msg	
    
    ;convert minutes-seconds string to a list
    ;for example, "4 30" becomes (4 30)
    (setf m-s (string-to-list click-track-dur))

    ;run error checks on input values
    (errorcheck)

    (if (= (length error-msg) 0) ;if no errors, generate Click Track
      (progn
        ; duration of 1 click, originally statically 0.01 s
        (setf ticklen (* (max 1 (min 100 ticklen)) 0.001))
        (setf beatlen (/ 60.0 tempo))
        
        ;call function to make one measure
        (makemeasure)
    
        ;calculate measures from text input (if used)
        (if m-s (setq measures 
          (/ (m-s-to-seconds m-s)(* sig beatlen))))

        ;if previewing, restrict number of measures
        (let ((preview (/ (get '*project* 'preview-duration)
                          (* sig beatlen)))) 
          (if (not (get '*track* 'view))  ;NIL if preview
              (setq measures (min preview measures))))

        ;round up number of measures
        (setq measures (round-up measures))

        ;loop measures
        (setf clicktrack (loop measure measures sig beatlen))
      
        ;add time offset and beatlen of silence to clicktrack,
        (seq (s-rest offset) ;offset
          (cue clicktrack) ;click track
          (s-rest(- beatlen ticklen)))) ;trailing silence
      ;Else error message
      (setq error-msg (strcat (format nil
"Error.~%You have entered at least one invalid value:~%~%") error-msg)))))
