;nyquist plug-in
;version 4
;type generate
;categories "http://lv2plug.in/ns/lv2core#GeneratorPlugin"
;name "Rhythm Track..."
;preview linear
;action "Generating Rhythm..."
;author "Dominic Mazzoni"
;copyright "Released under terms of the GNU General Public License version 2"

;; by Dominic Mazzoni, David R. Sky and Steve Daulton.
;; Drip sound generator by Paul Beach
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .


;control action "Action choice" choice "Generate track, Help screen 1, Help screen 2" 0
;control tempo "Tempo (beats per minute)" real "30 - 300 beats/minute" 120 30 300
;control timesig "Beats per measure (bar)" int "1 - 20 beats/measure" 4 1 20
;control swing "Swing amount" float "+/- 1" 0 -1 1
;control measures "Number of measures (bars)" int "1 - 1000 bars" 16 1 1000
;control click-track-dur "Optional rhythm track duration (minutes seconds)" string "Whole numbers only" ""  

;;control ticklen "Individual beat duration (milliseconds)" int "1 - 100 ms" 10 1 100

;control offset "Start time offset (seconds)" real "0 - 30 seconds" 0 0 30
;control click-type "Beat sound" choice "Metronome tick,Ping,Cowbell,Resonant noise,Noise click,Drip" 0

;;control q "Noise click resonance - discernable pitch (q)" int "1 - 20" 1 1 20

;control high "MIDI pitch of strong beat" int "18 - 116" 84 18 116
;control low "MIDI pitch of weak beat" int "18 - 116" 80 18 116

;; allow q control to be commented out.
(if (not (boundp 'q))
    (setf q 10))

;; allow ticklen control to be commented out.
(if (not (boundp 'ticklen))
    (setf ticklen 10))

;; TODO: Hard code tick length (long and short versions
;; TODO: add drum sounds


(defun help1 ()
  (format nil
"Rhythm Track Generator help - screen 1 of 2

Generates a rhythm track at the selected tempo, beats per
measure, and either number of measures or track duration,
using the selected sound.

'Tempo': number of beats (clicks) per minute.

'Beats per measure (bar)': For example, 3/4 time means one
strong beat then two others to form one bar, repeated
depending on 'number of measures' or 'rhythm track duration'.

'Swing amount': When set to a non-zero amount, alternate
beats are delayed or advanced to give a swing feel. At
maximum / minimum settings the rhythm plays with triplet timing.

'Optional rhythm track duration': If you enter a value into this
field, either (minutes seconds - separated by a space), or
(seconds), the generated rhythm track will be at or slightly
longer than this duration: the end of the track is extended
into a whole measure if the entered duration does not
produce this. Use whole numbers only.

If you enter a value into this field, the 'number of measures'
value will be ignored.

To generate rhythm track or view help screen 2,
restart Rhythm Track and select from 'Action choice'.")) ;end of help1


(defun help2 ()
  (format nil
"Rhythm Track Generator help - screen 2 of 2

'Start time offset': makes the rhythm track start at a later
time than the very beginning (zero seconds), maximum
of 30 seconds.

'Beat sound': choose which sound to use for the beats.

'MIDI pitch of strong/weak beat': MIDI values indicate
what pitch to use. C-notes are:

24, 36, 48, 60 (middle C), 72, 84, 96, 108.
C# (C-sharp) above middle C is 61.

To generate rhythm track or view help screen 1,
restart Rhythm Track and select from 'Action choice'.")) ;end of help 2


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
  (if (= (check timesig 1 20) 1)
    (setq error-msg (strcat error-msg (format nil
"Beats per measure ~a outside valid range 1 to 20
" timesig))))

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
"If used, 'Optional rhythm track duration' must be 
entered as either one number (seconds0, or two 
numbers (minutes seconds) separated by a space.
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
"~a is outside valid range 0 to (60 59)~%"
    m-s))))))) ;end of error checking

(defun metronome-tick (hz peak)
  (let* ((ln 300)
         (sig-array (make-array ln))
         (x 1))
    ;; generate some 'predictable' white noise
    (dotimes (i ln)
      (setf x (rem (* 479 x) 997))
      (setf (aref sig-array i) (- (/ x 500.0) 1)))
    (setf sig (sim (s-rest-abs 0.2)
                   (snd-from-array 0 44100 sig-array)))
    (setf sig
      (mult (abs-env (pwev 10 (/ ln 44100.0) 2 1 0))
            (highpass8  (lowpass2 sig (* 2 hz) 6)
                        hz)))
    (let ((gain (/ (peak sig 300))))
      ; The '1.11' factor makes up for gain reduction in 'resample'
      (mult (abs-env (pwlv 1.11 0.02 1.11 0.05 0 ))
        (jcrev (mult peak gain sig) 0.01 0.1)))))

(defun s-rest-abs (d)
  (abs-env (s-rest d)))

(defun cowbell (hz)
  (sim
    (mult (pwev 0.3 0.8 0.0005)
          (hzosc hz *tri-table*)
          (hzosc (* hz 3.46) *tri-table*))
    (mult (pwev 0.7 0.2 0.01)
          (hzosc (* hz 7.3) *tri-table*)
          (hzosc (* hz 1.52) *tri-table*))))


(defun get-metronome-tick (hz gain)
  (resample
    (sound-srate-abs 44100 (metronome-tick hz gain))
    *sound-srate*))

(defun get-ping (pitch)
  (stretch-abs ticklen
    (mult
      (control-srate-abs *sound-srate* (pwl 0.005 amp 0.995 amp 1))
      (osc pitch))))

(defun get-resonant-noise (pitch)
  (stretch-abs 0.05 ; 50 milliseconds
    (mult
      (control-srate-abs *sound-srate* (pwl 0.05 amp 0.95 amp 1))
      (normalize (lowpass2 (noise 1) (step-to-hz pitch) 20)))))

(defun get-noise-click (pitch)
  (stretch-abs 0.005
    (mult
      (control-srate-abs *sound-srate* (pwl 0.005 amp 0.995 amp 1))
      (normalize (lowpass2 (noise 1) (step-to-hz pitch) 2)))))

(defun get-drip (pitch)
  (stretch-abs ticklen
    (mult
      (control-srate-abs *sound-srate* (pwl 0.005 amp 0.995 amp 1))
      (normalize (drip (step-to-hz pitch))))))

(defun get-cowbell (pitch)
  (mult 0.8 (cowbell (step-to-hz pitch))))

;Function to make click
(defun click (type accent)
  (setq pitch (if (= accent 1) high low))
  (setq amp (if (= accent 1) 0.75 0.5))
  (case type
    (0 (get-metronome-tick (step-to-hz pitch) amp))
    (1 (get-ping pitch))
    (2 (get-cowbell pitch))
    (3 (get-resonant-noise pitch))
    (4 (get-noise-click pitch))
    (t (get-drip pitch))))


(defun swing-adjust (i val)
  (* val (/ 3.0) (rem (1+ i) 2)))

;Function to make one measure and save it in the global *measure*
(defun makemeasure ()
  (setf *measure*
    (sim
      (s-rest (* timesig beatlen)) ;required for trailing silence
      (click click-type 1) ;accented beat
      (simrep (x (- timesig 1))
        (at-abs (* beatlen (+ x 1 (swing-adjust x swing)))
            (cue (click click-type 0))))))) ;unaccented beat
        

(defun make-click-track (measures mdur)
  (setf *measure* (set-logical-stop (cue *measure*) (* timesig beatlen)))
  (seqrep (i measures) (cue *measure*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
;view 1 of 2 help screens, or generate rhythm track
(cond ;'master' cond
  ((= action 1)(help1)); display help screen 1
  ((= action 2)(help2)) ; display help screen 2
  (t ;Run Program
    (setq len (/ (* 60.0 *sound-srate* timesig measures) tempo))
    (setq error-msg "") ;initialize error-msg	
    
    ;convert minutes-seconds string to a list
    ;for example, "4 30" becomes (4 30)
    (setf m-s (string-to-list click-track-dur))

    ;run error checks on input values
    (errorcheck)

    (if (= (length error-msg) 0) ;if no errors, generate Rhythm Track
      (progn
        ; duration of 1 beat, originally statically 0.01 s
        (setf ticklen (* (max 1 (min 100 ticklen)) 0.001))
        (setf beatlen (/ 60.0 tempo))
        
        ;call function to make one measure
        (makemeasure)
    
        ;calculate measures from text input (if used)
        (if m-s (setq measures 
          (/ (m-s-to-seconds m-s)(* timesig beatlen))))

        ;if previewing, restrict number of measures
        (let ((preview (/ (get '*project* 'preview-duration)
                          (* timesig beatlen))))
          (if (not (get '*track* 'view))  ;NIL if preview
              (setq measures (min preview measures))))

        ;round up number of measures
        (setq measures (round-up measures))

        ;add time offset and beatlen of silence to rhythm track,
        (seq 
          (s-rest offset)    ;offset
          (make-click-track measures (* timesig beatlen)))) ;click track
      ;Else error message
      (setq error-msg (strcat (format nil
"Error.~%You have entered at least one invalid value:~%~%") error-msg)))))
