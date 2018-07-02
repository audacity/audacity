$nyquist plug-in
$version 3
$type process
$preview enabled
$name (_ "Vocoder")
$manpage "Vocoder"
$action (_ "Processing Vocoder...")
$author (_ "Edgar-RFT")
$release 2.3.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; vocoder.ny by Edgar-RFT
;; a bit of code added by David R. Sky
;; GUI update by Steve Daulton July 2012.
;; Performance improvement, error message for mono, code cleanup
;; by Paul Licameli and Steve Daulton October 2014

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

$control dst (_ "Distance: (1 to 120, default = 20)") real "" 20 1 120
$control mst (_ "Output choice") choice (
   ("BothChannels" (_ "Both Channels"))
   ("RightOnly" (_ "Right Only"))
) 0
$control bands (_ "Number of vocoder bands") int "" 40 10 240
$control track-vl (_ "Amplitude of original audio (percent)") real "" 100 0 100
$control noise-vl (_ "Amplitude of white noise (percent)") real "" 0 0 100
$control radar-vl (_ "Amplitude of Radar Needles (percent)") real "" 0 0 100
$control radar-f (_ "Frequency of Radar Needles (Hz)") real "" 30 1 100

; maybe the code once again has to be changed into _one_ local let-binding
; if you have lots of nyquist "[gc:" messages try this:
; (expand 100) ; gives Xlisp more memory but I have noticed no speed difference

;; number of octaves between 20hz and 20khz
(setf octaves (/ (log 1000.0) (log 2.0)))

;; convert octaves to number of steps (semitones)
(setf steps (* octaves 12.0))

;; interval - number of steps per vocoder band
(setf interval (/ steps bands))

;;; Some useful calculations but not used in this plugin

;; half tone distance in linear
; (print (exp (/ (log 2.0) 12)))

;; octave distance in linear
; (print (exp (/ (log 1000.0) 40)))

;;; The Radar Wavetable

;; make *radar-table* a global variable.
(setf contol-dummy *control-srate*)   ; save old *control-srate*
(set-control-srate *sound-srate*)  
(setf *radar-table* (pwl (/ 1.0 *control-srate*) 1.0  ; 1.0 after 1 sample
                         (/ 2.0 *control-srate*) 0.0  ; 0.0 after 2 samples
                         (/ 1.0 radar-f))) ; stay 0.0 until end of the period
(set-control-srate contol-dummy)      ; restore *control-srate*
;; make *radar-table* become a nyquist wavetable of frequency radar-f
(setf *radar-table* (list *radar-table* (hz-to-step radar-f) T))

;; increase the volume of the audacity track in the middle of the slider
;; the sqrt trick is something like an artifical db scaling
(setf track-vol (sqrt (/ track-vl 100.0)))
;; decrease the volume of the white noise in the middle of the slider
;; the expt trick is an inverse db scaling
(setf noise-vol (expt (/ noise-vl 100.0) 2.0))
;; also increase the volume of the needles in the middle of the slider
(setf radar-vol (sqrt (/ radar-vl 100.0)))

;;; here you can switch the tracks on and off for bug tracking

;  (setf radar-vol 0)
;  (setf noise-vol 0)
;  (setf track-vol 0)

;;; The Mixer

;; calculate duration of audacity selection
(setf duration (/ len *sound-srate*))

(defun mix ()
  ;; if track volume slider is less than 100 percent decrease track volume
  (if (< track-vl 100)
      (setf s
            (vector (aref s 0) (scale track-vol (aref s 1)))))

  ;; if radar volume slider is more than 0 percent add some radar needles
  (if (> radar-vl 0)
      (setf s
            (vector (aref s 0)
                    (sim (aref s 1)
                         (scale radar-vol (osc (hz-to-step radar-f)
                                               duration *radar-table*))))))

  ;; if noise volume slider is more than 0 percent add some white noise
  (if (> noise-vl 0)
      (setf s
            (vector (aref s 0)
                    (sim (aref s 1) (scale noise-vol (noise duration)))))))

;;; The Vocoder

(defun vocoder ()
  (do* ((i 0 (1+ i))
        mod-envelope  ; local variable for filtered envelope of left channel.
        band          ; local variable for band-passed audio.
        (result 0)    ; result must be initialized because you cannot sum to NIL.
        (q (/ (sqrt 2.0) (/ octaves bands)))    ; quick approximation of q
                                                ; for given bandwidth.
        (p (+ (hz-to-step 20) (/ interval 2.0)) ; midi step of 20 Hz + offset.
           (+ p interval))
        (f (step-to-hz p) (step-to-hz p)))
      ((= i bands) result)        ; DO for each band then return 'result'.
    (setf band (bandpass2 s f q)) ; intermediate results (2 channels)
    (setf mod-envelope (lowpass8 (s-abs (aref band 0)) (/ f dst)))
    (setf result
          (sum result
               (bandpass2
                 (mult mod-envelope (aref band 1))
                 f q)))))

;;; The Program

(cond
 ((arrayp s)
  (mix)                     ; changes s
  (let ((original (or (= mst 0) (aref s 0))))
    (setf s (vocoder))
    ;; Now normalize s to 0 db peak
    (setf s (scale (/ (peak s ny:all)) s))
    (case mst
          (0 s)             ; let Audacity coerce back to stereo
          (1 (vector original s)))))
 (t                         ; this effect isn't meant for mono
  (format nil (_ "Error.~%Stereo track required."))))
