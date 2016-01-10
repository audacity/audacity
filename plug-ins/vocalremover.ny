;nyquist plug-in
;version 3
;type process
;preview linear
;categories "http://lv2plug.in/ns/lv2core#UtilityPlugin"
;name "Vocal Remover..."
;action "Removing center-panned audio..."
;info "For reducing center-panned vocals"
;author "Steve Daulton"
;copyright "Released under terms of the GNU General Public License version 2"

;; This version of vocalremover.ny by Steve Daulton June 2013.
;;
;; based on Center pan Remover by David R. Sky November 12, 2004
;; Modified by Gale Andrews January 2008 to make full spectrum removal 
;; the default, restore a single Help screen and restore error checking.
;; Thanks to David Hostetler for notes in his own vocal remover plug-in,
;; http://www.freelists.org/archives/audacity4blind/06-2006/msg00049.html
;;
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html

;control action "Remove vocals or view Help" choice "Remove vocals,View Help" 0
;control band-choice "Removal choice" choice "Simple (entire spectrum),Remove frequency band,Retain frequency band" 0
;control low-range "Frequency band from (Hz)" float-text "" 500 0 nil
;control high-range "Frequency band to (Hz)" float-text "" 2000 0 nil


; Initialize globals
(setf message "")     ; empty output message

(defun help ()
  (format nil
"Vocal Remover requires a stereo track. It works best with
lossless files like WAV or AIFF, rather than MP3 or
other compressed formats. It only removes vocals or other
audio that is panned to center (sounds equally loud in left
and right). Vocals may be mixed this way. Inverting one
channel then panning both to center cancels out any audio
which was originally center-panned, making it inaudible.
This can remove some parts of the audio you may want to
keep, such as drums, which are also often mixed to center.
If the vocals and other centered parts differ in pitch,
this can be solved by removing only selected frequencies.~%
Vocal Remover thus has three choices of removal method.
'Simple' inverts the entire frequency spectrum of one
channel. This may remove too much music if other parts of
the audio are centered as well as the vocals. In that case,
try the other choices. If the vocals are at a different
pitch than the other audio (such as a high female voice),
try 'Remove frequency band'. This only removes frequencies
between a lower and upper limit which you can enter in the
'Frequency band...' boxes. Experiment by entering what
sounds like the most significant frequency range of the
original vocals. If the other choices remove too much
audio in a particular frequency range (such as low drums
or bass), try 'Retain frequency band'. This only removes
frequencies outside the limits, retaining the others."))


;;; ERROR CHECKING:

;; Check that selected audio is stereo
(defun check-stereo ()
  (if (soundp s)
      (setf message (format nil
                      "~%Vocal Remover requires an unsplit, stereo track.~%~
                      If you have a stereo track split into left and right~%~
                      channels, use 'Make Stereo Track' on the Track~%~
                      Drop-Down Menu, then run Vocal Remover again.~%"))))

(defmacro validate (Hz)
;; Filters become unstable when very close to 0 Hz or
;; Nyquist frequency, so disable.
  `(if (or (< ,Hz 1)(> ,hz (- (/ *sound-srate* 2) 1)))
       (setf ,Hz nil)))

;;; Check that frequency range is valid
(defun check-range ()
  ;; Ensure min < max
  (when (< high-range low-range)
    (let ((temp low-range))
      (setf low-range high-range)
      (setf high-range temp)))
  (validate low-range)
  (validate high-range))

(defun show-message ()
  ;; output to both message box and to debug window
  ;; Copying from debug window is supported on all platforms.
  (when (= action 0)      ; error
    (setf message (format nil "Error.~%~a" message)))
  (format t message)
  (format nil message))


;;; DSP FUNCTIONS:

(defun bandpass (sig low high)
  (cond
    ((and low high) ;bandpass
      (lowpass8
        (highpass8 sig low)
        high))
    (low (highpass8 sig low))
    (high (lowpass8 sig high))
    (t sig)))

(defun bandstop (sig low high)
  (if (< (/ (- high low) low) 0.1)
      (format t "Warning:~%~
              Selected band-stop filter is~%~
              ~a Hz to ~a Hz.~%~
              A very narrow stop-band filter may have~%~
              unexpected results.~%~%"
              low high))
  (let ((low-sig (if low (lowpass8 sig low)(s-rest 1))))
    (sum
      low-sig
      (if high (highpass8 (diff sig low-sig) high)(s-rest 1)))))

(defun CentrePanRemove ()
  (cond
    ((= band-choice 1)            ; remove frequencies inside range
      (sum (aref s 0)
           (mult -1 (aref s 1))
           (bandstop (aref s 1) low-range high-range)))
    ((= band-choice 2)            ; remove frequencies outside range
      (sum (aref s 0)
           (mult -1 (aref s 1))
           (bandpass (aref s 1) low-range high-range)))
    (t                            ; invert and add right to left channel
      (sum (aref s 0)
           (mult -1 (aref s 1))))))


;;; MAIN PROGRAM:

(cond
  ((= action 1)       ; Show help
    (setf message (help)))
  ((= band-choice 0)  ; Remove full spectrum
    (check-stereo))
  (t                  ; Remove band limited
    (check-stereo)
    (check-range)))

(if (= (length message) 0)
    (CentrePanRemove)
    (show-message))
