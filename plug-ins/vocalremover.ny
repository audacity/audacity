;nyquist plug-in
;version 3
;type process
;categories "http://lv2plug.in/ns/lv2core#UtilityPlugin"
;name "Vocal Remover (for center-panned vocals)..."
;action "Removing vocals or other center-panned audio..."
;info "by David R. Sky www.shellworld.net/~davidsky/ \nReleased under terms of the GNU General Public License version 2 \nRemoves center-panned audio in a stereo track by inversion and panning to center.\n\n'Simple' removal removes all the center-panned audio. If too much audio is removed,\ntry removing only selected frequencies - enter these in the box 'Frequency band\nlower and upper limit'. Then choose 'Remove band' to remove only frequencies in\nthat band, or 'Retain band' to remove only frequencies outside that band. After\nremoval, the audio will sound mono because both channels are panned to center.\n\nFor further help, select 'View Help' in the first dropdown menu and click OK. After\nreading Help, please reopen Vocal Remover to use it.\n"


;control action "Remove vocals or view Help" choice "Remove vocals,View Help" 0
;control bc "Removal choice" choice "Simple (entire spectrum),Remove frequency band,Retain frequency band" 0
;control range "Frequency band lower and upper limit [Hz]\n [Enter two values between 0 and 20000]" string " " "500 2000"

; Center pan Remover by David R. Sky November 12, 2004
; updated October/November 2007. Further modified by 
; Gale Andrews January 2008 to make full spectrum removal 
; the default, restore a single Help screen and restore error checking.
; Ideally wants rewriting so that no error checking occurs when
; default full spectrum removal selected.
; Thanks to David Hostetler for notes in his own vocal remover plug-in,
; which makes this plug-in more effective. See -
; http://www.freelists.org/archives/audacity4blind/06-2006/msg00049.html

(cond ; either explain this effect or perform it
((= action 1) ; display Help screen
(format nil
"Vocal Remover requires a stereo track. It works best with\nlossless files like like WAV or AIFF, rather than MP3 or\nother compressed formats. It only removes vocals or other\naudio that is panned to center (sounds equally loud in left\nand right). Vocals are often mixed this way. Inverting one\nchannel then panning both to center cancels out any audio\nwhich was originally center-panned, making it inaudible.\nThis can remove some parts of the audio you may want to\nkeep, such as drums, which are also often mixed to center.\nIf the vocals and other centered parts differ in pitch,\nthis can be solved by removing only selected frequencies.\n 
Vocal Remover thus has three choices of removal method.\n'Simple' inverts the entire frequency spectrum of one\nchannel. This may remove too much music if other parts of\nthe audio are centered as well as the vocals. In that case,\ntry the other choices. If the vocals are at a different\npitch than the other audio (such as a high female voice),\ntry 'Remove frequency band'. This only removes frequencies\nbetween a lower and upper limit which you can enter in the\n'Frequency band...' box. Experiment by entering what sounds\nlike the most significant frequency range of the original\nvocals. If the other choices remove too much audio in a\nparticular frequency range (such as low drums or bass), try\n'Retain frequency band'. This only removes frequencies\noutside the limits entered, retaining the others."))


(t ; perform effect
(defun string-to-list (str)
(read (make-string-input-stream (format nil "(~a)" str))))


(setf range (string-to-list range))

; initialize empty error-msg
(setf error-msg "")

; Error-checking...
;
; check that selected audio is stereo
(setf error-msg (if (arrayp s)
error-msg
(strcat error-msg (format nil
"Error:\n\nVocal Remover requires an unsplit, stereo track.\n\nIf you have a stereo track split into left and right\nchannels, use 'Make Stereo Track' on the Track\nDropdown Menu, then run Vocal Remover again. 
"))))


; Check there are two input frequency values given. If not and remove or retain band is selected,  
; ask to enter their required values. If not and 'Simple' removal selected, ask to enter any two values. 
(setf error-msg (if 
(and (> bc 0)
(< (length range) 2))
(strcat error-msg (format nil
"Error:\n\nPlease enter both a lower and upper value for the\nfrequency band you want to remove or retain.\n\nBoth values must be between 0 and 20000.
" (nth bc '("" ""))))

; are range values numbers?  
(if
(or (not (numberp (first range))) (not (numberp (second range))))
(strcat error-msg (format nil
"To perform 'Simple' removal you can enter any\ntwo numbers (separated by a space) in the\n'Frequency band lower and upper limit' box.\nThe numbers entered don't affect the result.\n\nTo remove or retain a frequency band, enter\nlower and upper values for the band between\n0 and 20000.
" (first range) (second range)))

; if 'Simple' removal not selected, throw error if both frequency values are not between 0 and 20000
(if (or (= bc 0)
(and 
(>= (first range) 0) (<= (first range) 20000)
(>= (second range) 0) (<= (second range) 20000)))
error-msg
(strcat error-msg (format nil
"Error:\n\n~aAt least one frequency value in your band is invalid. \nYou entered: ~a   ~a\n\nBoth the lower and upper values must be between\n0 and 20000.
"(nth bc '("" "" ""))
(first range) (second range)))))))


(cond
((> (length error-msg) 0)
(format nil "~a" error-msg))

(t ; no error msg
(setf lower (min (first range) (second range)))
(setf upper (max (first range) (second range)))


(cond
((= bc 1) ; invert [delete] band of frequencies inside range
(sum (aref s 0) (mult -1 (aref s 1))
(highpass8 (aref s 1) upper)
(lowpass8 (aref s 1) lower)))

((= bc 2) ; invert [delete] frequencies outside band range
(sum (aref s 0) (mult -1 (aref s 1))
(highpass8 (lowpass8 (aref s 1) upper) lower)))

(t ; invert one channel
(sum (aref s 0) (mult -1 (aref s 1)))))
) ; end t apply effect
) ; end cond between display error msg or apply effect
) ; end t perform effect
) ; end cond explain effect or perform it
