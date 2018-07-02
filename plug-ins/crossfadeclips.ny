$nyquist plugin
$version 4
$type process
$mergeclips 1
$restoresplits 0
$name (_ "Crossfade Clips")
$manpage "Crossfade_Clips"
$action (_ "Crossfading...")
$author (_ "Steve Daulton")
$release 2.3.0
$copyright (_ "Released under terms of the GNU General Public License version 2") 


;; crossfadeclips.ny by Steve Daulton Dec 2014.

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;; Instructions:
;; Place two audio clips into the same track.
;; Select (approximately) the same amount of audio from the
;; end of one clip and the start of the other.
;; Apply the effect.
;; The selected regions will be crossfaded.
;;
;; Note, the audio clips do not need to be touching. Any
;; white-space between the clips is ignored.
;;
;; If the selected region is continuous audio (no splits),
;; the the first and last halves of the selected audio
;; will be crossfaded.
;;
;; Advanced Tip:
;; A discontinuity in a waveform may be smoothed by applying
;; a short crossfade across the glitch.

;; Limitations (should not occur in normal usage).
;; 1) There may be no more than two clips selected in each channel.
;; 2) The selection may not start or end in white-space.


(setf err1 (format nil (_ "Error.~%Invalid selection.~%More than 2 audio clips selected.")))
(setf err2 (format nil (_ "Error.~%Invalid selection.~%Empty space at start/ end of the selection.")))


(defun find-ends (T0 T1 clips)
"Look for a split or gap within the selection, or return the mid-point"
  (let ((trk-ends ())    ;starts of clips
        (trk-starts ())) ;ends of clips
    (dolist (clip clips)
      ;; look for clip enclosing the selection.
      (when (and (>= (second clip) T1) (<= (first clip) T0))
        (psetq trk-ends   (list (/ (+ T0 T1) 2))
               trk-starts (list (/ (+ T0 T1) 2)))
        (return))
      ;; look for track starts.
      (when (and (> (first clip) T0) (< (first clip) T1))
        (push (first clip) trk-starts))
      ;; look for track ends.
      (when (and (> (second clip) T0) (< (second clip) T1))
        (push (second clip) trk-ends))
      ; stop looking when we have passed end of selection.
      (when (> (first clip) T1) (return)))
    ;; if exactly one split position for crossfading,
    ;; return clip positions, else error.
    (cond
      ((and (= (length trk-ends) 1)
            (= (length trk-starts) 1)
            (<= (car trk-ends) (car trk-starts)))
        (list (car trk-ends)(car trk-starts)))
      ((or (> (length trk-ends) 1)
           (> (length trk-starts) 1))
        (throw 'error err1))
      (T (throw 'error err2)))))

(defun crossfade (sig out-end in-start end)
"Do the crossfade"
  (abs-env
    (control-srate-abs *sound-srate*
      (let* ((fade-out (mult sig (env out-end 0)))
             (cflen (max out-end (- end in-start))) ;crossfade length
             (finstart (max (- out-end (- end in-start)) 0))
             (fade-in (mult (extract (- end cflen) end sig)
                            (env (- cflen finstart) 1 finstart))))
        (sim fade-out fade-in)))))

(defun env (dur direction &optional (offset 0))
"Generate envelope for crossfade"
  (abs-env
    (if (< dur 0.01)            ;make it linear
        (control-srate-abs *sound-srate*
          (if (= direction 0)
              (pwlv 1 dur 0)      ;fade out
              (pwlv 0 offset 0 (+ offset dur) 1)))  ;fade in
        (if (= direction 0)     ;cosine curve
            (cos-curve dur 0)
            (seq (s-rest offset)
                 (cos-curve dur 1))))))

(defun cos-curve (dur direction)
"Generate cosine curve"
  (if (= direction 0) ;fade out
      (osc (hz-to-step (/ 0.25 dur)) dur *sine-table* 90)
      (osc (hz-to-step (/ 0.25 dur)) dur *sine-table* 0)))

(defun process (sig t0 t1 clips)
"Find the split positions and crossfade"
  (setf fadeclips
    (multichan-expand #'find-ends t0 t1 clips))
  (if (arrayp fadeclips)
      (prog ((fade-out-end (min (first (aref fadeclips 0))
                                (first (aref fadeclips 1))))
             (fade-in-start (max (second (aref fadeclips 0))
                                 (second (aref fadeclips 1)))))
        (return
          (multichan-expand #'crossfade sig 
                                       (- fade-out-end t0)
                                       (- fade-in-start t0)
                                       (- t1 t0))))
      (crossfade sig
                 (- (first fadeclips) t0)
                 (- (second fadeclips) t0)
                 (- t1 t0))))


;;; Run the program.
(if (= (length (get '*selection* 'tracks)) 1)
    (catch 'error
      (process *track*
               (get '*selection* 'start) 
               (get '*selection* 'end)
               (get '*track* 'clips)))
    (format nil (_ "Error.~%Crossfade Clips may only be applied to one track.")))
