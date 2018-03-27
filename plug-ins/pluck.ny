$nyquist plug-in
$version 4
$type generate
$name (_ "Pluck")
$manpage "Pluck"
$debugbutton false
$preview linear
$action (_ "Generating pluck sound...")
$info (_ "MIDI values for C notes: 36, 48, 60 [middle C], 72, 84, 96.")
$author (_ "David R.Sky")
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

; original pluck.ny modified by David R.Sky October 2007
; [vastly simplified later]
; to give user option to use [default] abrupt or gradual fade-out,
; and ability to make pluck sound up to 30 seconds in duration.
; Modified January 2007 to use 'snd-pluck' by edgar-rft@web.de
; instead of the Nyquist 'pluck' function, and normalise to 0.8
; maximum amplitude. As defined in Audacity, 'pluck' has 
; incorrect length duration and clipping at the start which gives
; rise to DC offset. Using 'snd-pluck' avoids the clipping and 
; reduces offset so we don't need the highpass8 filter that we used before.
; Updated to v4 by Steve Daulton May 2015


$control pitch (_ "Pluck MIDI pitch") int "" 60 1 120
$control fade (_ "Fade-out type") choice (
   (_ "Abrupt")
   (_ "Gradual")
) 0
$control dur (_ "Duration [seconds]") real "" 1 0.1 30


; set final-amp for abrupt or gradual fade
(setf final-amp (if (= fade 1) 0.001 0.000001))

;; Get length of preview
(setq pdur
  (if (get '*track* 'view) ;NIL if preview
      dur
      (get '*project* 'preview-duration)))

(let* ((pluck-sound (snd-pluck *sound-srate* (step-to-hz pitch) 0 dur final-amp))
       (pluck-sound (extract-abs 0 pdur pluck-sound)) ; shorten if necessary for preview.
       (max-peak (peak pluck-sound ny:all)))
  ;; snd-pluck has a random element and will occasionally produce
  ;; zero amplitude at very high pitch settings. Avoid division by zero.
  (if (> max-peak 0)
      (scale (/ 0.8 max-peak) pluck-sound)
      pluck-sound))
