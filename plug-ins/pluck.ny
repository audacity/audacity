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
$release 2.3.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

$control pitch (_ "Pluck MIDI pitch") int "" 60 1 120
$control fade (_ "Fade-out type") choice ((_ "Abrupt") (_ "Gradual")) 0
$control dur (_ "Duration (60s max)") time "" 1 0.0 60


; set final-amp for abrupt or gradual fade
(setf final-amp (if (= fade 1) 0.001 0.000001))

(cond
  ((> dur 0)
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
          pluck-sound)))
  ;; If previewing   give Audacity a bit of silence, else return null string.
  ((get '*track* 'view) "")
  (t (s-rest 0.1)))
