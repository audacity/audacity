$nyquist plug-in
$version 4
$type generate
$name (_ "Pluck")
$debugbutton false
$preview linear
$author (_ "David R.Sky")
$release 2.4.2
$copyright (_ "GNU General Public License v2.0")


;;MIDI values for C notes: 36, 48, 60 [middle C], 72, 84, 96.

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control PITCH (_ "Pluck MIDI pitch") int "" 60 1 120
$control FADE (_ "Fade-out type") choice ((_ "Abrupt") (_ "Gradual")) 0
$control DUR (_ "Duration (60s max)") time "" 1 0 60


; set final-amp for abrupt or gradual fade
(setf final-amp (if (= FADE 1) 0.001 0.000001))

(cond
  ((> DUR 0)
    ;; Get length of preview
    (setq pdur
      (if *previewp*
          (get '*project* 'preview-duration)
          DUR))

    (let* ((pluck-sound (snd-pluck *sound-srate* (step-to-hz PITCH) 0 DUR final-amp))
           (pluck-sound (extract-abs 0 pdur pluck-sound)) ; shorten if necessary for preview.
           (max-peak (peak pluck-sound ny:all)))
      ;; snd-pluck has a random element and will occasionally produce
      ;; zero amplitude at very high pitch settings. Avoid division by zero.
      (if (> max-peak 0)
          (scale (/ 0.8 max-peak) pluck-sound)
          pluck-sound)))
  ;; Length of sound is zero!
  ;; If previewing give Audacity a bit of silence, else return null string.
  (*previewp* (s-rest 0.1))
  (t ""))
