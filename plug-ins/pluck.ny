
;nyquist plug-in
;version 3
;type generate
;categories "http://lv2plug.in/ns/lv2core#GeneratorPlugin"
;name "Pluck..."
;action "Generating pluck sound..."
;info "modified by David R. Sky\nReleased under terms of the GNU General Public License version 2 \nMIDI values for C notes: 36, 48, 60 [middle C], 72, 84, 96."

;control p "Pluck MIDI pitch" int "" 60 1 127
;control fade "Fade-out type" choice "abrupt,gradual" 0
;control dur "Duration [seconds]" real "" 1 0.1 30

; original pluck.ny modified by David R.Sky October 2007
; [vastly simplified later]
; to give user option to use [default] abrupt or gradual fade-out,
; and ability to make pluck sound up to 30 seconds in duration.
; Modified January 2007 to use 'snd-pluck' by edgar-rft@web.de
; instead of the Nyquist 'pluck' function, and normalise to 0.8
; maximum amplitude. As defined in Audacity, 'pluck' has 
; incorrect length duration and clipping at the start which gives
; rise to DC offset. Using 'snd-pluck' avoids the clipping and 
; offset so we don't need the highpass8 filter that we used before.  


; set final-amp for abrupt or gradual fade
(setf final-amp (if (= fade 1) 0.001 0.000001))

(let* ((pluck-sound (snd-pluck *sound-srate* (step-to-hz p) 0 dur final-amp))
       (max-peak (peak pluck-sound ny:all)))
  (scale (/ 0.8 max-peak) pluck-sound))  
  
  
;arch-tag: bebc6cb8-3bb0-42d5-a467-df6bd1a7f1e4