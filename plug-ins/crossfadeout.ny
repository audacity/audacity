;nyquist plug-in
;version 2
;type process
;categories "http://lv2plug.in/ns/lv2core#MixerPlugin"
;name "Cross Fade Out"
;action "Cross-Fading Out..."
;author "Audacity"
;copyright "Unknown"

(mult s (snd-exp
          (snd-scale 0.5 (snd-log
                          (sum 1 (snd-scale -1 (ramp)))))))
