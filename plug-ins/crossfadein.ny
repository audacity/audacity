;nyquist plug-in
;version 2
;type process
;categories "http://lv2plug.in/ns/lv2core#MixerPlugin"
;name "Cross Fade In"
;action "Cross-Fading In..."
(mult s (snd-exp (snd-scale 0.5 (snd-log (ramp)))))
