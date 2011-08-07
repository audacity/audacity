;nyquist plug-in
;version 2
;type process
;categories "http://lv2plug.in/ns/lv2core#MixerPlugin"
;name "Cross Fade In"
;action "Cross-Fading In..."
(mult s (control-srate-abs *sound-srate* (s-sqrt (pwlv 0 1 1))))
