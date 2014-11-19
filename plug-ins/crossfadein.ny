;nyquist plug-in
;version 2
;type process
;categories "http://lv2plug.in/ns/lv2core#MixerPlugin"
;name "Cross Fade In"
;action "Cross-Fading In..."
;author "Audacity"
;copyright "Unknown"

(mult s (control-srate-abs *sound-srate* (s-sqrt (pwlv 0 1 1))))
