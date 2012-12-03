;nyquist plug-in
;version 1
;type process
;categories "http://lv2plug.in/ns/lv2core#MixerPlugin"
;name "One-click Fade out"
;action "Applying Fade..."
;info "by Steve Daulton. (www.easyspacepro.com).\nReleased under GPL v2.\n"

;; one-click-fade.ny by Steve Daulton July 2012.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; Produce a professional sounding fade out.
;; Applies a sinusoidal fade out with a progressive low-pass
;; filter from full spectrum at start to 100 Hz at end.
;;
;; For information about writing and modifying Nyquist plug-ins:
;; http://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

(lp 
  (mult s 0.5 
    (sum 1 
      (osc (hz-to-step (/ (get-duration 2))) 1 *table* 90)))
 (pwlv (/ *sound-srate* 2) 1 100))
