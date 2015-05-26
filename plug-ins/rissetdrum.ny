;nyquist plug-in
;version 4
;type generate
;categories "http://lv2plug.in/ns/lv2core#GeneratorPlugin"
;preview linear
;name "Risset Drum..."
;action "Generating Risset Drum..."
;author "Steven Jones"
;copyright "Released under terms of the GNU General Public License version 2"

;; rissetdrum.ny by Steven Jones, after Jean Claude Risset.
;; Updated by Steve Daulton July 2012 and May 2015.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; http://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;control freq "Frequency (Hz)" real "" 100 50 2000
;control decay "Decay (seconds)" real "" 2 0.1 60
;control cf "Center frequency of noise (Hz)" real "" 500 100 5000
;control bw "Width of noise band (Hz)" real "" 400 10 1000
;control noise "Amount of noise in mix (percent)" real "" 25 0 100
;control gain "Amplitude (0 - 1)" real "" 0.8 0 1


(defun sanitise (val minx maxx)
  (min (max val minx) maxx))

;; Not required with validation in Audacity 2.1.1 but left
;; for compatibility.
(setq freq (sanitise freq 1 (/ *sound-srate* 2)))
(setq decay (sanitise decay 0.1 600))
(setq cf (sanitise cf 1 (/ *sound-srate* 2)))
(setq bw (sanitise bw 10 1000))
(setq noise (sanitise (/ noise 100) 0 1))
(setq gain (sanitise gain 0 1))

;; Get length of preview
(setq pdur
  (if (get '*track* 'view) ;NIL if preview
      decay
      (get '*project* 'preview-duration)))

(setq *rdrum-table* 
  (list 
    (mult 0.17
      (sum
        (scale 1.00 (build-harmonic 10 2048))
        (scale 1.50 (build-harmonic 16 2048))
        (scale 2.00 (build-harmonic 22 2048))
        (scale 1.50 (build-harmonic 23 2048))))
    (hz-to-step 1) t))

(defun log2 (n)
  (/ (log (float n))(log 2.0)))

(defun percussion-env (decay)
  (let* ((half-life (expt 2.0 (- (log2 decay) 3))))
    (exp-dec 0 half-life decay)))

 
(defun risset-drum (freq decay cf bw noise-gain)
  (let* ((decay2 (* decay 0.50))
         (low-note (* freq 0.10))
         (tone-gain (- 1 noise-gain)))
    (setf pink (lowpass6 (noise decay2) bw))
    (setf rdrum 
      (mult tone-gain 
        (osc (hz-to-step low-note) decay2 *rdrum-table*)))
    (setf noise-band 
      (mult noise-gain 
        (sine (hz-to-step cf) decay2)
        pink))
    (sum 
      (mult 
        (percussion-env decay2)
        (sum noise-band rdrum ))
      (mult tone-gain 
        (percussion-env decay)
        (sine (hz-to-step freq) decay)))))

;; Generate and normalize
(let* ((output (risset-drum freq decay cf bw noise))
       (output (extract-abs 0 pdur output)) ; shorten if necessary for preview.
       (peakval (peak output ny:all)))
  (scale (/ gain peakval) output))
