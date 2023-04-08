$nyquist plug-in
$version 4
$type generate
$preview linear
$i18n-hint named for Jean-Claude Risset (silent t)
$name (_ "Risset Drum")
$debugbutton false
$author (_ "Steven Jones")
$release 2.3.0-2
$copyright (_ "GNU General Public License v2.0 or later")

;; rissetdrum.ny by Steven Jones, after Jean Claude Risset.
;; Updated by Steve Daulton 2012 and May 2015.

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control FREQ (_ "Frequency (Hz)") real "" 100 50 2000
$control DECAY (_ "Decay (seconds)") real "" 2 0.1 60
$control CF (_ "Center frequency of noise (Hz)") real "" 500 100 5000
$control BW (_ "Width of noise band (Hz)") real "" 400 10 1000
$control NOISE (_ "Amount of noise in mix (percent)") real "" 25 0 100
$control GAIN (_ "Amplitude (0 - 1)") real "" 0.8 0 1


;; Reduce length when previewing.
(setq pdur
  (if *previewp*
      (get '*project* 'preview-duration)
      DECAY))


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


(defun percussion-env (dur)
  (let* ((half-life (expt 2.0 (- (log2 dur) 3))))
    (exp-dec 0 half-life dur)))

 
(defun risset-drum ()
  (let* ((decay2 (* DECAY 0.50))
         (low-note (* FREQ 0.10))
         (tone-gain (- 1 NOISE)))
    (setf pink (lowpass6 (noise decay2) BW))
    (setf rdrum 
      (mult tone-gain 
        (osc (hz-to-step low-note) decay2 *rdrum-table*)))
    (setf noise-band 
      (mult NOISE 
        (sine (hz-to-step CF) decay2)
        pink))
    (sum 
      (mult 
        (percussion-env decay2)
        (sum noise-band rdrum ))
      (mult tone-gain 
        (percussion-env DECAY)
        (sine (hz-to-step FREQ) DECAY)))))


;; Generate and normalize
(let* ((output (risset-drum))
       (output (extract-abs 0 pdur output)) ; shorten if necessary for preview.
       (peakval (peak output ny:all)))
  (scale (/ GAIN peakval) output))
