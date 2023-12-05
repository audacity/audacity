$nyquist plug-in
$version 4
$type process
$preview linear
$name (_ "Shelf Filter")
$debugbutton disabled
$author (_ "Steve Daulton")
$release 2.4.0-1
$copyright (_ "GNU General Public License v2.0")

;; License: GPL v2
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control TYPE (_ "Filter type") choice (("Low" (_ "Low-shelf"))
                                        ("High" (_ "High-shelf"))) 0
$control HZ (_ "Frequency (Hz)") int "" 1000 10 10000
$control GAIN (_ "Amount (dB)") int "" -6 -72 72


(cond ((> HZ (/ *sound-srate* 2))
          (format nil (_ "Error.~%Frequency set too high for selected track.")))
      ((> HZ (/ *sound-srate* 2.1))  ;Handle edge case close to Nyquist frequency.
          (setf *track* (force-srate (* 2 *sound-srate*) *track*))
          (if (= TYPE 0)
              (force-srate *sound-srate* (eq-lowshelf *track* HZ GAIN))
              (force-srate *sound-srate* (eq-highshelf *track* HZ GAIN))))
      ((= GAIN 0) "")  ; no-op
      (t  (if (= TYPE 0)
              (eq-lowshelf *track* HZ GAIN)
              (eq-highshelf *track* HZ GAIN))))
