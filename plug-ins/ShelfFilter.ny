$nyquist plug-in
$version 4
$type process
$preview linear
$name (_ "Shelf Filter")
$debugbutton disabled
$author (_ "Steve Daulton")
$release 2.4.0
$copyright (_ "GNU General Public License v2.0")

;; License: GPL v2
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control type (_ "Filter type") choice (("Low" (_ "Low-shelf"))
                                        ("High" (_ "High-shelf"))) 0
$control hz (_ "Frequency (Hz)") int "" 1000 10 10000
$control gain (_ "Amount (dB)") int "" -6 -72 72


(cond ((> hz (/ *sound-srate* 2))
          (format nil (_ "Error.~%Frequency set too high for selected track.")))
      ((> hz (/ *sound-srate* 2.1))  ;Handle edge case close to Nyquist frequency.
          (setf *track* (force-srate (* 2 *sound-srate*) *track*))
          (if (= type 0)
              (force-srate *sound-srate* (eq-lowshelf *track* hz gain))
              (force-srate *sound-srate* (eq-highshelf *track* hz gain))))
      ((= gain 0) "")  ; no-op
      (t  (if (= type 0)
              (eq-lowshelf *track* hz gain)
              (eq-highshelf *track* hz gain))))
