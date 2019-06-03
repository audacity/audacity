$nyquist plug-in
$version 4
$type analyze
$name (_ "Beat Finder")
$manpage "Beat_Finder"
$action (_ "Finding beats...")
$author (_ "Audacity")
$release 2.3.2
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control thresval (_ "Threshold Percentage") int "" 65 5 100

(setf threshold (/ thresval 100.0))

(defun mix-to-mono (sig)
  (if (arrayp sig)
      (sum (aref sig 0) (aref sig 1))
      sig))

(defun bass-tracker (sig)
  (let* ((bass (lp sig 50))
         ;(snd-follow sound floor risetime falltime lookahead)
         (follower (snd-follow bass 0.001 0.01 0.1 512)))
    (force-srate 1000 (lp follower 10))))


(let ((beats (bass-tracker (mix-to-mono *track*))))
  (setf peak-sig (peak beats ny:all))
  (setf threshold (* threshold peak-sig))
  (do ((time 0.0 (+ time 0.001))
       (val (snd-fetch beats) (snd-fetch beats))
       (flag T)
       labels)
      ((not val) labels)
    (when (and flag (> val threshold))
      (push (list time "B") labels))
    (setf flag (< val threshold))))
