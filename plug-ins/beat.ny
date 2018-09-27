$nyquist plug-in
$version 1
$type analyze
$name (_ "Beat Finder")
$manpage "Beat_Finder"
$action (_ "Finding beats...")
$author (_ "Audacity")
$release 2.3.0
$copyright (_ "Released under terms of the GNU General Public License version 2")

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference


$control thresval (_ "Threshold Percentage") int "" 65 5 100
(setf s1 (if (arrayp s) (snd-add (aref s 0) (aref s 1)) s))
(defun signal () (force-srate 1000 (lp (snd-follow (lp s1 50) 0.001 0.01 0.1 512) 10)))
(setq max (peak (signal) NY:ALL))
(setq thres (* (/ thresval 100.0) max))
(setq s2 (signal))
(do ((c 0.0) (l NIL) (p T) (v (snd-fetch s2))) ((not v) l)
; "B" seems too short to put into the i18n catalog.  Make it a control?
 (if (and p (> v thres)) (setq l (cons (list c "B") l)))
 (setq p (< v thres))
 (setq c (+ c 0.001))
 (setq v (snd-fetch s2)))
