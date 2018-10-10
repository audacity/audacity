;nyquist plug-in
;version 4
;type analyze
;name "RMS"
;author "Steve Daulton"
;copyright "Released under terms of the GNU General Public License version 2"

;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

(defun stereo-rms(ar)
  "Stereo RMS is the root mean of all (samples ^ 2) [both channels]"
  (let ((left-mean-sq (* (aref ar 0)(aref ar 0)))
        (right-mean-sq (* (aref ar 1)(aref ar 1))))
    (sqrt (/ (+ left-mean-sq right-mean-sq) 2.0))))

(setf *locale*
  '(("de" (("Left" "Links")("Right" "Rechts")("Stereo" "Stereo")("Mono" "Mono")("dB" "dB")))
    ("es" (("Left" "Izquierda") ("Right" "Derecha") ("Stereo" "Estéreo") ("Mono" "Mono")("dB" "dB")))
    ("fr" (("Left" "Gauche")("Right" "Droite")("Stereo" "Stéréo")("Mono" "Mono")("dB" "dB")))
    ("ru" (("Left" "Левый")("Right" "Правый")("Stereo" "Стерео")("Mono" "Моно")("dB" "дБ")))))


(let ((rms (get '*selection* 'rms)))
  (if (arrayp rms)
      (format nil "~a: \t~a ~a~%~
                  ~a: \t~a ~a~%~
                  ~a: \t~a ~a"
                  (_ "Left") (linear-to-db (aref rms 0)) (_ "dB")
                  (_ "Right") (linear-to-db (aref rms 1)) (_ "dB")
                  (_ "Stereo") (linear-to-db (stereo-rms rms)) (_ "dB"))
      (format nil "~a: \t~a ~a" (_ "Mono")(linear-to-db rms)(_ "dB"))))
