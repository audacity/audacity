;nyquist plug-in
;version 4
;type analyze
;name "Measure RMS"
;debugbutton false
;author "Steve Daulton"
;release 2.3.1-1
$copyright (_ "GNU General Public License v2.0 or later")

;; License: GPL v2+
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
;;
;; For information about writing and modifying Nyquist plug-ins:
;; https://wiki.audacityteam.org/wiki/Nyquist_Plug-ins_Reference

;; This plug-in demonstrates how authors of Nyquist plug-ins may add translations
;; for output messages. It is not currently possible to provide translations for strings
;; in the header comments (such as the plug-in name) of 3rd party plug-ins.


;; Translations strings:
;; The "en" translation is not normally required unless the original text is in another
;; language, but it can make testing easier.
(setf *locale*
  '(("en" (("Left" "Left")
           ("Right" "Right")
           ("Stereo" "Stereo")
           ("Mono" "Mono")
           ("dB" "dB")))
    ("de" (("Left" "Links")
           ("Right" "Rechts")
           ("Stereo" "Stereo")
           ("Mono" "Mono")
           ("dB" "dB")))
    ("es" (("Left" "Izquierda")
           ("Right" "Derecha")
           ("Stereo" "Estéreo")
           ("Mono" "Mono")
           ("dB" "dB")))
    ("fr" (("Left" "Gauche")
           ("Right" "Droite")
           ("Stereo" "Stéréo")
           ("Mono" "Mono")
           ("dB" "dB")))
    ("ru" (("Left" "Левый")
           ("Right" "Правый")
           ("Stereo" "Стерео")
           ("Mono" "Моно")
           ("dB" "дБ")))))


(defun stereo-rms(ar)
  ;;; Stereo RMS is the root mean of all (samples ^ 2) [both channels]
  (let ((left-mean-sq (* (aref ar 0)(aref ar 0)))
        (right-mean-sq (* (aref ar 1)(aref ar 1))))
    (sqrt (/ (+ left-mean-sq right-mean-sq) 2.0))))


(let ((rms (get '*selection* 'rms)))
  (if (arrayp rms)
      (format nil "~a: \t~a ~a~%~
                  ~a: \t~a ~a~%~
                  ~a: \t~a ~a"
                  (_ "Left") (linear-to-db (aref rms 0)) (_ "dB")
                  (_ "Right") (linear-to-db (aref rms 1)) (_ "dB")
                  (_ "Stereo") (linear-to-db (stereo-rms rms)) (_ "dB"))
      (format nil "~a: \t~a ~a" (_ "Mono")(linear-to-db rms)(_ "dB"))))
