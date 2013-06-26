;nyquist plug-in
;version 1
;type process
;categories "http://lv2plug.in/ns/lv2core/#FilterPlugin"
;name "Notch Filter..."
;action "Performing Notch Filter..."

;control freq "Frequency" real "Hz" 60 0 10000
;control q "Q (higher value reduces width)" real "" 1 0.1 20

;; notch.ny by Steve Daulton and Bill Wharrie, September 2010.
;; Released under terms of the GNU General Public License version 2:
;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.html .

;; (multichan-expand) provides legacy support for old versions of Audacity 
;; in which the (notch2) function only supports mono tracks.

(cond
  ((> freq (/ *sound-srate* 2.0))(format nil "Error:\nFrequency too high for track sample rate."))
  ((< freq 0)(format nil "Error:\nNegative frequency is invalid."))
  ((< q 0.01)(format nil "Error:\nWidth must be at least 0.01."))
  ((= freq 0) (format nil "Nothing to be done."))
  (T (multichan-expand #'notch2 s freq q)))
