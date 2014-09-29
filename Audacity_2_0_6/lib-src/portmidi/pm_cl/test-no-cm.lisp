;; this is a half-baked sequence of PortMidi calls to test the interface
;; No calls to Common Music are made, hence test-no-cm.lisp

; setup cffi if it has not been done already
(if (not (boundp '*clpath*))
    (load "setup-pm.lisp"))

(defun println (s) (print s) (terpri))

;; initialize portmidi lib
(pm:portmidi)
;; timer testing
(pt:Start )
(pt:Started)
(format t "time is ~A, type something~%" (pt:Time))
(read)
(format t "time is ~A, type something~%" (pt:Time))
(read)
(pt:Time)
(format t "time is ~A, type something~%" (pt:Time))

;; device testing
(pm:CountDevices)
(pprint (pm:GetDeviceInfo ))
(defparameter inid (pm:GetDefaultInputDeviceID ))
(pm:GetDeviceInfo inid)
(defparameter outid (pm:GetDefaultOutputDeviceID ))
(pm:GetDeviceInfo outid)
;; output testing
(defparameter outid 4) ; 4 = my SimpleSynth
(defparameter outdev (pm:OpenOutput outid 100 1000))
(pm:getDeviceInfo outid) ; :OPEN should be T
;; message tests
(defun pm (m &optional (s t))
  (format s "#<message :op ~2,'0x :ch ~2,'0d :data1 ~3,'0d :data2 ~3,'0d>"
          (ash (logand (pm:Message.status m) #xf0) -4)
          (logand (pm:Message.status m) #x0f)
          (pm:Message.data1 m)
          (pm:Message.data2 m)))
(defparameter on (pm:message #b10010000 60 64))
(terpri)
(pm on)
(pm:Message.status on)
(logand (ash (pm:Message.status on) -4) #x0f)
(pm:Message.data1 on)
(pm:Message.data2 on)
(pm:WriteShort outdev (+ (pm:time) 100) on)
(defparameter off (pm:message #b10000000 60 64))
(terpri)
(pm off)
(terpri)
(println "type something for note off")
(read)
(pm:WriteShort outdev (+ (pm:time) 100) off)
(println "type something to close output device")
(read)
(pm:Close outdev)
;; event buffer testing
(defparameter buff (pm:EventBufferNew 8))
(loop for i below 8 for x = (pm:EventBufferElt buff i) 
   ;; set buffer events
   do
     (pm:Event.message x (pm:message #b1001000 (+ 60 i) (+ 100 i)))
     (pm:Event.timestamp x (* 1000 i)))
(loop for i below 8 for x = (pm:EventBufferElt buff i) 
   ;; check buffer contents
   collect (list (pm:Event.timestamp x)
                 (pm:Message.data1 (pm:Event.message x))
                 (pm:Message.data2 (pm:Event.message x))))
(pm:EventBufferFree buff)
;; input testing -- requires external midi keyboard
(println (pm:GetDeviceInfo ))
(defparameter inid 1) ; 1 = my external keyboard
(defparameter indev (pm:OpenInput inid 256)) 
(pm:GetDeviceInfo inid) ; :OPEN should be T
(pm:SetFilter indev pm:filt-realtime) ; ignore active sensing etc.
(println "poll says:")
(println (pm:Poll indev))
(println "play midi keyboard and type something")
(read)
;;
;; ...play midi keyboard, then ...
;;
(println "poll says")
(println (pm:Poll indev))
(defparameter buff (pm:EventBufferNew 32))
(defparameter num (pm:Read indev buff 32))
(println "pm:Read gets")
(println num)
(println "input messages:")
(pm:EventBufferMap (lambda (a b) b (terpri) (pm a))
                   buff num)
(pm:Poll indev)

(println "play keyboard, to stop, play middle-C")

;;; recv testing

(defparameter pitch 0)
(loop while (/= pitch 60) do
  (let ((n (pm:Read indev buff 1)))
    (cond ((= n 1)
           (pm:EventBufferMap
                (lambda (a b) 
                   b (pm a) (terpri)
                   (setf pitch (pm:Message.data1 a)))
                buff n)))))

(pm:EventBufferFree buff)
(pm:Close indev)


