;; sliders.lsp -- communicate with NyquistIDE to implement control panels
;; Roger B. Dannenberg
;; April 2015

;;    (stop-on-zero s) -- a sound that returns 1 until s goes to zero, then
;;            the sound terminates. If s comes from a slider and you multiply
;;            a sound by (stop-on-zero s), you can interactively stop it
;;    (make-slider-panel "name" color) -- sets panel name for the following
;;            sliders
;;    (make-slider "param" [initial [low high]]) -- create slider named 
;;            "param" with optional range and initial value. Also returns
;;            a sound.
;;    (make-button "param" normal) -- create a button named "param" with
;;            a starting value of normal (either 0 or 1). While the button
;;            in the panel is pressed, the value changes to 1 or 0.
;;    (get-slider-value "param") -- when called with a string, this looks up
;;            the slider value by name
;;    (slider-panel-close "name") -- close the panel window. Values of any 
;;            existing sliders become undefined.
;;    (slider "panel" "name" [dur]) -- make a signal from slider value
;;    (slider "name" [dur]) -- make a signal from slider in current panel
;;    (get-slider-value "panel" "name") -- get a float value
;;    (get-slider-value "name") -- get a float in current panel

;; *active-slider-panel* is the current panel to which sliders are added
;;
(if (not (boundp '*active-slider-panel*))
    (setf *active-slider-panel* nil))

;; *panels-in-use* is an assoc list of panels, where each panel
;;   is a list of allocated sliders stored as (name number)
;;
(if (not (boundp '*panels-in-use*))
    (setf *panels-in-use* nil))

;; allocate-slider-num -- find an unused slider number
;;   linear search is used to avoid maintaining a parallel structure
;;   for faster searching. We search starting at slider #10, leaving
;;   sliders 0-9 unused; for example, you might want to control them
;;   via open sound control, so this gives you 10 sliders that are
;;   off limits to allocation by the SLIDER function.
;;   
;;   This code takes advantage of the fact that dotimes and dolist
;;   return nil when they end normally, so we signal that we found
;;   or did not find i by explicitly returning. Note that RETURN
;;   returns from the innermost dotimes or dolist -- they do not
;;   return from allocate-slider-num.
;;
(defun allocate-slider-num ()
  (dotimes (n 990)
    (let ((i (+ n 10)))
      (cond ((not (dolist (panel *panels-in-use*)
                    (cond ((dolist (pair (cdr panel))
                             (cond ((eql (second pair) i) (return t))))
                           (return t)))))
              (return i))))))

;; remove panel from list of panels
(defun slider-panel-free (panel)
  (setf *panels-in-use* (remove panel *panels-in-use* :test #'equal)))

(setfn stop-on-zero snd-stoponzero)

(defun make-slider-panel (name &optional (color 0))
  (let ((panel (assoc name *panels-in-use* :test #'equal)))
    ;; first find if panel already exists. If so, free the resources
    (cond (panel
           (slider-panel-free panel)))
    (setf *active-slider-panel* (list name))
    (setf *panels-in-use* (cons *active-slider-panel* *panels-in-use*))
    (format t "slider-panel-create: \"~A\" ~A~%" name color)))

(defun make-slider (name &optional (init 0) (low 0) (high 1))
  (let ((num (allocate-slider-num)))
    (cond ((null num)
           (format t "WARNING: MAKE-SLIDER is out of slider numbers. ~A~%"
                     "No slider created."))
          ((not (and (stringp name) (numberp init) 
                     (numberp low) (numberp high)))
           (display 
            "WARNING: MAKE-SLIDER called with bad arguments. No slider created"
            name init low high)))
    ;; make sure we have an active panel
    (cond ((null *active-slider-panel*)
           (make-slider-panel "Controls")))
    ;; insert new slider into list of sliders in active panel. This
    ;; is aliased with an element in the assoc list *panels-in-use*.
    (rplacd *active-slider-panel* (cons (list name num) 
                                        (cdr *active-slider-panel*)))
    (format t "slider-create: \"~A\" ~A ~A ~A ~A~%" name num init low high)
    num))

(defun make-button (name &optional (normal 0))
  (let ((num (allocate-slider-num)))
    (cond ((null num)
           (format t "WARNING: MAKE-BUTTON is out of slider numbers. ~A~%"
                     "No button created."))
          ((not (and (stringp name) (numberp normal)))
           (display 
            "WARNING: MAKE-BUTTON called with bad arguments. No button created"
            name normal)))
    ;; make sure we have an active panel
    (cond ((null *active-slider-panel*)
           (slider-panel "Controls")))
    ;; insert new button into list of controls in active panel. This
    ;; is aliased with an element in the assoc list *panels-in-use*.
    (rplacd *active-slider-panel* (cons (list name num) 
                                        (cdr *active-slider-panel*)))
    (format t "button-create: \"~A\" ~A ~A~%" name num normal)
    num))

(defun close-slider-panel (name)
  (let ((panel (assoc name *panels-in-use* :test #'equal)))
    (cond ((not (stringp name))
           (display "WARNING: SLIDER-PANEL-CLOSED called with bad argument."
                    name)))
    (cond (panel
           (slider-panel-free panel)
           (format t "slider-panel-close: \"~A\"~%" name))
          (t
           (format t "WARNING: slider panel ~A not found.~%" name)))))

;; SLIDER-LOOKUP - find the slider by name
;;
(defun slider-lookup (name slider)
  (let ((panel (assoc name *panels-in-use* :test #'equal)) s)
    (cond ((null panel)
           (error "Could not find slider panel named" name)))
    (setf s (assoc slider (cdr panel) :test #'equal))
    (cond ((null s)
           (error "Could not find slider named" s)))
    (second s)))


;; SLIDER - creates a signal from real-time slider input
;; 
;; options are:
;;   (SLIDER number [dur])
;;   (SLIDER "name" [dur]) -- look up slider in current slider panel
;;   (SLIDER "panel" "name" [dur]) -- look up panel, then look up slider
;;
(defun slider (id &optional slider-name dur)
    (cond ((and (numberp id) (null slider-name))
           (setf dur 1.0))
          ((and (numberp id) (numberp slider-name) (null dur))
           (setf dur slider-name))
          ((and (stringp id) (null slider-name))
           (setf dur 1.0)
           (setf id (slider-lookup (car *active-slider-panel*) id)))
          ((and (stringp id) (numberp slider-name) (null dur))
           (setf dur slider-name)
           (setf id (slider-lookup (car *active-slider-panel*) id)))
          ((and (stringp id) (stringp slider-name) (null dur))
           (setf dur 1.0)
           (setf id (slider-lookup id slider-name)))
          ((and (stringp id) (stringp slider-name) (numberp dur))
           (setf id (slider-lookup id slider-name)))
          (t
           (error "SLIDER called with invalid arguments")))
    (setf dur (get-duration dur))
    (setf id (round id)) ;; just to make sure it's an integer
    (cond ((or (< id 0) (>= id 1000))
           (error "SLIDER index out of bounds" id)))
    (display "slider" id slider-name dur)
    (snd-slider id *rslt* *sound-srate* dur))


(if (not (boundp '*lpslider-cutoff*))
    (setf *lpslider-cutoff* 20.0))

(defun lpslider (id &optional slider-name dur)
  (lp (slider id slider-name dur) 20.0))

;; save built-in get-slider-value so we can redefine it
(if (not (fboundp 'prim-get-slider-value))
    (setfn prim-get-slider-value get-slider-value))

(defun get-slider-value (id &optional slider-name)
  (cond ((and (numberp id) (null slider-name)) nil)
        ((and (stringp id) (null slider-name))
         (setf id (slider-lookup (car *active-slider-pael*) id)))
        ((and (stringp id) (stringp slider-name))
         (setf id (slider-lookup id slider-name)))
        (t
         (error "GET-SLIDER-VALUE called with invalid arguments")))
  ;; further parameter checking is done in get-slider-value:
  (prim-get-slider-value id))

(autonorm-off)
(snd-set-latency 0.02)
(print "**********sliders.lsp************************")
(print "WARNING: AUTONORM IS NOW TURNED OFF")
(print "WARNING: AUDIO LATENCY SET TO 20MS")
(print "To restore settings, execute (autonorm-on) and")
(print "  (set-audio-latency 0.3)")
(print "*********************************************")
