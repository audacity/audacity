;; seqmidi.lsp -- functions to use MIDI files in Nyquist
;
; example call:
; 
; (seq-midi my-seq
;   (note (chan pitch velocity) (= chan 2) (my-note pitch velocity))
;   (ctrl (chan control value) (...))
;   (bend (chan value) (...))
;   (touch (chan value) (...))
;   (prgm (chan value) (setf (aref my-prgm chan) value))

;; seq-midi - a macro to create a sequence of sounds based on midi file
;
; 
(defmacro seq-midi (the-seq &rest cases)
  (seq-midi-cases-syntax-check cases)
  `(let (_the-event _next-time _the-seq _seq-midi-closure _nyq-environment 
         _the-seq _tag)
    (setf _the-seq (seq-copy ,the-seq))
    (setf _nyq-environment (nyq:the-environment))
    (setf _seq-midi-closure #'(lambda (t0)
      ; (format t "_seq_midi_closure: t0 = ~A~%" t0)
      (prog (_the-sound)
loop	; go forward until we find note to play (we may be there)
        ; then go forward to find time of next note
        (setf _the-event (seq-get _the-seq))
        ; (display "seq-midi" _the-event t0)
        (setf _tag (seq-tag _the-event))
        (cond ((= _tag seq-ctrl-tag)
               ,(make-ctrl-handler cases))
              ((= _tag seq-bend-tag)
               ,(make-bend-handler cases))
              ((= _tag seq-touch-tag)
               ,(make-touch-handler cases))
              ((= _tag seq-prgm-tag)
               ,(make-prgm-handler cases))
              ((= _tag seq-done-tag)
               ; (format t "_seq_midi_closure: seq-done")
               (cond (_the-sound ; this is the last sound of sequence
                      ; (format t "returning _the-sound~%")
                      (return _the-sound))
                     (t ; sequence is empty, return silence
                      ; (format t "returning snd-zero~%")
                      (return (snd-zero t0 *sound-srate*)))))
              ((and (= _tag seq-note-tag)
                    ,(make-note-test cases))
               (cond (_the-sound ; we now have time of next note
                      (setf _next-time (/ (seq-time _the-event) 1000.0))
                      (go exit-loop))
                     (t
                      (setf _the-sound ,(make-note-handler cases))))))
        (seq-next _the-seq)
        (go loop)
exit-loop ; here, we know time of next note
        ; (display "seq-midi" _next-time)
        ; (format t "seq-midi calling snd-seq\n")
        (return (snd-seq
                  (set-logical-stop-abs _the-sound 
                        (local-to-global _next-time))
                  _seq-midi-closure)))))
    ; (display "calling closure" (get-lambda-expression _seq-midi-closure))
    (funcall _seq-midi-closure (local-to-global 0))))


(defun seq-midi-cases-syntax-check (cases &aux n)
  (cond ((not (listp cases))
         (break "syntax error in" cases)))
  (dolist (case cases)
    (cond ((or (not (listp case)) 
               (not (member (car case) '(NOTE CTRL BEND TOUCH PRGM)))
               (not (listp (cdr case)))
               (not (listp (cadr case)))
               (not (listp (cddr case)))
               (not (listp (last (cddr case)))))
           (break "syntax error in" case))
          ((/= (length (cadr case))
               (setf n (cdr (assoc (car case) 
                             '((NOTE . 3) (CTRL . 3) (BEND . 2)
                               (TOUCH . 2) (PRGM . 2))))))
           (break (format nil "expecting ~A arguments in" n) case))
          ((and (eq (car case) 'NOTE)
                (not (member (length (cddr case)) '(1 2))))
           (break 
            "note handler syntax is (NOTE (ch pitch vel) [filter] behavior)"
            case)))))


(defun make-ctrl-handler (cases)
  (let ((case (assoc 'ctrl cases)))
    (cond (case
           `(let ((,(caadr case) (seq-channel _the-event))
                  (,(cadadr case) (seq-control _the-event))
                  (,(caddar (cdr case)) (seq-value _the-event)))
              ,@(cddr case)))
          (t nil))))

(defun make-bend-handler (cases)
  (let ((case (assoc 'bend cases)))
    (cond (case
           `(let ((,(caadr case) (seq-channel _the-event))
                  (,(cadadr case) (seq-value _the-event)))
              ,@(cddr case)))
          (t nil))))

(defun make-touch-handler (cases)
  (let ((case (assoc 'touch cases)))
    (cond (case
           `(let ((,(caadr case) (seq-channel _the-event))
                  (,(cadadr case) (seq-value _the-event)))
              ,@(cddr case)))
          (t nil))))

(defun make-prgm-handler (cases)
  (let ((case (assoc 'pgrm cases)))
    (cond (case
           `(let ((,(caadr case) (seq-channel _the-event))
                  (,(cadadr case) (seq-value _the-event)))
              ,@(cddr case)))
          (t nil))))

(defun make-note-test (cases)
  (let ((case (assoc 'note cases)))
    (cond ((and case (cdddr case))
           (caddr case))
          (t t))))
           

(defun make-note-handler (cases)
  (let ((case (assoc 'note cases))
        behavior)
    (cond ((and case (cdddr case))
           (setf behavior (cadddr case)))
          (t
           (setf behavior (caddr case))))
    `(with%environment _nyq-environment
        (with-note-args ,(cadr case) _the-event ,behavior))))


(defmacro with-note-args (note-args the-event note-behavior)
  ; (display "with-note-args" the-event)
  `(let ((,(car note-args) (seq-channel ,the-event))
         (,(cadr note-args) (seq-pitch ,the-event))
         (,(caddr note-args) (seq-velocity ,the-event)))
     (at (/ (seq-time ,the-event) 1000.0)
      (stretch (/ (seq-duration ,the-event) 1000.0) ,note-behavior))))


;(defun seq-next-note-time (the-seq find-first-flag)
;  (prog (event)
;    (if find-first-flag nil (seq-next the-seq))
;loop
;    (setf event (seq-get the-seq))
;    (cond ((eq (seq-tag event) seq-done-tag)
;	   (return (if find-first-flag 0.0 nil)))
;	  ((eq (seq-tag event) seq-note-tag)
;	   (return (/ (seq-time event) 1000.0))))
;    (seq-next the-seq)
;    (go loop)))
; 
