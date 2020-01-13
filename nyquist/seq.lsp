;; seq.lsp -- sequence control constructs for Nyquist

;; get-srates -- this either returns the sample rate of a sound or a
;;   vector of sample rates of a vector of sounds
;;
(defun get-srates (sounds)
  (cond ((arrayp sounds)
         (let ((result (make-array (length sounds))))
           (dotimes (i (length sounds))
                    (setf (aref result i) (snd-srate (aref sounds i))))
           result))
        (t
         (snd-srate sounds))))

; These are complex macros that implement sequences of various types.
; The complexity is due to the fact that a behavior within a sequence
; can reference the environment, e.g. (let ((p 60)) (seq (osc p) (osc p)))
; is an example where p must be in the environment of each member of
; the sequence.  Since the execution of the sequence elements are delayed,
; the environment must be captured and then used later.  In XLISP, the
; EVAL function does not execute in the current environment, so a special
; EVAL, EVALHOOK must be used to evaluate with an environment.  Another
; feature of XLISP (see evalenv.lsp) is used to capture the environment
; when the seq is first evaluated, so that the environment can be used
; later.  Finally, it is also necessary to save the current transformation
; environment until later.

;; SEQ-EXPR-EXPAND - helper function, expands expression to push/pop entry 
;;    on *sal-call-stack* to help debug calls into SAL from lazy evaluation
;;    of SAL code by SEQ
(defun seq-expr-expand (expr)
  (if *sal-call-stack*
    (list 'prog2 (list 'sal-trace-enter (list 'quote (list "Expression in SEQ:" expr)))
                 expr
                 '(sal-trace-exit))
    expr))


(defmacro seq (&rest list)
  (cond ((null list)
         (snd-zero (warp-time *WARP*) *sound-srate*))
        ((null (cdr list))
         (car list))
        ((null (cddr list))
         ;; SEQ with 2 behaviors
         `(let* ((first%sound ,(seq-expr-expand (car list)))
                (s%rate (get-srates first%sound)))
            (cond ((arrayp first%sound)
                   (snd-multiseq (prog1 first%sound (setf first%sound nil))
                     #'(lambda (t0)
                        (with%environment ',(nyq:the-environment)
                            (at-abs t0
                                (force-srates s%rate ,(seq-expr-expand (cadr list))))))))
                  (t
                   ; allow gc of first%sound:
                   (snd-seq (prog1 first%sound (setf first%sound nil))
                     #'(lambda (t0)
                        (with%environment ',(nyq:the-environment)
                            (at-abs t0
                                (force-srate s%rate ,(seq-expr-expand (cadr list)))))))))))

        (t ;; SEQ with more than 2 behaviors
         `(let* ((nyq%environment (nyq:the-environment))
                 (first%sound ,(car list))
                 (s%rate (get-srates first%sound))
                 (seq%environment (getenv)))
            (cond ((arrayp first%sound)
                   (snd-multiseq (prog1 first%sound (setf first%sound nil))
                     #'(lambda (t0)
                        (multiseq-iterate ,(cdr list)))))
                  (t 
                   ; allow gc of first%sound:
                   (snd-seq (prog1 first%sound (setf first%sound nil))
                     #'(lambda (t0)
                        (seq-iterate ,(cdr list))))))))))

(defun envdepth (e) (length (car e)))

(defmacro myosd (pitch)
  `(let () (format t "myosc env depth is ~A~%" 
                   (envdepth (getenv))) (osc ,pitch)))

(defmacro seq-iterate (behavior-list)
  (cond ((null (cdr behavior-list))
         ;; last expression in list
         `(eval-seq-behavior ,(seq-expr-expand (car behavior-list))))
        (t ;; more expressions after this one
         `(snd-seq (eval-seq-behavior ,(seq-expr-expand (car behavior-list)))
                   (evalhook '#'(lambda (t0) 
                                  ; (format t "lambda depth ~A~%" (envdepth (getenv)))
                                  (seq-iterate ,(cdr behavior-list)))
                             nil nil seq%environment)))))

(defmacro multiseq-iterate (behavior-list)
  (cond ((null (cdr behavior-list))
         `(eval-multiseq-behavior ,(seq-expr-expand (car behavior-list))))
        (t
         `(snd-multiseq (eval-multiseq-behavior ,(seq-expr-expand (car behavior-list)))
                   (evalhook '#'(lambda (t0) 
                                  (multiseq-iterate ,(cdr behavior-list)))
                             nil nil seq%environment)))))

(defmacro eval-seq-behavior (beh)
  `(with%environment nyq%environment 
                     (at-abs t0
                             (force-srate s%rate ,beh))))

(defmacro eval-multiseq-behavior (beh)
  `(with%environment nyq%environment 
                     (at-abs t0
                             (force-srates s%rate ,beh))))

(defmacro with%environment (env &rest expr)
  `(progv ',*environment-variables* ,env ,@expr))



(defmacro seqrep (pair sound)
  `(let ((,(car pair) 0)
         (loop%count ,(cadr pair))
         (nyq%environment (nyq:the-environment))
         seqrep%closure first%sound s%rate)
     ; note: s%rate will tell whether we want a single or multichannel
     ; sound, and what the sample rates should be.
     (cond ((not (integerp loop%count))
            (error "bad argument type" loop%count))
           (t
            (setf seqrep%closure #'(lambda (t0)
;             (display "SEQREP" loop%count ,(car pair))
              (cond ((< ,(car pair) loop%count)
                     (setf first%sound 
                           (with%environment nyq%environment
                                             (at-abs t0 ,sound)))
                     ; (display "seqrep" s%rate nyq%environment ,(car pair)
                     ;          loop%count)
                     (if s%rate
                       (setf first%sound (force-srates s%rate first%sound))
                       (setf s%rate (get-srates first%sound)))
                     (setf ,(car pair) (1+ ,(car pair)))
                     ; note the following test is AFTER the counter increment
                     (cond ((= ,(car pair) loop%count)
;                            (display "seqrep: computed the last sound at"
;                               ,(car pair) loop%count
;                               (local-to-global 0))
                            first%sound) ;last sound
                           ((arrayp s%rate)
;                            (display "seqrep: calling snd-multiseq at"
;                             ,(car pair) loop%count (local-to-global 0) 
;                             (snd-t0 (aref first%sound 0)))
                            (snd-multiseq (prog1 first%sound
                                                 (setf first%sound nil))
                                          seqrep%closure))
                           (t
;                            (display "seqrep: calling snd-seq at"
;                             ,(car pair) loop%count (local-to-global 0) 
;                             (snd-t0 first%sound))
                            (snd-seq (prog1 first%sound
                                            (setf first%sound nil))
                                     seqrep%closure))))
                    (t (snd-zero (warp-time *WARP*) *sound-srate*)))))
            (funcall seqrep%closure (local-to-global 0))))))


(defmacro trigger (input beh)
  `(let ((nyq%environment (nyq:the-environment)))
     (snd-trigger ,input #'(lambda (t0) (with%environment nyq%environment
                                                (at-abs t0 ,beh))))))

;; EVENT-EXPRESSION -- the sound of the event
;;
(setfn event-expression caddr)


;; EVENT-HAS-ATTR -- test if event has attribute
;;
(defun event-has-attr (note attr)
  (expr-has-attr (event-expression note)))


;; EXPR-SET-ATTR -- new expression with attribute = value
;;
(defun expr-set-attr (expr attr value)
  (cons (car expr) (list-set-attr-value (cdr expr) attr value)))

(defun list-set-attr-value (lis attr value)
  (cond ((null lis) (list attr value))
        ((eq (car lis) attr)
         (cons attr (cons value (cddr lis))))
        (t
         (cons (car lis)
           (cons (cadr lis) 
                 (list-set-attr-value (cddr lis) attr value))))))


;; EXPAND-AND-EVAL-EXPR -- evaluate a note, chord, or rest for timed-seq
;;
(defun expand-and-eval-expr (expr)
  (let ((pitch (member :pitch expr)))
    (cond ((and pitch (cdr pitch) (listp (cadr pitch)))
           (setf pitch (cadr pitch))
           (simrep (i (length pitch))
             (eval (expr-set-attr expr :pitch (nth i pitch)))))
          (t
           (eval expr)))))


;; (timed-seq '((time1 stretch1 expr1) (time2 stretch2 expr2) ...))
;; a timed-seq takes a list of events as shown above
;; it sums the behaviors, similar to 
;;     (sim (at time1 (stretch stretch1 expr1)) ...)
;; but the implementation avoids starting all expressions at once
;; 
;; Notes: (1) the times must be in increasing order
;;   (2) EVAL is used on each event, so events cannot refer to parameters
;;        or local variables
;;
;; If score events are very closely spaced (< 1020 samples), the block
;; overlap can cause a ripple effect where to complete one block of the
;; output, you have to compute part of the next score event, but then
;; it in turn computes part of the next score event, and so on, until
;; the stack overflows (if you have 1000's of events).
;;
;; This is really a fundamental problem in Nyquist because blocks are
;; not aligned. To work around the problem (but not totally solve it)
;; scores are evaluated up to a length of 100. If there are more than
;; 100 score events, we form a balanced tree of adders so that maybe
;; we will end up with a lot of sound in memory, but at least the
;; stack will not overflow. Generally, we should not end up with more
;; than 100 times as many blocks as we would like, but since the
;; normal space required is O(1), we're still using constant space +
;; a small constant * log(score-length).
;;
(setf MAX-LINEAR-SCORE-LEN 100)
(defun timed-seq (score)
  (must-be-valid-score "TIMED-SEQ" score)
  (let ((len (length score))
        pair)
    (cond ((< len MAX-LINEAR-SCORE-LEN)
           (timed-seq-linear score))
          (t ;; split the score -- divide and conquer
           (setf pair (score-split score (/ len 2)))
           (sum (timed-seq (car pair)) (timed-seq (cdr pair)))))))

;; score-split -- helper function: split score into two, with n elements
;;                in the first part; returns a dotted pair
(defun score-split (score n)
  ;; do the split without recursion to avoid stack overflow
  ;; algorithm: modify the list destructively to get the first
  ;; half. Copy it. Reassemble the list.
  (let (pair last front back)
    (setf last (nthcdr (1- n) score))
    (setf back (cdr last))
    (rplacd last nil)
    (setf front (append score nil)) ; shallow copy
    (rplacd last back)
    (cons front back)))

 
;; TIMED-SEQ-LINEAR - check to insure that times are strictly increasing
;;                    and >= 0 and stretches are >= 0
(defun timed-seq-linear (score)
  (let ((start-time 0) error-msg rslt)
    (dolist (event score)
      (cond ((< (car event) start-time)
             (error (format nil
                     "Out-of-order time in TIMED-SEQ: ~A, consider using SCORE-SORT"
                     event)))
            ((< (cadr event) 0)
             (error (format nil "Negative stretch factor in TIMED-SEQ: ~A" event)))
            (t
             (setf start-time (car event)))))
    ;; remove rests (a rest has a :pitch attribute of nil)
    (setf score (score-select score #'(lambda (tim dur evt)
                                       (expr-get-attr evt :pitch t))))
    (cond ((and score (car score) 
                (eq (car (event-expression (car score))) 'score-begin-end))
           (setf score (cdr score)))) ; skip score-begin-end data
    (cond ((null score) (s-rest 0))
          (t
           (at (caar score)
               (seqrep (i (length score))
                 (progn
                   (cond (*sal-call-stack*
                          (sal-trace-enter (list "Score event:" (car score)) nil nil)
                          (setf *sal-line* 0)))
                   (setf rslt
                     (cond ((cdr score)
                            (prog1
                              (set-logical-stop
                                (stretch (cadar score)
                                  (expand-and-eval-expr (caddar score)))
                                (- (caadr score) (caar score)))
                              (setf score (cdr score))))
                           (t
                            (stretch (cadar score) (expand-and-eval-expr
                                                    (caddar score))))))
                   (if *sal-call-stack* (sal-trace-exit))
                   rslt)))))))
