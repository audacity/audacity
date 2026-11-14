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
;
; The SEQ implementation passes an environment through closures that
; are constructed to evaluate expressions. SEQREP is similar, but
; the loop variable must be incremented and tested.
;
; Other considerations are that SEQ can handle multi-channel sounds, but
; we don't know to call the snd_multiseq primitive until the first
; SEQ expression is evaluated. Also, there's no real "NIL" for the end
; of a sequence, so we need several special cases: (1) The sequences
; is empty at the top level, so return silence, (2) There is one
; expression, so just evaluate it, (3) there are 2 expressions, so 
; return the first followed by the second, (4) there are more than
; 2 expressions, so return the first followed by what is effectively
; a SEQ consisting of the remaining expressions.


;; SEQ-EXPR-EXPAND - helper function, expands expression to push/pop entry 
;;    on *sal-call-stack* to help debug calls into SAL from lazy evaluation
;;    of SAL code by SEQ
(defun seq-expr-expand (expr source)
  (if *sal-call-stack*
    `(prog2 (sal-trace-enter '(,(strcat "Expression in " source ":") ,expr))
            ,expr ;; here is where the seq behavior is evaluated
            (sal-trace-exit))
    expr))


(defun with%environment (env expr)
  ;; (progv (var1 ...) (val1 ...) expression-list)
  `(progv ',*environment-variables* ,env ,expr))
;(trace with%environment seq-expr-expand)

(defmacro eval-seq-behavior (beh source)
  ;(tracemacro 'eval-seq-behavior (list beh source)
  (seq-expr-expand (with%environment 'nyq%environment
                      `(at-abs t0
                               (force-srates s%rate ,beh))) source));)

;; Previous implementations grabbed the environment and passed it from
;; closure to closure so that each behavior in the sequence could be
;; evaluated in the saved environment using an evalhook trick. This
;; version precomputes closures, which avoids using evalhook to get or
;; use the environment. It's still tricky, because each behavior has
;; to pass to snd-seq a closure that computes the remaining behavior
;; sequence. To do this, I use a recursive macro to run down the
;; behavior sequence, then as the recursion unwinds, construct nested
;; closures that all capture the current environment. We end up with a
;; closure we can apply to the current time to get a sound to return.
;;
(defmacro seq (&rest behlist)
  ;; if we have no behaviors, return zero
  (cond ((null behlist)
         '(snd-zero (local-to-global 0) *sound-srate*))
        (t  ; we have behaviors. Must evaluate one to see if it is multichan:
         `(let* ((first%sound ,(seq-expr-expand (car behlist) "SEQ"))
                 (s%rate (get-srates first%sound))
                 (nyq%environment (nyq:the-environment)))
            ; if there's just one behavior, we have it and we're done:
            ,(progn (setf behlist (cdr behlist))
                    (if (null behlist) 'first%sound
                        ; otherwise, start the recursive construction:
                        `(if (arrayp first%sound)
                             (seq2-deferred snd-multiseq ,behlist)
                             (seq2-deferred snd-seq ,behlist))))))))


;; seq2-deferred uses seq2 and seq3 to construct nested closures for
;; snd-seq. It is deferred so that we can first (in seq) determine whether
;; this is a single- or multi-channel sound before recursively constructing
;; the closures, since we only want to do it for either snd-seq or
;; snd-multiseq, but not both. It simply calls seq2 to begin the expansion.
;;
(defmacro seq2-deferred (seq-prim behlist)
  (seq2 seq-prim behlist))


#|
;; for debugging, you can replace references to snd-seq with this
(defun snd-seq-trace (asound aclosure)
  (princ "Evaluating SND-SEQ-TRACE instead of SND-SEQ...\n")
  (format t "  Sound argument is ~A\n" asound)
  (princ "  Closure argument is:\n")
  (pprint (get-lambda-expression aclosure))
  (princ "  Calling SND-SEQ ...\n")
  (let ((s (snd-seq asound aclosure)))
    (format t "  SND-SEQ returned ~A\n" s)
    s))

;; also for debugging, you can uncomment some tracemacro wrappers from
;; macro definitions. This function prints what the macro expands to
;; along with name and args (which you add by hand to the call):
(defun tracemacro (name args expr)
  (format t "Entered ~A with args:\n" name)
  (pprint args)
  (format t "Returned from ~A with expression:\n" name)
  (pprint expr)
  expr)
|#

  
;; we have at least 2 behaviors so we need the top level call to be
;; a call to snd-multiseq or snd-seq. This macro constructs the call
;; and uses recursion with seq3 to construct the remaining closures.
;;
(defun seq2 (seq-prim behlist)
  `(,seq-prim first%sound
              (prog1 ,(seq3 seq-prim behlist)  ; <- passed to seq-prim
                     ;; we need to remove first%sound from the closure
                     ;; to avoid accumulating samples due to an unnecessary
                     ;; reference:
                     (setf first%sound nil))))

;; construct a closure that evaluates to a sequence of behaviors.
;; behlist has at least one behavior in it.
;;
(defun seq3 (seq-prim behlist)
  `(lambda (t0)
     (setf first%sound (eval-seq-behavior ,(car behlist) "SEQ"))
     ,(progn (setf behlist (cdr behlist))
             (if (null behlist) 'first%sound
                 (seq2 seq-prim behlist)))))


; we have to use the real loop variable name since it could be
; referred to by the sound expression, so we avoid name collisions
; by using % in all the macro variable names
;
(defmacro seqrep (loop-control snd-expr)
  ;(tracemacro "SEQREP" (list loop-control snd-expr)
  `(let ((,(car loop-control) 0)
         (loop%count ,(cadr loop-control))
         (nyq%environment (nyq:the-environment))
         s%rate seqrep%closure)
     ; note: s%rate will tell whether we want a single or multichannel
     ; sound, and what the sample rates should be.
     (cond ((not (integerp loop%count))
            (error "bad argument type" loop%count))
           ((< loop%count 1)
            (snd-zero (local-to-global 0) *sound-srate*))
           ((= loop%count 1)
            ,snd-expr)
           (t ; more than 1 iterations
            (setf loop%count (1- loop%count))
            (setf first%sound ,snd-expr)
            (setf s%rate (get-srates first%sound))
            (setf nyq%environment (nyq:the-environment))
            (if (arrayp first%sound)
                (seqrep2 snd-multiseq ,loop-control ,snd-expr)
                (seqrep2 snd-seq ,loop-control ,snd-expr))))));)


(defmacro seqrep2 (seq-prim loop-control snd-expr)
  ;(tracemacro "SEQREP2" (list seq-prim loop-control snd-expr)
  `(progn (setf seqrep%closure
                (lambda (t0) ,(seqrep-iterate seq-prim loop-control snd-expr)))
          (,seq-prim (prog1 first%sound (setf first%sound nil))
                     seqrep%closure)));)


(defun seqrep-iterate (seq-prim loop-control snd-expr)
  (setf snd-expr `(eval-seq-behavior ,snd-expr "SEQREP"))
  `(progn
     (setf ,(car loop-control) (1+ ,(car loop-control))) ; incr. loop counter
     (if (>= ,(car loop-control) loop%count) ; last iteration 
         ,snd-expr
         (,seq-prim ,snd-expr seqrep%closure))))


;; TRIGGER - sums instances of beh which are launched when input becomes
;;     positive (> 0). New in 2021: input is resampled to *sound-srate*.
;;     As before, beh sample rates must match, so now they must also be
;;     *sound-srate*. This implementation uses eval-seq-behavior to create
;;     a more helpful stack trace for SAL.
(defmacro trigger (input beh)
  `(let* ((nyq%environment (nyq:the-environment))
          (s%rate *sound-srate*))
     (snd-trigger (force-srate *sound-srate* ,input)
                  #'(lambda (t0) (eval-seq-behavior ,beh "TRIGGER")))))


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
