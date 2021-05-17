;;************
;; writemake.lsp -- generate the sound create routine
;;************
;;************
;;           Change Log
;;  Date     | Change
;;-----------+--------------------
;; 17-Dec-91 | [1.1] <jmn> Created
;; 17-Dec-91 | [1.1] <jmn> return sound_create(...) cast type to correct
;;           | type
;; 21-Dec-91 | [1.2] <jmn> added start-time, default 0.0
;; 21-Dec-91 | [1.2] <jmn> prefix creation local variables with C_
;; 13-Jan-92 | [1.2] <jmn> reformatted and recommented
;;  3-May-99 | <rbd> modified toss_fetch code to retain proper t0
;;************



;; check-for-no-interpolation - if you see an "s", make sure there
;;     is a corresponding "n", if not use "s" to cover the "n" case. And vice versa.
;; 
(defun check-for-no-interpolation (encoding interpolation-rationale stream)
  ; *cfni-output* used to keep track of newline output
  (setf *cfni-output* nil)
  (check-for-no-interpolation-1 encoding 0 interpolation-rationale stream))

;; Hint: this algorithm constructs the 2^n variations by substituting
;; (or not) 'n' for 's' whereever s'es occur.  The search is cut off
;; however, when an altered string is found in the encoding-list,
;; which tells what cases are handled directly.
;;
;; Wow, returning to the description above after several months, I
;; couldn't make heads or tails of it, and I wrote it!  Here's another
;; perhaps better, description:
;; 
;; We generated various _fetch routines that differ in their
;; assumptions about how to access signal arguments.  There are (now)
;; 4 variations: NONE, SCALE, INTERP, and RAMP.  All 4^N combinations
;; of these are generated initially, but many combinations are deleted
;; before any code is generated.  Reasons for removing a combination
;; include the use of symetry, linearity, and simply the promise that
;; input arguments will be interpolated externally.  In most of these
;; cases, combinations are removed because they cannot occur in
;; practice.  But in others, combinations are removed because they
;; should be handled by different code.  For example, an input signal
;; matching the output sample rate and with a scale factor of 1 is
;; normally handled by NONE style "interpolation".  Note:
;; "interpolation" is used throughout this code, but a better term
;; would be "access method," because no interpolation is involved in
;; the NONE and SCALE variants. The inner loop access code for NONE
;; style is something like "*s++".  However, an input signal suitable
;; for NONE style interpolation can also be handled by SCALE style
;; interpolation (which looks something like "(*s++ * s_scale)", i.e.
;; an extra multiplication is required.  If the attribute
;; INTERNAL-SCALING is used, then the scale factor does not actually
;; appear at the access point because it has been factored into a
;; filter coefficient or some other factor, saving the multiply.
;; Alternatively, the ALWAYS-SCALE attribute can specify that there is
;; little to be gained by saving a multiply.  In these cases, we want
;; to handle NONE style signals with SCALE style interpolation.  Let's
;; run through these possibilities again and describe how they are
;; handled:
;;
;; ALWAYS-SCALE: here we delete the NONE variant(s) and only generate
;; fetch routines that have scaling code in them.  When we get an
;; actual parameter with a scale factor of 1 (implying NONE
;; interpolation), we handle it with the SCALE fetch routine.
;; INTERNAL-SCALING: here we generate NONE fetch routines because the
;; scale factor is taken care of elsewhere in the code, e.g. in a
;; filter coefficient.  LINEAR: here, the scale factor of the actual
;; argument becomes a scale factor on the output (part of the data
;; structure), deferring multiplies until later.  We then modify the
;; argument scale factor to 1, and NONE style interpolation applies.
;; There is no need to generate SCALE style routines, because there
;; will never be any need for them.
;;
;; For a given signal parameter, these 3 cases are mutually exclusive.
;;
;; Looking at these three cases, we see that sometimes there will be
;; SCALE style routines handling NONE arguments, sometimes NONE style
;; routines handling SCALE arguments, and sometimes NONE style
;; routines because there will never be a need for SCALE.  This code
;; is going to generate labels so that other fetch routines handle the
;; "missing" ones.  To do this, we generate extra labels in the case
;; statement that selects the fetch routine (interpolation is in the
;; inner loop in the fetch routine.  For example, we might generate
;; this code: ...  case INTERP_nn: case INTERP_sn: case INTERP_ns:
;; case INTERP_ss: susp->susp.fetch = tonev_ss_fetch; break; ...
;; Here, a single fetch routine (tonev_ss_fetch) handles all
;; variations of NONE and SCALE (n and s) types of the two signal
;; arguments.  The basic rule is: if you did not generate a fetch
;; routine for the NONE case, then handle it with the SCALE case, and
;; if you did not generate a fetch routine for the SCALE case, handle
;; it with the NONE case.
;; 
;; The algorithm uses the list interpolation-rationale, which lists
;; for each sound parameter one of {NIL, LINEAR, ALWAYS-SCALE,
;; INTERNAL-SCALING}.  Using this list, the code enumerates all the
;; possible cases that might be handled by the current fetch routine
;; (represented by the "encoding" parameter).  This is a recursive
;; algorithm because, if there are n SCALE type parameters, then there
;; are 2^N possible variations to enumerate.  (E.g. look at the 4
;; variations in the code example above.)
;;
;;
(defun check-for-no-interpolation-1 (encoding index 
                                     interpolation-rationale stream)
  (cond ((= index (length encoding))
         ; (display "check-for-no-interpolation output" encoding)
         ; see if we need a newline (*cfni-output* is initially nil)
         (if *cfni-output* (format stream "/* handled below */~%"))
         (setf *cfni-output* t)
         (format stream "      case INTERP_~A: " encoding))
        (t
         (let ((ch (char encoding index)))
           ; (display "cfni" index ch)
           (cond ((eql ch #\s)
                  (let ((new-encoding (strcat (subseq encoding 0 index)
                                              "n"
                                              (subseq encoding (1+ index)))))
                    (cond ((eq (nth index interpolation-rationale) 'ALWAYS-SCALE)
                           (check-for-no-interpolation-1 new-encoding (1+ index)
                                interpolation-rationale stream)))))
                 ((eql ch #\n)
                  (let ((new-encoding (strcat (subseq encoding 0 index)
                                              "s"
                                              (subseq encoding (1+ index)))))
                    (cond ((eq (nth index interpolation-rationale) 'INTERNAL-SCALING)
                           (check-for-no-interpolation-1 new-encoding (1+ index)
                                interpolation-rationale stream))))))
           (check-for-no-interpolation-1 encoding (1+ index)
                interpolation-rationale stream)))))
                              
;;************
;; is-argument -- see if string is in argument list
;;
;;************

(defun is-argument (arg arguments)
  (dolist (a arguments)
    (cond ((equal arg (cadr a)) (return t)))))



;; needs-mark-routine -- is there anything for GC to mark here?
;;
(defun needs-mark-routine (alg)
  (or (get-slot alg 'sound-names)
      (get-slot alg 'xlisp-pointers)))


;; lsc-needed-p -- see if we need the lsc variable declared
(defun lsc-needed-p (alg)
  (let ((spec (get-slot alg 'logical-stop)))
    (and spec (listp (car spec))
                  (eq (caar spec) 'MIN)
              (cdar spec)
              (cddar spec))))


;; write-initial-logical-stop-cnt -- writes part of snd_make_<name>
;;
(defun write-initial-logical-stop-cnt (alg stream)
  (let ((spec (get-slot alg 'logical-stop))
        min-list)
    (cond ((and spec (listp (car spec))
                    (eq (caar spec) 'MIN)
                (cdar spec))
           (setf min-list (cdar spec))
           ; take stop_cnt from first argument in MIN list
           (format stream
            "    susp->susp.log_stop_cnt = logical_stop_cnt_cvt(~A);\n"
            (symbol-to-name (cadar spec)))
           ; modify stop_cnt to be minimum over all remaining arguments
           (dolist (sym (cddar spec))
             (let ((name (symbol-to-name sym)))
               (format stream
                "    lsc = logical_stop_cnt_cvt(~A);\n" name)
               (format stream
                "    if (susp->susp.log_stop_cnt > lsc)\n"
                name)
               (format stream
                "        susp->susp.log_stop_cnt = lsc;\n"
                name))))
          (t
           (format stream
            "    susp->susp.log_stop_cnt = UNKNOWN;\n")))
))


;;************
;; write-mark
;;
;; Inputs:
;;      alg - algorithm description
;;      stream - stream on which to write .c file
;; Effect:
;;      writes NAME_mark(...)
;;************

(defun write-mark (alg stream)
  (let ((name (get-slot alg 'name))
        (sound-names (get-slot alg 'sound-names))
        (xlisp-pointers (get-slot alg 'xlisp-pointers)))
    ;----------------
    ; void NAME_mark(snd_susp_type a_susp)
    ; {
    ;     NAME_susp_type susp = (NAME_susp_type) a_susp;
    ; *WATCH*: printf("NAME_mark(%x)\n", susp);
    ;----------------
    (format stream "~%~%void ~A_mark(snd_susp_type a_susp)~%{~%" name)
    (format stream "    ~A_susp_type susp = (~A_susp_type) a_susp;~%"
            name name)
    (if *WATCH*
      (format stream
       "    printf(\"~A_mark(%x)\\n\", susp);~%" name))

    ;----------------
    ; for each LVAL argument:
    ;
    ; if (susp->NAME) mark(susp->NAME);
    ;----------------
    (dolist (name xlisp-pointers)
            (format stream "    if (susp->~A) mark(susp->~A);~%" name name))

    ;----------------
    ; for each sound argument:
    ;
    ; *WATCH*: printf("marking SND@%x in NAME@%x\n", susp->snd, susp);
    ; sound_xlmark(susp->NAME);
    ;----------------
    (dolist (snd sound-names)
            (if *watch*
              (format stream
               "    printf(\"marking ~A@%x in ~A@%x\\n\", susp->~A, susp);~%"
               snd name snd))
            (format stream "    sound_xlmark(susp->~A);~%" snd))

    ;----------------
    ; }
    ;----------------
    (format stream "}~%")))

(print 'write-mark)

;; in-set-of-srate-determiners
;;
;; We want to make sure no input has a sample rate higher than the output
;; sample rate. A test is generated, but sometimes the test is not
;; necessary. In particular, if the output sample rate is the max of
;; some input sample rates, we don't have to test those input sample rates.
;; This function tells whether the output sample rate is known to be as
;; high as that of name. This is true when name is an element of a MAX
;; expression in the SAMPLE-RATE property.
;;
;; name is one of sound-names
(defun in-set-of-srate-determiners (name sr)
  (display "in-set-of-srate-determiners" name sr)
  (or (null sr) ; no SAMPLE-RATE spec, so take max of all sounds
      (and (listp sr) (eq (car sr) 'MAX) ; explicit max expression
           (member name (cdr sr) :test
            #'(lambda (x y) (string-equal x (symbol-to-name y)))))))
         

;;************
;; out-of-line-interpolation -- determine if input sound should be
;;     interpolated using snd_up() unit generator
;;
;; Interpolate out-of-line if inline-interpolation is false either
;;   by default or by specification
;; If out-of-line-interpolation is true, then the signal should be
;;   either scaled internally or always scaled. Otherwise, we'll generate
;;   an extra implementation for scaling (S) vs non-scaling (N) which is
;;   a real waste given that we're willing to run a separate unit generator
;;   to do up-sampling. Therefore, this will raise an error.
;;************

(defun out-of-line-interpolation (alg name)
  (let ((ili *INLINE-INTERPOLATION*)
        (ili-spec (get alg 'inline-interpolation)))
    (if ili-spec (setf ili t))
    (if (eq ili-spec 'no) (setf ili nil))
    ;(display "out-of-line-interpolation" alg ili name
    ;         (get alg 'ALWAYS-SCALE) (get alg 'INTERNAL-SCALING))
;    (cond ((and (not ili) ;; make sure always scaled in some way
;                (not (member name (get alg 'LINEAR) :test
;                      #'(lambda (x y) (string-equal x (symbol-to-name y)))))
;                (not (member name (get alg 'ALWAYS-SCALE) :test
;                      #'(lambda (x y) (string-equal x (symbol-to-name y)))))
;                (not (member name (get alg 'INTERNAL-SCALING) :test
;                      #'(lambda (x y) (string-equal x (symbol-to-name y))))))
;           (error (format nil "~A is not always scaled" name))))
    (not ili)))

;; a signal needs out-of-line scaling if out-of-line interpolation is
;; in effect and there is no built-in scaling (ALWAYS-SCALE or
;; INTERNAL-SCALING)
;;
(defun needs-out-of-line-scaling (alg name)
  (let ((ili *INLINE-INTERPOLATION*)
        (ili-spec (get alg 'inline-interpolation)))
    (if ili-spec (setf ili t))
    (if (eq ili-spec 'no) (setf ili nil))
    (and (not ili)
         (not (member name (get alg 'LINEAR) :test
                      #'(lambda (x y) (string-equal x (symbol-to-name y)))))
         (not (member name (get alg 'ALWAYS-SCALE) :test
                      #'(lambda (x y) (string-equal x (symbol-to-name y)))))
         (not (member name (get alg 'INTERNAL-SCALING) :test
                      #'(lambda (x y) (string-equal x (symbol-to-name y))))))))


;;************
;; write-make
;;
;; Inputs:
;;      alg - algorithm description
;;      stream - stream on which to write .c file
;; Effect:
;;      writes NAME_free(...), NAME_print_tree,  and snd_make_NAME(...)
;;************

(defun write-make (alg stream)
  (let ((name (get-slot alg 'name))
        (sr (get-slot alg 'sample-rate))
        (else-prefix "")
        first-time
        (sound-names (get-slot alg 'sound-names))
        (logical-stop (car (get-slot alg 'logical-stop)))
        (sound-to-name (get-slot alg 'sound-to-name))
        (state-list (get-slot alg 'state))
        (linear (get-slot alg 'linear))
        (arguments (get-slot alg 'arguments))
        (finalization (get-slot alg 'finalization))
        (interpolation-list (get-slot alg 'interpolation-list))
        (interpolation-rationale (get-slot alg 'interpolation-rationale))
        encoding-list
        (terminate (car (get-slot alg 'terminate)))
        (type-check (car (get-slot alg 'type-check)))
        (delay (get-slot alg 'delay))
        (start (get-slot alg 'start)))

    ;--------------------
    ; void NAME_free(snd_susp_type a_susp)
    ; {
    ;     NAME_susp_type susp = (NAME_susp_type) a_susp;
    ;----------------
    (format stream "~%~%void ~A_free(snd_susp_type a_susp)~%{~%"
            name)
    (format stream "    ~A_susp_type susp = (~A_susp_type) a_susp;~%"
            name name)

    ;----------------
    ; if there's a finalization, do it
    ;----------------
    (if finalization (print-strings finalization stream))

    ;----------------
    ; for each sound argument:
    ;
    ; sound_unref(susp->NAME);
    ;----------------
    (dolist (name sound-names)
      (format stream "    sound_unref(susp->~A);~%" name))

    ;----------------
    ;     ffree_generic(susp, sizeof(NAME_susp_node), "fn-name");
    ; }
    ;--------------------
    (format stream 
            "    ffree_generic(susp, sizeof(~A_susp_node), \"~A_free\");~%}~%"
            name name)

    ;--------------------
    ; void NAME_print_tree(snd_susp_type a_susp, int n)
    ; {
    ;     NAME_susp_type susp = (NAME_susp_type) a_susp;
    ;----------------
    (format stream "~%~%void ~A_print_tree(snd_susp_type a_susp, int n)~%{~%"
            name name)
    (cond (sound-names
           (format stream "    ~A_susp_type susp = (~A_susp_type) a_susp;~%"
            name name)))
    ;----------------
    ; for each sound argument:
    ;
    ; indent(n);
    ; printf("NAME:");
    ; sound_print_tree_1(susp->NAME, n);
    ;----------------
    (setf first-time t)
    (dolist (name sound-names)
      (cond (first-time
             (setf first-time nil))
            (t  ; space between each iteration
             (format stream "~%")))
      (format stream "    indent(n);~%    stdputstr(\"~A:\");~%" name)
      (format stream "    sound_print_tree_1(susp->~A, n);~%" name))

    ;----------------
    ; }
    ;--------------------
    (format stream "}~%")

    ;--------------------
    ;   sound_type snd_make_NAME
    ;--------------------

    (format stream "~%~%sound_type snd_make_~A" name)

    ;--------------------
    ;    ( type name, ...)
    ;--------------------

    (write-ansi-parameter-list stream "" arguments)
    (format stream "~%")
    (if (not *ANSI*)
        (dolist (arg arguments)
          (format stream "  ~A ~A;~%" (car arg) (cadr arg))))

    ;--------------------
    ;     NAME_susp_type susp;
    ;--------------------
    (format stream 
     "{~%    register ~A_susp_type susp;~%" name);

    ;; declare "state" variables with TEMP flag
    ;--------------------
    ;     <type[i]> <name[i]>;
    ;--------------------
    (dolist (state state-list)
      (cond ((and (cdddr state)
                  (cadddr state)
                  (eq (cadddr state) 'TEMP))
             (format stream "    ~A ~A;~%" 
                (car state) (cadr state)))))

    (write-sample-rate stream sr sound-names arguments)

    ; compute the t0 for new signal (default: use zero): 
    ;
    (write-start-time stream start arguments)

    ;--------------------
    ;    int interp_desc = 0;
    ;--------------------
    (cond ((< 1 (length interpolation-list))
           (format stream "    int interp_desc = 0;~%")))

    ;--------------------
    ;     sample_type scale_factor = 1.0F;
    ;  time_type t0_min; -- but only if there are sound args, implied by non-null sound-names
    ;  int64_t lsc;  
    ;--------------------
    (format stream "    sample_type scale_factor = 1.0F;~%")
    (if sound-names (format stream "    time_type t0_min = t0;~%"))
    (if (lsc-needed-p alg)
        (format stream "    int64_t lsc;~%"))

    ; now do canonical ordering of commutable sounds
    ;
    (dolist (lis (get-slot alg 'commutative))
      ;--------------------
      ;   /* sort commuative signals: s1 s2 ... */
      ;   snd_sort_<n>
      ;         (...)
      ;--------------------
      (format stream "    /* sort commutative signals: ~A */~%" lis)
      (format stream "    snd_sort_~A" (length lis))
      (write-parameter-list stream ""
        (append (mapcar 
                  '(lambda (snd) 
                    (strcat "&" (cdr (assoc snd sound-to-name))))
                  lis)
                '("sr")))
      (format stream ";~%~%"))

    ; figure scale factor -- if signal is linear wrt some interpolated or
    ;   ramped signal (which do the multiply anyway), then put the scale
    ;   factor there.
    ;--------------------
    ;  /* combine scale factors of linear inputs <linear> */
    ;--------------------

    (cond (linear
           (format stream 
            "    /* combine scale factors of linear inputs ~A */~%" linear)))
    ;--------------------
    ;  scale_factor *= NAME ->scale;
    ;  NAME ->scale = 1.0F;
    ;--------------------

    (dolist (snd linear)
      (let ((name (cdr (assoc snd sound-to-name))))
        (format stream "    scale_factor *= ~A->scale;~%" name)
        (format stream "    ~A->scale = 1.0F;~%" name)))

    ;--------------------
    ;  /* try to push scale_factor back to a low sr input */
    ;--------------------

    (cond (linear
           (format stream 
            "~%    /* try to push scale_factor back to a low sr input */~%")))

    ;--------------------
    ; if (NAME ->sr < sr) {
    ;     NAME ->scale = scale_factor; scale_factor = 1.0F; }
    ;--------------------

    (dolist (snd linear)
      (let ((name (cdr (assoc snd sound-to-name))))
        (format stream 
"    ~Aif (~A->sr < sr) { ~A->scale = scale_factor; scale_factor = 1.0F; }~%"
         else-prefix name name)
        (setf else-prefix "else ")))
    (if linear (format stream "~%"))

    ;-------------------
    ; insert TYPE-CHECK code here
    ;-------------------
    ; (display "write-make" type-check)
    (if type-check
      (format stream type-check))

    ;--------------------
    ; falloc_generic(susp, NAME_susp_node, "fn-name"); 
    ;--------------------
    (format stream 
     "    falloc_generic(susp, ~A_susp_node, \"snd_make_~A\");~%" name name)

    ;; initialize state: the state list has (type field initialization [temp])
    ;--------------------
    ;    susp-> <state[i]> = <value[i]>
    ;--------------------
    ;; if TEMP is present, generate:
    ;--------------------
    ;    <state[i]> = <value[i]>
    ;--------------------

    (dolist (state state-list)
      (let ((prefix "susp->"))
        (cond ((and (cdddr state)
                    (cadddr state)
                    (eq (cadddr state) 'TEMP))
               (setf prefix "")))
        (format stream "    ~A~A = ~A;~%" 
                prefix (cadr state) (caddr state))))

    ;---------------------
    ; /* make sure no sample rate is too high */
    ; if (***->sr > sr) {
    ;    sound_unref(***);
    ;    snd_badsr();
    ; } [maybe there will be an else part here]
    ;---------------------
    ; where *** is any sound input that is not in the max clause of
    ; the SAMPLE-RATE attribute
    ;
    (setf first-time t)
    (dolist (name sound-names)
      (let (too-high-test ;; did we test for sr too high (follow with else)
            (srate-determiner (in-set-of-srate-determiners name sr)))
        (cond ((not srate-determiner)
               (cond (first-time
                      (setf first-time nil)
                      (format stream
                       "~%    /* make sure no sample rate is too high */~%")))
               (format stream "    if (~A->sr > sr) {~%" name)
               (format stream "        sound_unref(~A);~%" name)
               (format stream "        snd_badsr();~%" name)
               (format stream "    }")
               (setf too-high-test t)
    ;---------------------
    ; Add this if signal needs out-of-line resampling
    ;  else if (***->sr < sr) *** = snd_make_up(sr, ***);
    ;---------------------
               (cond ((out-of-line-interpolation alg name)
                      (format stream (if too-high-test " else " "    "))
                      (format stream "if (~A->sr < sr) " name)
                      (format stream "~A = snd_make_up(sr, ~A);" name name)))
               (format stream "~%")))
    ;---------------------
    ; Add this if signal needs out-of-line rescaling
    ; if (***->scale != 1.0) *** = snd_make_normalize(***);
    ;---------------------
        (cond ((needs-out-of-line-scaling alg name)
               (format t "WARNING: out-of-line scaling possible for ~A\n" name)
               (format stream "    if (~A->scale != 1.0F) " name)
               (format stream "~A = snd_make_normalize(~A);~%" name name)))))

    ; (display "write-make select implementation" interpolation-list)
    ; if we have a choice of implementations, select one
    (cond ((< 1 (length interpolation-list))

           ;--------------------
           ; /* select a susp fn based on sample rates */
           ;--------------------
           ; build a descriptor
           (format stream 
                "~%    /* select a susp fn based on sample rates */~%")

           ;------------------------
           ;  interp_desc = (interp_desc << 2) + interp_style( NAME, sr);
           ;------------------------
           (dolist (snd sound-names)
             (format stream
              "    interp_desc = (interp_desc << 2) + interp_style(~A, sr);~%"
              snd))

           ;--------------------
           ;     switch (interp_desc) {
           ;--------------------
           (cond (interpolation-list
                  (format stream "    switch (interp_desc) {~%")))

           ;--------------------------
           ;         case INTERP_<encoding>: susp->susp.fetch = 
           ;                                 NAME_<encoding>_fetch; break;
           ;--------------------------
           (setf encoding-list (mapcar #'encode interpolation-list))
           (dolist (encoding encoding-list)
             (check-for-no-interpolation encoding interpolation-rationale stream)
             (format stream "susp->susp.fetch = ~A_~A_fetch; break;~%"
              name encoding))
           ;--------------------------
           ;     default: snd_badsr(); break;
           ;--------------------------
           (format stream "      default: snd_badsr(); break;~%")
           ;--------------------
           ;        } /* initialize susp state */
           ;-------------------------
           (format stream "    }~%~%"))
          (interpolation-list
           (format stream "    susp->susp.fetch = ~A_~A_fetch;~%" 
            name (encode (car interpolation-list))))
          (t
        ;-------------------------
        ;   susp->susp.fetch = NAME__fetch;
        ;-------------------------
           (format stream "    susp->susp.fetch = ~A__fetch;~%~%" name)))
 
    ;----------------
    ; /* compute terminate count */
    ;----------------
    (cond ((terminate-check-needed terminate alg)
           (cond ((eq (car terminate) 'AT)
                  (let ((time-expr (cadr terminate)))
        ;----------------
        ; susp->terminate_cnt = check_terminate_cnt(ROUNDBIG(((TIME-EXPR) - t0) * sr));
        ;----------------
                    (format stream 
                     "    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG(((~A) - t0) * sr));~%"
                            time-expr)))
                 ((eq (car terminate) 'AFTER)
                  (let ((dur-expr (cadr terminate)))
                    ;----------------
                    ; susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((DUR-EXPR) * sr));
                    ;----------------
                    (format stream 
                            "    susp->terminate_cnt = check_terminate_cnt(ROUNDBIG((~A) * sr));~%"
                            dur-expr)))
                 (t
                  ;----------------
                  ; susp->terminate_cnt = UNKNOWN;
                  ;----------------
                  (format stream "    susp->terminate_cnt = UNKNOWN;~%")))))

    ;----------------
    ;    /* handle unequal start times, if any */
    ;----------------
    (if sound-names
      (format stream "    /* handle unequal start times, if any */~%"))
    ;----------------
    ; for each sound argument:
    ;    if (t0 < NAME->t0) sound_prepend_zeros(NAME, t0);
    ;----------------
    (dolist (name sound-names)
      (format stream 
       "    if (t0 < ~A->t0) sound_prepend_zeros(~A, t0);~%" name name))
    ;----------------
    ; t0_min = min(NAME1->t0, min(NAME2->t0, ... NAMEn->t0, t0)...);
    ;----------------
    (cond (sound-names
           (format stream "    /* minimum start time over all inputs: */~%")
           (format stream "    t0_min = ")
           (dolist (name sound-names)
             (format stream "min(~A->t0, " name))
           (format stream "t0")
           (dolist (name sound-names)
             (format stream ")"))
           (format stream ";~%")))

    ;----------------
    ;    /* how many samples to toss before t0: */
    ;    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + <DELAY>.5);
    ;    if (susp->susp.toss_cnt > 0) {
    ;   susp->susp.keep_fetch = susp->susp.fetch;
    ;   susp->susp.fetch = NAME_toss_fetch;
    ;   t0 = t0_min; -- DELETED 3MAY99 by RBD
    ;    }
    ;----------------
    (cond (sound-names
           (format stream "    /* how many samples to toss before t0: */\n")
           (if delay
             (format stream "    /* Toss an extra ~A samples to make up for internal buffering: */\n" delay))
           (format stream "    susp->susp.toss_cnt = (long) ((t0 - t0_min) * sr + ~A.5);\n"
                   (if delay delay 0))
           (format stream "    if (susp->susp.toss_cnt > 0) {\n")
           (format stream "        susp->susp.keep_fetch = susp->susp.fetch;\n")
           (format stream "        susp->susp.fetch = ~A_toss_fetch;~%" name)
;          (format stream "        t0 = t0_min;~%    }\n\n")))
           (format stream "    }\n\n")))

    ;--------------------
    ; /* initialize susp state */
    ; susp->susp.free = NAME_free;
    ; susp->susp.sr = sr;
    ; susp->susp.t0 = t0;
    ;--------------------
    (format stream "    /* initialize susp state */~%")
    (format stream "    susp->susp.free = ~A_free;~%" name)
    (format stream "    susp->susp.sr = sr;~%")
    (format stream "    susp->susp.t0 = t0;~%")

    ;----------------
    ; if there are sound arguments:
    ;     susp->susp.mark = NAME_mark;
    ; otherwise...
    ;     susp->susp.mark = NULL;
    ;----------------
    (let ((value "NULL"))
      (cond ((needs-mark-routine alg)
             (setf value (strcat name "_mark"))))
      (format stream "    susp->susp.mark = ~A;~%" value))

    ;----------------
    ; for debugging...
    ;    susp->susp.print_tree = NAME_print_tree;
    ;    susp->susp.name = "NAME";
    ;----------------
    (format stream "    susp->susp.print_tree = ~A_print_tree;~%" name)
    (format stream "    susp->susp.name = \"~A\";~%" name)

    ;----------------
    ; if there is a logical stop attribute:
    ; susp->logically_stopped = false;
    ; susp->susp.log_stop_cnt = UNKNOWN;
    ;----------------
    (cond ((logical-stop-check-needed logical-stop)
           (format stream 
            "    susp->logically_stopped = false;\n")))
    (write-initial-logical-stop-cnt alg stream)

    ;--------------------
    ; ramped or interpolated:
    ;
    ;   susp->started = false;
    ;--------------------
    (cond ((any-ramp-or-interp-in interpolation-list)
           (format stream "    susp->started = false;~%")))

    ;--------------------
    ;   susp->susp.current = 0;
    ;--------------------
    (format stream "    susp->susp.current = 0;~%")

    ;----------------------------
    ; For each sound arg:
    ;
    ;     susp-> <arg> = <arg>;
    ;     susp-> <arg>_cnt = 0;
    ;----------------------------

    (dotimes (n (length (get alg 'sound-args)))
      (let ((interpolation (union-of-nth interpolation-list n)))
        (setf arg (nth n sound-names))  ; get name of signal
        (format stream "    susp->~A = ~A;~%" arg arg)
        (format stream "    susp->~A_cnt = 0;~%" arg)
        ;-----------------------------------------------
        ; Interpolation: 
        ;
        ;         susp-> <arg>_pHaSe = 0.0;               
        ;         susp-> <arg>_pHaSe_iNcR = <arg> ->sr    
        ;-----------------------------------------------
        (cond ((member 'INTERP interpolation)
               (format stream "    susp->~A_pHaSe = 0.0;~%" arg)
               (format stream "    susp->~A_pHaSe_iNcR = ~A->sr / sr;~%"
                              arg arg)))
        ;-----------------------------------------------
        ; Ramp: 
        ;
        ;    susp->output_per_<arg> = <arg> ->sr;
        ;-----------------------------------------------

        (cond ((member 'RAMP interpolation)
               (format stream "    susp->~A_n = 0;~%" arg)
               (format stream "    susp->output_per_~A = sr / ~A->sr;~%"
                              arg arg)))))
    
    ;----------------------------
    ;   return sound_create (snd_susp_type)susp, t0, sr, scale_factor);
    ;----------------------------

    (format stream 
     "    return sound_create((snd_susp_type)susp, t0, sr, scale_factor);~%}~%")))


(print 'write-make)

;;************
;; write-parameter-list -- with comma separator, open and close parens
;;
;;************

(defun write-parameter-list (stream prefix strings)
  (let ((comma ""))
    (format stream "(")
    (dolist (parm strings)
      (format stream "~A~A~A" comma prefix parm)
      (setf comma ", "))
    (format stream ")")))

;;************
;; write-ansi-prototype-list -- with comma separator, open and close parens
;;
;; Inputs:
;;      stream - output stream
;;      prefix - arg prefix, perhaps ""
;;      args - argument type/name pairs of the form
;;              ( (type1 name1) (type2 name2) ... )
;; Effect:
;;      if *ANSI* is set T, writes ANSI-style parameter list of the form
;;              type name, ...
;;      if *ANSI* is set NIL, writes antique-style parameter list of the form
;;              ()
;;************

(defun write-ansi-prototype-list (stream prefix args)
  (let ((comma ""))
    (format stream "(")
    (if *ANSI*
       (dolist (parm args)
          ;--------------------
          ; for each parameter
          ;     <comma>type <prefix><parm>
          ;--------------------
          (format stream "~A~A ~A~A" comma (car parm) prefix (cadr parm))
          (setf comma ", "))
    )
    (format stream ")")))

;;************
;; write-ansi-parameter-list
;;
;; Inputs:
;;      stream - output stream
;;      prefix - arg prefix, perhaps ""
;;      args - argument type/name pairs of the form
;;              ( (type1 name1) (type2 name2) ... )
;; Effect:
;;      if *ANSI* is set T, writes ANSI-style parameter list of the form
;;              (type name, ...)
;;      if *ANSI* is set NIL, writes antique-style parameter list of the form
;;              (name, ...)
;; Note:
;;  to get a space between types and arguments, a space is prepended to prefix if
;; this is an *ANSI* arg list.
;;************

(defun write-ansi-parameter-list (stream prefix args)
  (let ((comma ""))
    (format stream "(")
    (cond (*ANSI*
           (setf prefix (strcat " " prefix))))
    (dolist (parm args)
            (format stream "~A~A~A~A" comma 
                            (if *ANSI* (car parm) "")
                        prefix (cadr parm))
            (setf comma ", ")
    )
    (format stream ")")))

;;************
;; write-sample-rate
;; Effect:
;;      declare sr and compute the sample rate for the new signal
;; Notes:
;;      If sr is an input parameter, it is not declared
;;      If (SAMPLE-RATE expr) is specified, declare sr to be initialized
;;         to the expr
;;      If (SAMPLE-RATE (MAX s1 s2 ...)), sr is initialized to the max.
;;      Otherwise, sr is initialized to the max of the sample rates of
;;      all the sound-type arguments    
;;************

(defun write-sample-rate (stream sr sound-names arguments)
    ;; if sr is "sr" and "sr" is a parameter, then do nothing:

    ; (display "write-sample-rate: " sr sound-names arguments)

    (cond ( (and (equal sr "sr") (is-argument "sr" arguments))
            ;---------------------
            ;   /* sr specified as input parameter */
            ;---------------------
                (format stream "    /* sr specified as input parameter */~%")
          )
    ;; else if sample rate is specified, use it to initialize sr:
          ((stringp sr)
           ; (display "write-sample-rate: using specified sr" sr)
            ;---------------------
            ;   rate_type sr = <sr>;
            ;---------------------
           (format stream "    rate_type sr = ~A;~%" sr)
          )
    ;; else look for (MAX ...) expression
          ((and (listp sr) (eq (car sr) 'MAX))
           (format stream "    rate_type sr = ")
           (write-redux-of-names stream "max" 
                (mapcar #'symbol-to-name (cdr sr)) "->sr")
           (format stream ";~%")
          )
    ;; else assume sr is max of sr's of all sound arguments
          (sound-names
            ;---------------------
            ;   rate_type sr = max( <arg[0]> ->sr, <arg[i]> ->sr);
            ;---------------------
           (format stream "    rate_type sr = ")        ; jmn
           (write-redux-of-names stream "max" sound-names "->sr")
           (format stream ";~%")
          )
           (t
            (error "Missing SAMPLE-RATE specification."))
    )
)
 

(defun write-redux-of-names (stream fn sound-names suffix)
  (dotimes (n (1- (length sound-names)))
           (format stream "~A(" fn))
  (format stream "~A~A" (car sound-names) suffix)
  (dolist (snd (cdr sound-names))
          (format stream ", ~A~A)" snd suffix)))



;;************
;; write-start-time
;; Effect:
;;      declare sr and compute the start time for the new signal
;; Notes:
;;      If t0 is an input parameter, it is not declared
;;      If (START (AT expr)) is specified, declare t0 to be initialized
;;         to the expr
;;      Otherwise, t0 is initialized to 0
;;************

(defun write-start-time (stream start arguments)
  ;; if t0 is "t0" and "t0" is a parameter, then do nothing:
  ; (display "write-start time:" start arguments)
  (cond ((is-argument "t0" arguments)
         ;---------------------
         ;   /* t0 specified as input parameter */
         ;---------------------
             (format stream "    /* t0 specified as input parameter */~%"))
    ;; else if start time is specified, use it to initialize sr:
        (t (cond (start
                  ;---------------
                  ;   (START (AT <expr>)) specified:
                  ;
                  ;   time_type t0 = <expr>;
                  ;---------------
                  (setf start (car start))
                  (cond ((eq (car start) 'AT)
                         (format stream "    time_type t0 = ~A;~%" (cadr start)))
                        ((eq (car start) 'MIN)
                         (format stream "    time_type t0 = ")
                         (write-redux-of-names stream "min" 
                                               (c-names (cdr start)) "->t0")
                         (format stream ";~%"))
                        ((eq (car start) 'MAX)
                         (format stream "    time_type t0 = ")
                         (write-redux-of-names stream "max" 
                                               (c-names (cdr start)) "->t0")
                         (format stream ";~%"))
                        (t (error (format nil
                            "Unrecognized START specification ~A" start)))))
                ;---------------
                ;   time_type t0 = 0.0;
                ;---------------
                (t (format stream "    time_type t0 = 0.0;~%"))))))


;; c-names -- get the C names corresponding to list of symbols
;;
(defun c-names (syms) (mapcar '(lambda (sym) (string-downcase (symbol-name sym))) syms))

(defun is-table (alg snd)
  (dolist (table (get-slot alg 'table))
    (cond ((equal snd table)
           ; (display "is-table" snd table)
           (return t)))))

 
;; write-xlmake -- write out a function snd_NAME to be called by xlisp
;
; this function copies any sound arguments and passes them on to snd_make_NAME
;
(defun write-xlmake (alg stream)
  (let ((name (get-slot alg 'name))
        (sound-names (get-slot alg 'sound-names))
        (arguments (get-slot alg 'arguments))
        comma)
    ;--------------------
    ;   sound_type snd_NAME
    ;--------------------

    (format stream "~%~%sound_type snd_~A" name)

    ;--------------------
    ;    ( type name, ...)
    ;    {
    ;--------------------

    (write-ansi-parameter-list stream "" arguments)
    (format stream "~%")
    (if (not *ANSI*)
        (dolist (arg arguments)
          (format stream "  ~A ~A;~%" (car arg) (cadr arg))))
    (format stream "{~%")

    ;----------------
    ; for each sound argument that is not a table
    ;     sound_type SND_copy = sound_copy(SND);
    ;----------------

    (dolist (arg arguments)
      (cond ((equal (car arg) "sound_type")
             (let ((snd (cadr arg)))
                (cond ((not (is-table alg snd))
                       (format stream 
                               "    sound_type ~A_copy = sound_copy(~A);~%"
                               snd snd)))))))

    ;----------------
    ; now call snd_make_ALG. When SND is a sound_type that is not a table,
    ;  substitute SND_copy for SND.
    ;----------------

    (format stream "    return snd_make_~A(" name)
    (setf comma "")
    (dolist (arg arguments)
      (let ((suffix ""))
        (cond ((and (equal (car arg) "sound_type")
                    (not (is-table alg (cadr arg))))
               (setf suffix "_copy")))
        (format stream "~A~A~A" comma (cadr arg) suffix)
        (setf comma ", ")))
    (format stream ");~%}~%")))
