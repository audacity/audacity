;;************
;;          Change Log
;; Date     | Change
;;----------+---------------------
;; 17-Dec-91 | [1.1] <jmn> Created
;; 17-Dec-91 | [1.1] <jmn> cast arg of snd_list_create to correct type
;; 17-Dec-91 | [1.1] <jmn> cast truncation as (int) explicitly, avoid lint
;;           | errors
;; 13-Jan-92 | [1.2] <jmn> reformatted and recommented
;;************

;;****************
;; depended-on-in-inner-loop - test if variables updated in inner loop
;;****************
(defun depended-on-in-inner-loop (vars interp sound-names step-function)
  (dotimes (n (length interp))
    (let ((method (nth n interp))
          (name (nth n sound-names))
          interpolate-samples)
      (setf interpolate-samples
            (not (member (name-to-symbol name) step-function)))
      (cond ((and (or (member method '(NONE SCALE INTERP))
                      interpolate-samples)
                  (member name vars :test #'equal))
             (return t))))))

;;****************
;; fixup-depends-prime - write code to update depend variables
;;      this code is only run the first time the suspension
;;      is invoked
;;****************
(defun fixup-depends-prime (alg stream name indent var-name)
  (let ((depends (get-slot alg 'depends)))
    (dolist (dep depends)
      (cond ((equal name (cadr dep))
             (cond ((eq (cadddr dep) 'TEMP)
                    (format stream "~A~A = ~A;~%" indent (car dep)
                        (fixup-substitutions-prime alg
                                (caddr dep) name var-name)))
                   (t
                    (format stream "~Asusp->~A = ~A;~%" indent (car dep)
                        (fixup-substitutions-prime alg
                                (caddr dep) name var-name)))))))))

(print 'fixup-depends-prime)


;;****************
;; fixup-depends-prime-decls - write declarations for temp depend variables
;;      this code is only run the first time the suspension
;;      is invoked
;;****************
(defun fixup-depends-prime-decls (alg stream name)
  (let ((depends (get-slot alg 'depends)))
    (dolist (dep depends)
      (cond ((equal name (cadr dep))
             (cond ((eq (cadddr dep) 'TEMP)
                    (format stream "            ~A ~A;~%" (car (cddddr dep))
                                   (car dep)))))))))

(print 'fixup-depends-prime-decls)


;;****************
;; fixup-substitutions-prime - substitute susp-><var> for <var> for each 
;;      state variable in code, also substitute var-name for name
;;      (this is the depended-on value)
;;****************
(defun fixup-substitutions-prime (alg code name var-name)
  (dolist (state (get-slot alg 'state))
    (let ((var (cadr state)))
      (setf code (substitute code var (strcat "susp->" var) t))))
  (if name (setf code (substitute code name var-name nil)))
  code)

(print 'fixup-substitutions-prime)

;; fixup-substitutions-for-depends is used to prepare joint-dependency
;; code for use outside the inner loop. In this position, the state
;; variables must be accessed using "susp-><name>" and signals must
;; be accessed using the local variable <name>_val
;;
(defun fixup-substitutions-for-depends (alg code)
  (setf code (fixup-substitutions-prime alg code nil nil))
  (let ((interp (get alg 'interpolation))
        (step-function (get-slot alg 'step-function))
        (sound-names (get-slot alg 'sound-names)))
    (dotimes (n (length interp))
       ;(display "fixup-loop" n name interp sound-names)
       (let* ((name (nth n sound-names))
              (method (nth n interp))
              (is-step (member (name-to-symbol name) step-function)))
         (cond ((and is-step (eq method 'RAMP))
                (setf code (substitute code name (strcat name "_val") t))
                ;(display "fixup-check" name)
               ))))
    code))



;;****************
;; fixup-depends - write code to declare and update depend variables
;;      this is called at declaration time (the point where 
;;      declarations should be output), but also generates code
;;      to be output after the depended-on variable is updated
;;****************
(defun fixup-depends (alg stream name)
  (format stream "/* fixup-depends ~A */~%" name)
  (let ((depends (get-slot alg 'depends))
        (fixup-code "")
        (var-name (strcat name "_x1_sample_reg")))
    (dolist (dep depends)
      (cond ((equal name (cadr dep))
             (cond ((eq (cadddr dep) 'TEMP)
                    (format stream
                     "                ~A ~A; ~%" (car (cddddr dep)) (car dep))
                    (setf fixup-code
                          (format nil "~A                ~A = ~A;~%"
                                  fixup-code (car dep) 
                                  (fixup-substitutions alg
                                    (caddr dep) name var-name))))
                   (t
                    (setf fixup-code
                          (format nil "~A                ~A_reg = ~A;~%"
                                  fixup-code (car dep)
                                  (fixup-substitutions alg
                                   (caddr dep) name var-name))))))))
    (put-slot alg fixup-code 'fixup-code)))

(print 'fixup-depends)


;;****************
;; fixup-substitutions - substitute <var>_reg for <var> for each 
;;      state variable in code, also substitute var-name for name
;;      (this is the depended-on value)
;;****************
(defun fixup-substitutions (alg code name var-name)
  (dolist (state (get-slot alg 'state))
    (let ((var (cadr state)))
      (setf code (substitute code var (strcat var "_reg") t))))
  (substitute code name var-name nil))

(print 'fixup-substitutions)


;;****************
;; in-min-list - see if name is in TERMINATE MIN list or
;;   LOGICAL-STOP MIN list
;;
;; returns true if algorithm specified, say (TERMINATE (MIN s1 s2 s3)) and
;; name is, say, "s2".
;; NOTE: name is a string, so we have to do a lookup to get the symbol name
;;****************
(defun in-min-list (name alg terminate-or-logical-stop)
  (let ((spec (get alg terminate-or-logical-stop)))
;    (display "in-min-list" name alg terminate-or-logical-stop spec)
    (and spec
         (listp (car spec))
         (eq (caar spec) 'MIN)
         (member (name-to-symbol name) (cdar spec)))))


;;****************
;; logical-stop-check-needed -- says if we need to check for logical stop
;;    after the outer loop
;; the argument is the logical-stop clause from the algorithm prop list
;;****************
(defun logical-stop-check-needed (logical-stop)
  (cond ((and logical-stop
              (listp logical-stop)
              (or (eq (car logical-stop) 'MIN)
                  (eq (car logical-stop) 'AT))))))


;;****************
;; susp-check-fn -- find fn to check need for new block of samples
;;
;; To simply check if susp->S_ptr points to something, you call 
;; susp_check_samples(S, S_ptr, S_cnt), but during this check, it is
;; also necessary to check for termination condition and logical stop
;; condition, BUT ONLY if S is in a MIN list for the TERMINATE or
;; LOGICAL-STOP attributes (i.e. this signal stops when S does).
;;
;; The algorithm is: if S is on the LOGICAL-STOP MIN list and on
;; the TERMINATE MIN list, then call susp_check_term_log_samples.
;;Otherwise if S is on the LOGICAL-STOP MIN list then call
;; susp_check_log_samples.  Otherwise, if S is on the TERMINATE MIN
;; list, call susp_check_term_samples.  The "normal" case should be 
;; susp_check_term_samples, which happens when the LOGICAL-STOP
;; MIN list is empty (nothing was specified).  Note that a signal logically
;; stops at termination time anyway, so this achieves the logically stopped
;; condition with no checking.
;;****************
(defun susp-check-fn (name alg)
  (let ((in-log-list (in-min-list name alg 'logical-stop))
        (in-term-list (in-min-list name alg 'terminate)))
    (cond ((and in-log-list in-term-list)
           "susp_check_term_log_samples")
          (in-log-list
           "susp_check_log_samples")
          (in-term-list
           "susp_check_term_samples")
          (t
           "susp_check_samples"))))


;;************
;; write-depend-decls -- declare TEMP depends variables
;;
;;************
;(defun write-depend-decls (alg stream)
;  (dolist (dep (get-slot alg 'depends))
;    (cond ((eq (cadddr dep) 'TEMP)
;          (format stream "        ~A ~A; ~%" (car (cddddr dep)) (car dep))))))
;--------

(defun write-depend-decls (alg stream interp sound-names step-function)
  (dotimes (n (length interp))
    (let ((name (nth n sound-names))
          (method (nth n interp))
          is-step)
      (cond ((eq method 'INTERP)
             (setf is-step (member (name-to-symbol name) step-function))
             (cond (is-step
                    (fixup-depends-prime-decls alg stream name))))))))


;;************
;; write-prime -- write conditional code to prime input sounds and susp
;;
;;************
(defun write-prime (alg stream interp sound-names)
 (let ((step-function (get-slot alg 'step-function))
       (internal-scaling (get-slot alg 'internal-scaling)))
  ;------------------------------
  ;   /* make sure sounds are primed with first values */
  ;------------------------------
  (format stream "~%    /* make sure sounds are primed with first values */~%")

  ;------------------------------
  ;   if  (!susp->started) {
  ;       susp->started = true;
  ;------------------------------

  (format stream "    if (!susp->started) {~%")
  ; this is generating extraneous declarations, is it necessary?
  ; yes, at least sometimes, so we're leaving it in
  ; "atonev.alg" is a good test case to prove you can't comment this out
  (write-depend-decls alg stream interp sound-names step-function)
  (format stream "        susp->started = true;~%")

  ;------------------------------
  ; for each method
  ;------------------------------
  (dotimes (n (length interp))
    (let ((name (nth n sound-names))
          (method (nth n interp))
          is-step)
      (cond ((eq method 'INTERP)
             ;--------------------
             ; susp_XX_samples(NAME, NAME_ptr, NAME_cnt);
             ; susp->NAME_x1_sample = susp_fetch_sample(NAME, NAME_ptr,
             ;                                          NAME_cnt);
             ; <fixup depends variables> (if a step function)
             ;--------------------
             (format stream "        ~A(~A, ~A_ptr, ~A_cnt);~%"
              (susp-check-fn name alg) name name name)
             (cond ((member (name-to-symbol name) internal-scaling)
                    (format stream
                     "        susp->~A_cnt--;~%        ~
                              susp->~A_x1_sample = *(susp->~A_ptr);~%"
                     name name name))
                   (t
                    (format stream 
                     "        susp->~A_x1_sample = ~
                              susp_fetch_sample(~A, ~A_ptr, ~A_cnt);~%"
                    name name name name)))
             (setf is-step (member (name-to-symbol name) step-function))
             (cond (is-step
                    (fixup-depends-prime alg stream name "        "
                        (strcat "susp->" name "_x1_sample")))))
            ((eq method 'RAMP)
             ;--------------------
             ; susp->NAME_pHaSe = 1.0;
             ;--------------------
             (format stream "        susp->~A_pHaSe = ~A;~%" name "1.0")))))

  ;--------------------
  ; *WATCH*
  ;       show_samples(2,susp->NAME_x2,0);
  ;--------------------
; (if *WATCH*
;   (format stream "        show_samples(2,~A_x2,0);~%" name))

  ;--------------------
  ; }
  ;--------------------
  (format stream "    }~%")))
      

(print 'write-prime)

;;************
;; show-samples-option
;;
;; Inputs:
;;      stream: output stream for file
;;      name: token to use for forming name
;; Effect:
;;      Writes sampling clause
;;************
(defun show-samples-option (stream name)
    ;----------------------------
    ;   else
    ;      { /* just show NAME */
    ;       show_samples(1,NAME,NAME_ptr - NAME->samples); 
    ;      } /* just show NAME */
    ;----------------------------
;  (format stream "            show_samples(1, ~A, 0);~%        } else {~%" name)
;  (format stream "            show_samples(1, ~A, ~A_ptr - ~A->samples);~%~%"
;          name name name)
)


(print "show-samples-option")

;;************
;; write-susp -- compile the suspension according to interpolation spec
;;
;;************

(defun write-susp (alg stream)
  (let* ((interp (get alg 'interpolation))
         (encoding (encode interp))
         (internal-scaling (get alg 'internal-scaling))
         (sound-names (get alg 'sound-names))
         (name (get-slot alg 'name))
         (logical-stop (car (get-slot alg 'logical-stop)))
         (terminate (car (get-slot alg 'terminate)))
         (outer-loop (get-slot alg 'outer-loop))
         (step-function (get-slot alg 'step-function))
         (depends (get-slot alg 'depends))
         (inner-loop (get-slot alg 'inner-loop))
         n s m p fn-name loop-prefix joint-depend)

    (display "write-susp" interp encoding)

    ;---------------------------
    ; non-ANSI:
    ;     void NAME_<encoding>_fetch(a_susp, snd_list)
    ;   register pwl_susp_type a_susp;
    ;        snd_list_type snd_list;
    ;        {
    ; ANSI:
    ;     void NAME_<encoding>_fetch(snd_susp_type a_susp,
    ;                                snd_list_type snd_list)
    ;        {
    ;---------------------------

    (setf fn-name (format nil "~A_~A_fetch" name encoding))
    (cond (*ANSI*
           (format stream
        "~%~%void ~A(snd_susp_type a_susp, snd_list_type snd_list)~%{~%"
            fn-name))
          (t
           (format stream
            "~%~%void ~A(a_susp, snd_list)~%  snd_susp_type a_susp;~%~A~%"
            fn-name "  snd_list_type snd_list;\n{")))

    ;-----------------------------
    ;    NAME_susp_type susp = (NAME_susp_type) a_susp;
    ;    int cnt = 0;  /* how many samples computed */
    ;-----------------------------
    (format stream "    ~A_susp_type susp = (~A_susp_type) a_susp;~%"
            name name)
    (format stream "    int cnt = 0; /* how many samples computed */~%")

    (dotimes (n (length interp))
      (let ((name (nth n sound-names))
            interpolate-samples
            (method (nth n interp)))
        (setf interpolate-samples
              (not (member (name-to-symbol name) step-function)))

        (cond ((and interpolate-samples (eq method 'INTERP))
               (format stream "    sample_type ~A_x2_sample;~%" name))
              ((eq method 'INTERP))
              ((and interpolate-samples (eq method 'RAMP))
               ;-----------------
               ;    sample_type NAME_DeLtA;
               ;    sample_type NAME_val;
               ;-----------------
               (format stream "    sample_type ~A_DeLtA;~%" name)
               (format stream "    sample_type ~A_val;~%" name)
               (format stream "    sample_type ~A_x2_sample;~%" name))
              ((eq method 'RAMP)
               ;-----------------
               ;    sample_type NAME_val;
               ;-----------------
               (format stream "    sample_type ~A_val;~%" name)))))

    ;-----------------------------
    ;   int togo;
    ;   int n;
    ;   sample_block_type out;
    ;   register sample_block_values_type out_ptr;
    ;   register sample_block_values_type out_ptr_reg;
    ;-----------------------------
    (format stream "    int togo;~%")
    (format stream "    int n;~%")
    (format stream "    sample_block_type out;~%")
    (format stream "    register sample_block_values_type out_ptr;~%~%")
    (format stream "    register sample_block_values_type out_ptr_reg;~%~%")

    ;; computations for DEPENDS variables added to inner loop
    (setf loop-prefix "")
    (dolist (dep depends)
      (dotimes (n (length interp))
        (let ((method (nth n interp))
              (name (nth n sound-names))
              interpolate-samples)
          (setf interpolate-samples
                (not (member (name-to-symbol name) step-function)))
          (cond ((and (equal name (cadr dep))
                      (or (member method '(NONE SCALE))
                          interpolate-samples))
                 (setf loop-prefix (format nil "~A            ~A = ~A;~%"
                                    loop-prefix (car dep) (caddr dep))))))))

    ;; computation of JOINT-DEPENDENCY, if applicable
    (setf joint-depend "")
    (dolist (dep (get-slot alg 'joint-dependency))
      ;; if any depended on var is recomputed in inner loop, add the stmts
      (cond ((depended-on-in-inner-loop (car dep) interp sound-names 
                                        step-function)
             (dolist (stmt (cdr dep))
                (setf joint-depend (strcat joint-depend 
                                        "            " stmt "\n"))))))

    ; this computes some additional declarations
    (compute-inner-loop alg (strcat loop-prefix joint-depend
                                    "            " inner-loop))
    ; make the declarations
    (print-strings (get-slot alg 'register-decl) stream)

    ;-----------------------------
    ;   falloc_sample_block(out, "caller");
    ;  out_ptr = out->samples;
    ;  snd_list->block = out;
    ;-----------------------------
    (format stream "    falloc_sample_block(out, \"~A\");~%" fn-name)
    (format stream "    out_ptr = out->samples;~%")
    (format stream "    snd_list->block = out;~%")

    ;-----------------------------
    ; prime the ramp/interp streams
    ;-----------------------------
    ;; run this code the first time the suspension is called
    (cond ((or (member 'RAMP interp) (member 'INTERP interp))
           (write-prime alg stream interp sound-names)))

    (dotimes (n (length interp))
       (let ((name (nth n sound-names))
             interpolate-samples
             (method (nth n interp)))
         (setf interpolate-samples
               (not (member (name-to-symbol name) step-function)))

         (cond ((or (and interpolate-samples (eq method 'INTERP))
                    (eq method 'RAMP))
                ;-------------
                ;   susp_check_XX_samples(NAME, NAME_ptr, NAME_cnt);
                ;-------------
                (format stream
                        "~%    ~A(~A, ~A_ptr, ~A_cnt);~%"
                        (susp-check-fn name alg) name name name)))

         (cond ((and interpolate-samples (eq method 'INTERP))
                ;-------------
                ;  susp->NAME_x2_sample = susp->NAME->scale * susp->NAME_x2_ptr);
                ;-------------
                (cond ((member (name-to-symbol name) internal-scaling)
                       (format stream
                        "    ~A_x2_sample = *(susp->~A_ptr);~%" name name))
                      (t
                          (format stream
                        "    ~A_x2_sample = susp_current_sample(~A, ~A_ptr);~%"
                        name name name))))
               ((eq method 'INTERP)
                ;-------------
                ;
                ;-------------
                )
               ((and interpolate-samples (eq method 'RAMP))
                ;----------------
                ; susp->NAME_x2_sample = susp_current_sample(NAME, NAME_ptr);
                ;----------------
                (cond ((member (name-to-symbol name) internal-scaling)
                       (format stream
                        "    ~A_x2_sample = *(susp->~A_ptr);~%" name name))
                      (t
                       (format stream 
                        "    ~A_x2_sample = susp_current_sample(~A, ~A_ptr);~%"
                        name name name))))
               ((eq method 'RAMP)
                ))))

    ;----------------------------
    ; *WATCH*: printf("NAME %x new block %x\n", susp, out);
    ;----------------------------
    (if *watch*
      (format stream "    printf(\"~A %x new block %x\\n\", susp, out);~%" name))

    ;----------------------------
    ;    while (cnt < max_sample_block_len)  { /* outer loop */
    ;      /* first compute how many samples to generate in inner loop: */
    ;      /* don't overflow the output sample block: */
    ;      togo = max_sample_block_len - cnt;
    ;----------------------------
    
    (format stream 
            "~%    while (cnt < max_sample_block_len) { /* outer loop */~%")
    (format stream 
     "        /* first compute how many samples to generate in inner loop: */~%")
    (format stream
     "        /* don't overflow the output sample block: */~%")
    (format stream
     "        togo = max_sample_block_len - cnt;~%~%")

    ;; this loop gets ready to execute the INNER-LOOP
    (dotimes (n (length interp))
      (let ((name (nth n sound-names))
            interpolate-samples
            (method (nth n interp)))
        (setf interpolate-samples
              (not (member (name-to-symbol name) step-function)))

        (cond ((member method '(NONE SCALE))
               ;-----------------
               ; NONE:
               ;    /* don't run past the NAME input sample block */
               ; susp_check_XX_for_samples(NAME, NAME_ptr, NAME_cnt);
               ;    togo = min(togo, susp->NAME_cnt);
               ;-----------------
               (format stream
                "        /* don't run past the ~A input sample block: */~%" name)
               (display "don't run past the ..." name (susp-check-fn name alg))
               (format stream
                "        ~A(~A, ~A_ptr, ~A_cnt);~%"
                (susp-check-fn name alg) name name name)
               (format stream "        togo = min(togo, susp->~A_cnt);~%~%" name))
              ((eq method 'INTERP))
              ((and interpolate-samples (eq method 'RAMP))
                ;-----------------
                ; RAMP:
                ;
                ;    /* grab next NAME_x2_sample when phase goes past 1.0 */
                ;    /* we use NAME_n (computed below) to avoid roundoff errors: */
                ; if (susp->NAME_n <= 0) {
                ;     susp->NAME_x1_sample = NAME_x2_sample;
                ;     susp->NAME_ptr++;
                ;     susp_took(NAME_cnt, 1);
                ;     susp->NAME_pHaSe -= 1.0;
                ;     susp_check_log_samples(NAME, NAME_ptr, NAME_cnt);
                ;     NAME_x2_sample = susp_current_sample(NAME, NAME_ptr);
                ; }
                ; /* NAME_n gets number of samples before phase exceeds 1.0: */
                ; susp->NAME_n = 0.5 + (int64_t) ((1.0 - susp->NAME_pHaSe) * susp->output_per_NAME);
                ; togo = (int) min(togo, susp->NAME_n);
                ; NAME_DeLtA = (sample_type) ((NAME_x2_sample - susp->NAME_x1_sample) *  susp->NAME_pHaSe_iNcR);
                ; NAME_val = (sample_type) (susp->NAME_x1_sample * (1.0 - susp->NAME_pHaSe) +
                ;      NAME_x2_sample * susp->NAME_pHaSe);
                ;-----------------
                (format stream
                 "        /* grab next ~A_x2_sample when phase goes past ~
                 1.0; */~%" name)
                (format stream
                 "        /* we use ~A_n (computed below) to avoid roundoff ~
                 errors: */~%" name)
                (format stream "        if (susp->~A_n <= 0) {~%" name)
                (format stream "            susp->~A_x1_sample = ~
                 ~A_x2_sample;~%" name name)
                (format stream "            susp->~A_ptr++;~%" name);
                (format stream "            susp_took(~A_cnt, 1);~%" name);
                (format stream "            susp->~A_pHaSe -= 1.0;~%" name);
                (format stream "            ~A(~A, ~A_ptr, ~A_cnt);~%"
                 (susp-check-fn name alg) name name name)
                (cond ((member (name-to-symbol name) internal-scaling)
                       (format stream
                               "            ~A_x2_sample = *(susp->~A_ptr);~%"
                               name name))
                      (t
                       (format stream 
                        "            ~A_x2_sample = ~
                        susp_current_sample(~A, ~A_ptr);~%"
                        name name name)))
                (format stream
                 "            /* ~A_n gets number of samples before phase ~
                                 exceeds 1.0: */~%"
                        name)
                (format stream 
                        "            susp->~A_n = (int64_t) ~
                                     ((1.0 - susp->~A_pHaSe) *~%"
                        name name)
                (format stream "                                        ~
                        susp->output_per_~A);~%        }~%" name)
                (format stream "        togo = (int) min(togo, susp->~A_n);~%"
                        name)
                (format stream "        ~A_DeLtA = (sample_type) ~
                ((~A_x2_sample - susp->~A_x1_sample) * susp->~A_pHaSe_iNcR);~%" 
                        name name name name)
                (format stream 
                 "        ~A_val = (sample_type) ~
                 (susp->~A_x1_sample * (1.0 - susp->~A_pHaSe) +~%"
                        name name name)
                (format stream "                 ~A_x2_sample * ~
                        susp->~A_pHaSe);~%~%" name name))
              ((eq method 'RAMP)
                ;-----------------
                ; SLOW STEP FUNCTION
                ;
                ; /* grab next NAME_x1_sample when phase goes past 1.0 */
                ; /* use NAME_n (computed below) to avoid roundoff errors: */
                ; if (susp->NAME_n <= 0) {
                ;     <fixup depends declarations>
                ;     susp_check_log_samples(NAME, NAME_ptr, NAME_cnt);
                ;     susp->NAME_x1_sample = susp_fetch_sample(NAME, NAME_ptr,
                ;                                              NAME_cnt);
                ;     susp->NAME_pHaSe -= 1.0;
                ;     /* NAME_n gets number of samples before phase 
                ;        exceeds 1.0: */
                ;     susp->NAME_n = (int64_t) ((1.0 - susp->NAME_pHaSe) *
                ;                    susp->output_per_NAME);
                ;     <fixup depends variables>
                ; }
                ; togo = (int) min(togo, susp->NAME_n);
                ; NAME_val = susp->NAME_x1_sample; 
                ;-----------------
                (format stream
                 "        /* grab next ~A_x1_sample when phase goes ~
                             past 1.0; */~%"
                        name)
                (format stream
                "        /* use ~A_n (computed below) to avoid roundoff ~
                            errors: */~%"
                        name)
                (format stream "        if (susp->~A_n <= 0) {~%" name)
                (fixup-depends-prime-decls alg stream name)
                (format stream "            ~A(~A, ~A_ptr, ~A_cnt);~%"
                 (susp-check-fn name alg) name name name)
                (format stream 
                 "            susp->~A_x1_sample = ~
                              susp_fetch_sample(~A, ~A_ptr, ~A_cnt);~%"
                        name name name name)
                (format stream "            susp->~A_pHaSe -= 1.0;~%" name);
                (format stream
                 "            /* ~A_n gets number of samples before phase ~
                                 exceeds 1.0: */~%" name)
                (format stream 
                 "            susp->~A_n = (int64_t) ((1.0 - ~
                                 susp->~A_pHaSe) *~%"
                        name name)
                (format stream "                                        ~
                        susp->output_per_~A);~%" name)
                (fixup-depends-prime alg stream name "            "
                        (strcat "susp->" name "_x1_sample"))
                (format stream "        }~%" name)
                (format stream "        togo = (int) min(togo, susp->~A_n);~%"
                        name)
                (format stream 
                 "        ~A_val = susp->~A_x1_sample;~%" name name) ))))
    
    ;---------------
    ; see if there are joint-dependencies that should be output now
    ; output here if none of depended-on signals are updated in inner loop
    ;---------------
    ;; computation of JOINT-DEPENDENCY, if applicable
    (setf joint-depend "")
    (dolist (dep (get-slot alg 'joint-dependency))
      (cond ((not (depended-on-in-inner-loop (car dep) interp sound-names 
                                             step-function))
             (dolist (stmt (cdr dep))
                (setf joint-depend (strcat joint-depend 
                                        "        " stmt "\n"))))))
    (display "joint-depend before fixup" joint-depend)
    (setf joint-depend (fixup-substitutions-for-depends alg joint-depend))
    (if joint-depend (format stream joint-depend))
    (display "joint-depend outside loop" joint-depend)

    ;----------------
    ; if the teminate time is a MIN of some signals or AT some expression
    ; (i.e. specified at all) see if we're coming to the terminate cnt:
    ;
    ;   /* don't run past terminate time */
    ;   if (susp->terminate_cnt != UNKNOWN && 
    ;           susp->terminate_cnt <= susp->susp.current + cnt + togo) {
   ;        togo = (int) (susp->terminate_cnt - (susp->susp.current + cnt));
    ;       if (togo < 0) togo = 0; // avoids rounding errors
    ;       if (togo == 0) break;
    ;   }
    ;----------------
    (cond ((terminate-check-needed terminate alg)
           (print-strings '(
     "        /* don't run past terminate time */\n"
     "        if (susp->terminate_cnt != UNKNOWN &&\n"
     "            susp->terminate_cnt <= susp->susp.current + cnt + togo) {\n"
     "            togo = (int) (susp->terminate_cnt - "
     "(susp->susp.current + cnt));\n"
     "            if (togo < 0) togo = 0;  /* avoids rounding errros */\n"
     "            if (togo == 0) break;\n"
     "        }\n\n") stream)))

    ;----------------
    ; if the logical-stop attribute is MIN of some signals or AT some expression
    ; see if we're coming to the logical stop:
    ;
    ;   /* don't run past logical stop time */
    ;   if (!susp->logically_stopped && susp->susp.log_stop_cnt != UNKNOWN) {
    ;       int64_t to_stop = susp->susp.log_stop_cnt -
    ;                         (susp->susp.current + cnt);
    ;           /* break if to_stop == 0 (we're at the logical stop)
    ;        * AND cnt > 0 (we're not at the beginning of the
    ;        * output block).
    ;        */
    ;       if (to_stop < 0) to_stop = 0; // avoids rounding errors
    ;       if (to_stop < togo) {
    ;           if (to_stop == 0) {
    ;               if (cnt) {
    ;                   togo = 0;
    ;                   break;
    ;               } else /* keep togo as is: since cnt == 0, we
    ;                       * can set the logical stop flag on this
    ;                       * output block
    ;                       */
    ;                   susp->logically_stopped = true;
    ;           } else /* limit togo so we can start a new
    ;                   * block at the LST
    ;                   */
    ;               togo = (int) to_stop;
    ;                 }     
    ;   }
    ;----------------
    (cond (logical-stop
           (print-strings '(
     "\n        /* don't run past logical stop time */\n"
     "        if (!susp->logically_stopped && "
     "susp->susp.log_stop_cnt != UNKNOWN) {\n"
     "            int64_t to_stop = susp->susp.log_stop_cnt - "
     "(susp->susp.current + cnt);\n"
     "            /* break if to_stop == 0 (we're at the logical stop)\n"
     "             * AND cnt > 0 (we're not at the beginning of the\n"
     "             * output block).\n"
     "             */\n"
     "            if (to_stop < 0) to_stop = 0; /* avoids rounding errors */\n"
     "            if (to_stop < togo) {\n"
     "                if (to_stop == 0) {\n"
     "                    if (cnt) {\n"
     "                        togo = 0;\n"
     "                        break;\n"
     "                    } else /* keep togo as is: since cnt == 0, we\n"
     "                            * can set the logical stop flag on this\n"
     "                            * output block\n"
     "                            */\n"
     "                        susp->logically_stopped = true;\n"
     "                } else /* limit togo so we can start a new\n"
     "                        * block at the LST\n"
     "                        */\n"
     "                    togo = (int) to_stop;\n"
     "            }\n"
     "        }\n\n")
                          stream)))

    (cond (outer-loop
           (print-strings outer-loop stream)
           (format stream "~%")))

    ;----------------------------
    ; n = togo;
    ; *WATCH*: printf("ALG %x starting inner loop, n %d\n", susp, n);
    ;----------------------------

    (format stream "        n = togo;~%")
    (if *watch* 
      (format stream
              "        printf(\"~A %x starting inner loop, n %d\\n\", ~
              susp, n);~%" name))

    (dotimes (n (length interp))
       (let ((name (nth n sound-names))
             (method (nth n interp)))
          (cond ((eq method 'NONE))
                 ;-----------------
                 ;  NONE:
                 ;-----------------
                ((eq method 'RAMP))
                ;-----------------
                ; RAMP:
                ;-----------------
                ((and (eq method 'INTERP) (eq n 0))
                ;-----------------
                ; INTERP (first arg only)
;               ;       susp->NAME_cnt -= togo;
                ;-----------------
;                (format stream "        susp->~A_cnt -= togo;~%" name)
                 ))))

    (print-strings (get-slot alg 'register-init) stream)
    ;----------------------------
    ; if (n) do  { /* inner loop */
    ;----------------------------

    (format stream
            "        if (n) do { /* the inner sample computation loop */~%")

    ;;----------------------------
    ;; write local declarations supplied by user
    ;;----------------------------

    (print-strings (get-slot alg 'inner-loop-locals) stream)

    ;;----------------------------
    ;; declare temps that depend on signals
    ;;----------------------------

    (dotimes (n (length interp))
      (let ((method (nth n interp))
            interpolate-samples
            (name (nth n sound-names)))
        (setf interpolate-samples
              (not (member (name-to-symbol name) step-function)))
        (cond ((or (member method '(NONE SCALE))
                   interpolate-samples)
               (dolist (dep depends)
                (cond ((and (equal (cadr dep) name)
                            (eq (cadddr dep) 'TEMP))
                       (format stream "            ~A ~A;~%" (car (cddddr dep)) 
                                      (car dep)))))))))

    ;; this loop writes code that runs in the INNER-LOOP and checks to see
    ;; if we need to advance to the next pair of interpolated points for
    ;; each interpolated sound
    (dotimes (n (length interp))
      (let ((name (nth n sound-names))
            interpolate-samples
            (method (nth n interp)))
        (setf interpolate-samples
              (not (member (name-to-symbol name) step-function)))

        (cond ((and interpolate-samples (eq method 'INTERP))
                ;-----------------
                ; INTERP:
                ;
                ;    if (susp->NAME_pHaSe >= 1.0)  { 
                ;          NAME_x1_sample_reg =NAME_x2_sample_reg;
                ;       /* pick up next sample as NAME_x2_sample */
                ;       susp->NAME_ptr++;
                ;        susp_took(NAME_cnt, 1);
                                     ;        susp->NAME_pHaSe -= 1.0;
                ;       susp_check_XX_samples_break(NAME, NAME_ptr, NAME_cnt, NAME_x2_sample);
                ; }
                ; <maintenance of depends variables>
                ;-----------------
                (format stream "            if (~A_pHaSe_ReG >= 1.0) {~%" name)
                (format stream "                ~A_x1_sample_reg = ~
                        ~A_x2_sample;~%" name name)
                (format stream "                /* pick up next sample ~
                        as ~A_x2_sample: */~%" name)
                (format stream "                susp->~A_ptr++;~%" name)
                (format stream "                susp_took(~A_cnt, 1);~%" name)
                (format stream "                ~A_pHaSe_ReG -= 1.0;~%" name)
                (format stream "                ~A_break(~A, ~A_ptr, ~A_cnt, ~
                        ~A_x2_sample);~%"
                        (susp-check-fn name alg) name name name name)
;                (format stream "                ~A_x2_sample = ~
;                        susp_current_sample(~A, ~A_ptr);~%"
;                        name name name)

                ;     show_samples(2, susp->NAME_x2, susp->NAME_x2_ptr -
                ;                       NAME_x2->block->samples);
                ;-----------------

;               (if *WATCH* 
;                   (format stream "                ~
;                           show_samples(2,susp->~A_x2,susp->~A_x2_ptr - ~
;                                        susp->~A_x2->block->samples);~%"
;                           name name name)
;                )
                ;-----------------
                ;      }
                ;-----------------
                (format stream "            }~%")
              )
              ((eq method 'INTERP)
                ;-----------------
                ; STEP FUNCTION:
                ;
                ;    if (susp->NAME_pHaSe >= 1.0)  { 
                ;       <optional depends/fixup declarations>
                ;       /* pick up next sample as NAME_x1_sample */
                ;       susp->NAME_ptr++;
                ;        susp_took(NAME_cnt, 1);
                ;        susp->NAME_pHaSe -= 1.0;
                ;       susp_check_XX_samples_break(NAME, NAME_ptr, NAME_cnt, NAME_x1_sample);
                ;        NAME_x1_sample_reg = susp_current_sample(NAME, NAME_ptr);
                ;       <optional depends/fixup code>
                ; }
                ;-----------------
                (format stream "            if (~A_pHaSe_ReG >= 1.0) {~%" name)
                (fixup-depends alg stream name)
                (format stream "                /* pick up next sample as ~
                        ~A_x1_sample: */~%" name)
                (format stream "                susp->~A_ptr++;~%" name)
                (format stream "                susp_took(~A_cnt, 1);~%" name)
                (format stream "                ~A_pHaSe_ReG -= 1.0;~%" name)
                (format stream "                ~A_break(~A, ~A_ptr, ~A_cnt, ~
                        ~A_x1_sample_reg);~%"
                        (susp-check-fn name alg) name name name name)
                (format stream "                ~A_x1_sample_reg = ~
                        susp_current_sample(~A, ~A_ptr);~%"
                        name name name)

                ;     show_samples(2, susp->NAME_x2, susp->NAME_x2_ptr -
                ;                       NAME_x2->block->samples);
                ;-----------------

;               (if *WATCH* 
;                   (format stream "                ~
;                    show_samples(2,susp->~A_x2,susp->~A_x2_ptr - ~
;                    susp->~A_x2->block->samples);~%" name name name)
;                )
                (let ((fixup-code (get-slot alg 'fixup-code)))
                  (if fixup-code (format stream fixup-code)))

                ;-----------------
                ;      }
                ;-----------------
                (format stream "            }~%")))))

    (write-inner-loop alg stream)
    (print-strings (get-slot alg 'register-cleanup) stream)

    ;; this loop calls loop tail computations on all sounds
    (dotimes (n (length interp))
      (let ((name (nth n sound-names))
            interpolate-samples
            (method (nth n interp)))
        (setf interpolate-samples
              (not (member (name-to-symbol name) step-function)))

        (cond ((member method '(NONE SCALE))
               ;-----------------
               ; NONE:
               ;    susp_took(NAME_cnt, togo - n);
               ;-----------------
               (format stream "        susp_took(~A_cnt, togo);~%" name))
              ((eq method 'INTERP))
              ((eq method 'RAMP)
                ;-----------------
                ; RAMP:
                ;       susp->NAME_pHaSe += togo * susp->NAME_pHaSe_iNcR;
                ;       susp->NAME_n -= togo; 
                ;-----------------
                (format stream
                 "        susp->~A_pHaSe += togo * susp->~A_pHaSe_iNcR;~%"
                 name name)
                (format stream "        susp->~A_n -= togo;~%" name)
               ))))
    ;-----------------------------
    ;     cnt += togo;
    ;    } /* outer loop */
    ; 
    ; snd_list->block_len = cnt;
    ;-----------------------------

    (format stream "~A~%~A~%~%" "        cnt += togo;"
                   "    } /* outer loop */")
    ;-----------------------------
    ; if terminate is not NONE (infinite), check for it as follows:
    ;    /* test for termination */
    ;    if (togo == 0 && cnt == 0) {
    ;        snd_list_terminate(snd_list);
    ;   *WATCH*: printf("NAME %x terminated\n", susp);
    ;    } else {
    ;       snd_list->block_len = cnt;
    ;       susp->susp.current += cnt;
    ;    }
    ;-----------------------------
    (cond ((terminate-possible terminate alg)
           (print-strings '(
             "    /* test for termination */\n"
             "    if (togo == 0 && cnt == 0) {\n"
             "        snd_list_terminate(snd_list);\n")
            stream)
           (if *watch*
               (format stream
                       "        printf(\"~A %x terminated.\\n\", susp);~%"
                       name))
           (print-strings '(
             "    } else {\n"
             "        snd_list->block_len = cnt;\n"
             "        susp->susp.current += cnt;\n"
             "    }\n") stream))
          (t
    ;----------------
    ; OTHERWISE (no termination test):
    ;    snd_list->block_len = cnt;
    ;    susp->susp.current += cnt;
    ;----------------
           (print-strings '(
             "    snd_list->block_len = cnt;\n"
             "    susp->susp.current += cnt;\n") stream)))

     ;----------------
     ; if logical-stop is not the default check for it as follows:
     ;    /* test for logical stop */
     ;    if (susp->logically_stopped) {
     ;      snd_list-> logically_stopped = true;
     ;    } else if (susp->susp.log_stop_cnt == susp->susp.current) {
     ;      susp->logically_stopped = true;
     ;    }
     ;----------------
    (cond ((logical-stop-check-needed logical-stop)
           (print-strings '(
    "    /* test for logical stop */\n"
    "    if (susp->logically_stopped) {\n"
    "        snd_list->logically_stopped = true;\n"
    "    } else if (susp->susp.log_stop_cnt == susp->susp.current) {\n"
    "        susp->logically_stopped = true;\n"
    "    }\n") stream)))

     ;----------------
     ;  } /* name_encoding_fetch */
     ;----------------
    (format stream "} /* ~A_~A_fetch */~%" name encoding)))

(print 'write-susp)

; terminate-check-needed -- see if this is either a terminate clause
; that specifies MIN or AT, or is NIL (meaning none-specified so take
; the default) and there are signal parameters
;
(defun terminate-check-needed (terminate alg)
  (cond (terminate
         (cond ((listp terminate)
                (cond ((member (car terminate) '(MIN AT AFTER)) t)
                      (t nil)))
               ((member terminate '(COMPUTED NONE)) nil)
               (t
                (error "TERMINATE clause should specify a list"))))
        ((get alg 'sound-args) t)))

          
; same as terminate-check-needed, but also returns true for COMPUTED 
; termination
;
(defun terminate-possible (terminate alg)
  (cond (terminate
         (cond ((listp terminate)
                (cond ((member (car terminate) '(MIN AT AFTER COMPUTED)) t)
                      (t nil)))
               ((eq terminate 'NONE) nil)
               ((eq terminate 'COMPUTED) t)
               (t
                (error "TERMINATE clause should specify a list"))))
        ((get alg 'sound-args) t)))
