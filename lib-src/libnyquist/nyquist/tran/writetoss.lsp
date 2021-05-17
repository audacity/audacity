;; writetoss -- writes the "toss prepended samples" routine

;; modified May 3, 1999 by RBD to not adjust t0 when samples will be tossed
;;    also, final_time is just susp->susp.t0, since t0 is unadjusted.

(defun write-toss (alg stream)
  (let ((alg-name (get alg 'name))
        (sound-names (get alg 'sound-names)))
    ;;----------------
    ;; void ALG_toss_fetch(susp, snd_list)
    ;;   register ALG_susp_type susp;
    ;;   snd_list_type snd_list;
    ;; {
;   (format stream "~%~%void ~A_toss_fetch(susp, snd_list)~%" alg-name)
;   (format stream "  register ~A_susp_type susp;~%" alg-name)
;   (format stream "  snd_list_type snd_list;~%{~%")
    ;;--------OR------
    ;; void ALG_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)
    ;; {
    ;;    ALG_susp_type susp = (ALG_susp_type) a_susp;
    (format stream
     "~%~%void ~A_toss_fetch(snd_susp_type a_susp, snd_list_type snd_list)~%"
     alg-name)
    (format stream "{~%    ~A_susp_type susp = (~A_susp_type) a_susp;~%"
            alg-name alg-name)
    ;;----------------
    ;;    long final_count = susp->susp.toss_cnt);
    ;;    time_type final_time = susp->susp.t0;
    ;;    FORMERLY, THIS WAS:
    ;;      time_type final_time = susp->susp.t0 + final_count / susp->susp.sr;
    ;;    int n;
    ;;----------------
    (if *watch*
      (format stream
       "    long final_count = susp->susp.toss_cnt;~%"))
    (format stream
     "    time_type final_time = susp->susp.t0;~%")
    (format stream "    int n;~%~%")

    (cond (*watch*
           (format stream
            "    printf(\"~A_toss_fetch: final count %d final time %d\\n\", "
            alg-name)
           (format stream "final_count, final_time);~%")))

    ;;------------------------------
    ;; for each sound argument:
    ;;
    ;;    /* fetch samples from NAME up to final_time for this block of zeros */
    ;;    while ((ROUNDBIG((final_time - susp->NAME->t0) * susp->NAME->sr)) >=
    ;;     susp->NAME->current)
    ;;  susp_get_samples(NAME, NAME_ptr, NAME_cnt);
    ;;------------------------------
    (dolist (name sound-names)
      (format stream
  "    /* fetch samples from ~A up to final_time for this block of zeros */~%"
       name)
      (format stream
  "    while ((ROUNDBIG((final_time - susp->~A->t0) * susp->~A->sr)) >=~%" 
       name name)
      (format stream "           susp->~A->current)~%" name)
      (format stream "        susp_get_samples(~A, ~A_ptr, ~A_cnt);~%" 
              name name name))

    ;;----------------
    ;;    /* convert to normal processing when we hit final_count */
    ;;    /* we want each signal positioned at final_time */
    ;;----------------
      (format stream
       "    /* convert to normal processing when we hit final_count */~%")
      (format stream "    /* we want each signal positioned at final_time */~%")

    ;;----------------
    ;; for each sound argument:
    ;;
    ;;  n = (int) ROUNDBIG((final_time - susp->NAME->t0) * susp->NAME->sr -
    ;;       (susp->NAME->current - susp->NAME_cnt));
    ;;  susp->NAME_ptr += n;
    ;;  susp_took(NAME_cnt, n);
    ;;----------------
    (dolist (name sound-names)
      (format stream "    n = (int) ROUNDBIG((final_time - susp->~A->t0) * ~
                     susp->~A->sr -~%"
              name name)
      (format stream "         (susp->~A->current - susp->~A_cnt));~%"
              name name)
      (format stream "    susp->~A_ptr += n;~%" name)
      (format stream "    susp_took(~A_cnt, n);~%" name))

    ;;----------------
    ;;  susp->susp.fetch = susp->susp.keep_fetch;
    ;;  (*(susp->susp.fetch))(a_susp, snd_list);
    ;; }
    ;;----------------
    (format stream "    susp->susp.fetch = susp->susp.keep_fetch;~%")
    (format stream "    (*(susp->susp.fetch))(a_susp, snd_list);~%")
    (format stream "}~%")))
