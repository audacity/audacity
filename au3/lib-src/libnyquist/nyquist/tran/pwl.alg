(PWL-ALG
  (NAME "pwl")
  (ARGUMENTS ("time_type" "t0") ("rate_type" "sr") ("LVAL" "lis"))
  (SUPPORT-FUNCTIONS "
/* IMPLEMENTATION NOTE:
 * The lis argument is a list of alternating sample numbers and values
 * which are taken in pairs, with an implicit (0, 0) starting point.  The
 * last point  is a sample number only, the value being implicitly 0.
 * The bpt_ptr points to the next sample number in the list.
 * The incr is set to the increment per sample, and lvl is the next
 * sample value.
 *
 * The list is assumed to be well-formed, so it should be checked by
 * the caller (users should not call this directly).
 */

char *pwl_bad_breakpoint_list = \"bad breakpoint list\";

/* compute_lvl -- setup the susp with level, advance bpt_ptr */
/*
 * returns true if it is time to terminate
 *
 * Note: compute_lvl gets called in the outer loop to skip over
 * a breakpoint pair before starting the compute_incr loop, which
 * searches for a breakpoint that is some number of samples in the
 * future.  This code is not embedded in compute_incr because it is
 * NOT called from the initialization, where it would be wrong to
 * skip over the first breakpoint.
 */
boolean compute_lvl(pwl_susp_type susp)
{
    LVAL lval = susp->bpt_ptr;
    if (!consp(lval)) xlfail(pwl_bad_breakpoint_list);
    lval = cdr(lval);
    if (!lval) return true;
    if (!consp(lval)) xlfail(pwl_bad_breakpoint_list);
    lval = car(lval);
    if (!floatp(lval)) xlfail(pwl_bad_breakpoint_list);
    susp->lvl = getflonum(lval);
    susp->bpt_ptr = cdr(cdr(susp->bpt_ptr));
    return !susp->bpt_ptr;
}


/* compute_incr -- setup the susp with level and increment */
/*
 * returns true if it is time to terminate
 */
boolean compute_incr(pwl_susp_type susp, int64_t *n, int64_t cur)
{
    double target;
    while (*n == 0) {
        LVAL lval = susp->bpt_ptr;
        if (!consp(lval)) xlfail(pwl_bad_breakpoint_list);
        lval = car(lval);
        if (!fixp(lval)) xlfail(pwl_bad_breakpoint_list);
        *n = getfixnum(lval) - cur;
        /* if there is a 2nd element of the pair, get the target */
        lval = cdr(susp->bpt_ptr);
        if (lval) {
            if (!consp(lval)) xlfail(pwl_bad_breakpoint_list);
            lval = car(lval);
            if (!floatp(lval)) xlfail(pwl_bad_breakpoint_list);
            target = getflonum(lval);
        } else target = 0.0;
        if (*n > 0) susp->incr = (target - susp->lvl) / *n;
        else if (compute_lvl(susp)) return true;
    }
    return false;
}
")
 (SAMPLE-RATE "sr")
  (STATE 
        ("LVAL" "bpt_ptr" "lis")
        ("double" "incr" "0.0")
        ("double" "lvl" "0.0; 
    { int64_t temp = 0; compute_incr(susp, &temp, 0); }"))
  
  (OUTER-LOOP "
        if (susp->bpt_ptr == NULL) {
out:	    togo = 0;	/* indicate termination */
            break;	/* we're done */
        }
        { int64_t cur = susp->susp.current + cnt;
          int64_t nn = getfixnum(car(susp->bpt_ptr)) - cur;
          if (nn == 0) {
              if (compute_lvl(susp) || compute_incr(susp, &nn, cur)) goto out;
          }
          togo = (int) min(nn, togo);
        }
")
  (INNER-LOOP "output = (sample_type) lvl; lvl += incr")
  (MAINTAIN ("lvl" "susp->lvl += susp->incr * togo"))
  (CONSTANT "incr")
  (TERMINATE COMPUTED)
)

