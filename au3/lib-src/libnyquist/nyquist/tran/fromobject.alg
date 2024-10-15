(FROMOBJ-ALG
  (NAME "fromobject")
  (ARGUMENTS ("time_type" "t0") ("rate_type" "sr") ("LVAL" "src"))
  (SUPPORT-FUNCTIONS "
/* IMPLEMENTATION NOTE:
 * The src argument is an XLisp object that returns either a
 *  FLONUM sample or NIL. The output of fromobj is simply the
 * sequence of the samples. 
 */

#include \"samples.h\"
")

  (SAMPLE-RATE "sr")
  (STATE
        ("boolean" "done" "false")
        ("LVAL" "src" "src"))
  
  (OUTER-LOOP "        if (susp->done) {
            togo = 0; /* indicate termination */
            break;    /* we're done */
        }
")
  (INNER-LOOP "LVAL rslt = xleval(cons(s_send, cons(src,
                                                 consa(s_next))));
            if (floatp(rslt)) {
                output = (sample_type) getflonum(rslt);
            } else {
                done = true;
                /* adjust togo to what it should have been */
                break;
            }")
  (CONSTANT "length" "samples" "array" "src")
  (TERMINATE COMPUTED)
)

