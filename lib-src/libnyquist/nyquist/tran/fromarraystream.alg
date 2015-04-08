(FRMASTRM-ALG
  (NAME "fromarraystream")
  (ARGUMENTS ("time_type" "t0") ("rate_type" "sr") ("LVAL" "src"))
  (SUPPORT-FUNCTIONS "
/* IMPLEMENTATION NOTE:
 * The src argument is an XLisp object that returns either an
 * array of samples or NIL. The output of ifft is simply the
 * concatenation of the samples taken from the array. Later,
 * an ifft will be plugged in and this will return overlapped
 * adds of the ifft's.
 */

#include \"samples.h\"
")

  (SAMPLE-RATE "sr")
  (STATE
          ("long" "index" "0") ; samples index
          ("long" "length" "0"); samples length
          ("LVAL" "array" "NULL")
        ("LVAL" "src" "src")
        ("sample_type *" "samples" "NULL;"))
  
  (OUTER-LOOP "
        if (susp->src == NULL) {
out:	    togo = 0;	/* indicate termination */
            break;	/* we're done */
        }
        if (susp->index >= susp->length) {
            long i;
            susp->index = 0;
            susp->array = xleval(cons(s_send, cons(susp->src, consa(s_next))));
            susp->index = 0;
            if (susp->array == NULL) {
                susp->src = NULL;
                goto out;
            } else if (!vectorp(susp->array)) {
                xlerror(\"array expected\", susp->array);
            } else if (susp->samples == NULL) {
                /* assume arrays are all the same size as first one;
                   now that we know the size, we just have to do this
                   first allocation.
                 */
                susp->length = getsize(susp->array);
                if (susp->length < 1) xlerror(\"array has no elements\", susp->array);
                susp->samples = 
                    (sample_type *) calloc(susp->length,
                                           sizeof(sample_type));
            } else if (getsize(susp->array) != susp->length) {
                xlerror(\"arrays must all be the same length\", susp->array);
            }
            /* at this point, we have a new array and a place to put samples */
            for (i = 0; i < susp->length; i++) {
                LVAL elem = getelement(susp->array, i);
                if (ntype(elem) != FLONUM) {
                    xlerror(\"flonum expected\", elem);
                }
                susp->samples[i] = (sample_type) getflonum(elem);
            }
            susp->array = NULL; /* free the array */
        }
        togo = min(togo, susp->length - susp->index);
")
  (INNER-LOOP "output = samples[index++]")
  (CONSTANT "length" "samples" "array" "src")
  (TERMINATE COMPUTED)
  (FINALIZATION "    free(susp->samples);\n")

)












