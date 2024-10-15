(ALLPOLES-ALG
(NAME "allpoles")
(ARGUMENTS ("sound_type" "x_snd")("LVAL" "ak_array")("double" "gain"))
(START (MIN x_snd))
(NOT-IN-INNER-LOOP "ak_array")
(ALWAYS-SCALE x_snd)
(TERMINATE (MIN x_snd))
(LOGICAL-STOP (MIN x_snd))

(STATE
      ("long" "ak_len" "0") ; length of coefs ak array
      ("LVAL" "ak_array" "ak_array")
      ("double" "gain" "gain")
      ("double *" "ak_coefs" "NULL") ; coefs array
      ("double *" "zk_buf" "NULL")   ; last values of output
      ("long" "index" "0")
)

(OUTER-LOOP "
      if (susp->ak_array == NULL) {
             togo = 0; /* indicate termination */
             break;    /* we're done */
      }
      else if (!vectorp(susp->ak_array))
           xlerror(\"array expected\", susp->ak_array);
      else if (susp->ak_coefs == NULL)
           {
               long i;
               susp->ak_len = getsize(susp->ak_array);
               if (susp->ak_len < 1) xlerror(\"array has not elements\", susp->ak_array);
               susp->ak_coefs = (double *) calloc(susp->ak_len, sizeof(double));
               susp->zk_buf   = (double *) calloc(susp->ak_len, sizeof(double));
 
              /* at this point we have a new array and a place to put ak coefs */
              for(i=0; i < susp->ak_len; i++) {
                 LVAL elem = getelement(susp->ak_array,i);
                 if (ntype(elem) != FLONUM) {
                      xlerror(\"flonum expected\", elem);
                 }
                 susp->ak_coefs[i] = getflonum(elem);
              }

            }
")


(CONSTANT "ak_array" "ak_coefs" "ak_len" "gain")
(SAMPLE-RATE (MAX x_snd))
(INNER-LOOP-LOCALS "            double z0; long xi; long xj;")

(INNER-LOOP "z0 = x_snd*gain;
            for (xi=0; xi < ak_len ; xi++) {
                xj = index + xi; if (xj >= ak_len) xj -= ak_len;
                z0 +=  ak_coefs[xi] * zk_buf[xj];
            }
            zk_buf[index] = z0;
            index++; if (index == ak_len) index = 0;
            output = (sample_type) z0")
(FINALIZATION "
     free(susp->zk_buf);
     free(susp->ak_coefs);
     susp->ak_array = NULL;  /* free array */\n")
)


