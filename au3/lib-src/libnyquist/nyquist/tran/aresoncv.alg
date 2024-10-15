(ARESONCV-ALG
(NAME "aresoncv")
(ARGUMENTS ("sound_type" "s1") ("double" "hz") ("sound_type" "bw")
           ("int" "normalization"))
(LINEAR s1)
(ALWAYS-SCALE bw) ; because there's a necessary scale factor -PI2/sr
(START (MAX s1 bw))
(TERMINATE (MIN s1 bw))
(LOGICAL-STOP (MIN s1))
(SAMPLE-RATE (MAX s1))
(STEP-FUNCTION bw)
(STATE ("double" "c3co" "0.0")
       ("double" "coshz" "cos(hz * PI2 / s1->sr)")
       ("double" "c2" "0.0")
       ("double" "c1" "0.0")
       ("int" "normalization" "normalization")
       ("double" "y1" "0.0")
       ("double" "y2" "0.0;
    bw->scale = (float) (bw->scale * (-PI2 / s1->sr))"))
(DEPENDS ("c3co" "bw" "exp(bw)")
         ("c3p1" "bw" "c3co + 1.0" TEMP "double")
         ("c3t4" "bw" "c3co * 4.0" TEMP "double")
         ("omc3" "bw" "1.0 - c3co" TEMP "double")
         ("c2"   "bw" "c3t4 * coshz / c3p1")
         ("c1"   "bw" "(normalization == 0 ? 0.0 :
          (normalization == 1 ? 1.0 - omc3 * sqrt(1.0 - c2 * c2 / c3t4) :
              1.0 - sqrt(c3p1 * c3p1 - c2 * c2) * omc3 / c3p1))"))
(CONSTANT "c1" "c2" "c3co" "coshz" "normalization")
(FORCE-INTO-REGISTER normalization coshz)
(INNER-LOOP-LOCALS "            register double y0, current;
")
(INNER-LOOP "current = s1;
            output = (float) (y0 = c1 * current + c2 * y1 - c3co * y2);
            y2 = y1; y1 = y0 - current")
)
