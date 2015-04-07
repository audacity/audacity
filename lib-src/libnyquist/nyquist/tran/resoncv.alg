(RESONCV-ALG
(NAME "resoncv")
(ARGUMENTS ("sound_type" "s1") ("double" "hz") ("sound_type" "bw")
           ("int" "normalization"))
(INTERNAL-SCALING s1)
(ALWAYS-SCALE bw)
(START (MAX s1 bw))
(TERMINATE (MIN s1 bw))
(LOGICAL-STOP (MIN s1))
(SAMPLE-RATE (MAX s1))
(STEP-FUNCTION bw)
(STATE ("double" "scale1" "s1->scale")
       ("double" "c3co" "0.0")
       ("double" "coshz" "cos(hz * PI2 / s1->sr)")
       ("double" "c2" "0.0")
       ("double" "c1" "0.0")
       ("int" "normalization" "normalization")
       ("double" "y1" "0.0")
       ("double" "y2" "0.0;
    bw->scale = (sample_type) (bw->scale * (-PI2 / s1->sr))"))
(DEPENDS ("c3co" "bw" "exp(bw)")
         ("c3p1" "bw" "c3co + 1.0" TEMP "double")
         ("c3t4" "bw" "c3co * 4.0" TEMP "double")
         ("omc3" "bw" "1.0 - c3co" TEMP "double")
         ("c2"   "bw" "c3t4 * coshz / c3p1")
         ("c1"   "bw" "(normalization == 0 ? 1.0 :
          (normalization == 1 ? omc3 * sqrt(1.0 - c2 * c2 / c3t4) :
              sqrt(c3p1 * c3p1 - c2 * c2) * omc3 / c3p1)) * scale1"))
(CONSTANT "c1" "c2" "c3co" "coshz" "normalization" "scale1")
(FORCE-INTO-REGISTER normalization coshz scale1)
(INNER-LOOP "{ double y0 = c1 * s1 + c2 * y1 - c3co * y2;
              output = (sample_type) y0; 
              y2 = y1; y1 = y0; }")
)
