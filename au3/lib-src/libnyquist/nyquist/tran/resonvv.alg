(RESONVV-ALG
(NAME "resonvv")
(ARGUMENTS ("sound_type" "s1") ("sound_type" "hz1") ("sound_type" "bw")
           ("int" "normalization"))
(LINEAR s1)
(ALWAYS-SCALE hz1 bw)
(START (MAX s1 hz1 bw))
(TERMINATE (MIN s1 hz1 bw))
(LOGICAL-STOP (MIN s1))
(SAMPLE-RATE (MAX s1))
(STEP-FUNCTION hz1 bw)
(STATE ("double" "scale1" "s1->scale")
       ("double" "c3co" "0.0")
       ("double" "c3p1" "0.0")
       ("double" "c3t4" "0.0")
       ("double" "omc3" "0.0")
       ("double" "coshz" "0.0")
       ("double" "c2" "0.0")
       ("double" "c1" "0.0")
       ("boolean" "recompute" "false")
       ("int" "normalization" "normalization")
       ("double" "y1" "0.0")
       ("double" "y2" "0.0;
    hz1->scale = (sample_type) (hz1->scale * (PI2 / s1->sr));
    bw->scale = (sample_type) (bw->scale * (-PI2 / s1->sr));"))
(DEPENDS ("c3co"  "bw" "exp(bw)")
         ("c3p1"  "bw" "c3co + 1.0")
          ("c3t4"  "bw" "c3co * 4.0")
         ("omc3"  "bw" "1.0 - c3co")
         ("recompute" "bw" "true")
         ("coshz" "hz1" "cos(hz1)")
         ("recompute" "hz1" "true"))
(JOINT-DEPENDENCY (("hz1" "bw")
"if (recompute) {"
"    recompute = false;"
"    c2 = c3t4 * coshz / c3p1;"
"    c1 = (normalization == 0 ? 1.0 :"
"          (normalization == 1 ? omc3 * sqrt(1.0 - c2 * c2 / c3t4) :"
"           sqrt(c3p1 * c3p1 - c2 * c2) * omc3 / c3p1)) * scale1;"
"}"))
(CONSTANT "c1" "c2" "c3co" "coshz" "c3p1" "c3t4" "omc3" 
          "normalization" "scale1")
(FORCE-INTO-REGISTER recompute) ;c3t4 c3p1 normalization omc3 scale1
(INNER-LOOP "{ double y0 = c1 * s1 + c2 * y1 - c3co * y2;
              output = (sample_type) y0;
              y2 = y1; y1 = y0; }")
)
