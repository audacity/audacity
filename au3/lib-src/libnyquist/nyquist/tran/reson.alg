(RESON-ALG
(NAME "reson")
(ARGUMENTS ("sound_type" "s") ("double" "hz") ("double" "bw")
           ("int" "normalization"))
(START (MIN s))
(TERMINATE (MIN s))
(LOGICAL-STOP (MIN s))
(STATE ("double" "c3" "exp(bw * -PI2 / s->sr)")
       ("double" "c3p1" "susp->c3 + 1.0")
       ("double" "c3t4" "susp->c3 * 4.0")
       ("double" "omc3" "1.0 - susp->c3")
       ("double" "c2" "susp->c3t4 * cos(hz * PI2 / s->sr) / susp->c3p1")
       ("double" "c1" "(normalization == 0 ? 1.0 :
        (normalization == 1 ? susp->omc3 * sqrt(1.0 - susp->c2 * susp->c2 / susp->c3t4) :
            sqrt(susp->c3p1 * susp->c3p1 - susp->c2 * susp->c2) * susp->omc3 / susp->c3p1))")
       ("double" "y1" "0.0")
       ("double" "y2" "0.0"))
(CONSTANT "c1" "c2" "c3")
(INNER-LOOP "{ double y0 = c1 * s + c2 * y1 - c3 * y2;
              output = (sample_type) y0;	
              y2 = y1; y1 = y0; }")
)

