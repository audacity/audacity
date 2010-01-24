(ARESON-ALG
(NAME "areson")
(ARGUMENTS ("sound_type" "input") ("double" "hz") ("double" "bw")
           ("int" "normalization"))
(START (MIN input))
(TERMINATE (MIN input))
(LOGICAL-STOP (MIN input))
(LINEAR input)
(STATE ("double" "c3" "exp(bw * -PI2 / input->sr)")
       ("double" "c3p1" "susp->c3 + 1.0" TEMP)
       ("double" "c3t4" "susp->c3 * 4.0" TEMP)
       ("double" "omc3" "1.0 - susp->c3" TEMP)
       ("double" "c2" "c3t4 * cos(hz * PI2 / input->sr) / c3p1")
       ("double" "c1" "(normalization == 0 ? 0.0 :
        (normalization == 1 ? 1.0 - omc3 * sqrt(1.0 - susp->c2 * susp->c2 / c3t4) :
            1.0 - sqrt(c3p1 * c3p1 - susp->c2 * susp->c2) * omc3 / c3p1))")
       ("double" "y1" "0.0")
       ("double" "y2" "0.0"))
(CONSTANT "c1" "c2" "c3")
(INNER-LOOP-LOCALS "            register double y0, current;")
(INNER-LOOP "current = input;
            output = (sample_type) (y0 = c1 * current + c2 * y1 - c3 * y2);
            y2 = y1; y1 = y0 - current")
)
