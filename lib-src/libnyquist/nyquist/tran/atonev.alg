(ATONEV-ALG
(NAME "atonev")
(ARGUMENTS ("sound_type" "s1") ("sound_type" "hz"))
(LINEAR s1)
(ALWAYS-SCALE hz)
(START (MAX s1 hz))
(TERMINATE (MIN s1 hz))
(LOGICAL-STOP (MIN s1))
(STATE ("double" "cc" "0.0")
       ("double" "prev" "0.0;
    hz->scale = (sample_type) (hz->scale * (PI2 / s1->sr))"))
(SAMPLE-RATE (MAX s1))
(STEP-FUNCTION hz)
(DEPENDS ("bb" "hz" "2.0 - cos(hz)" TEMP "register double")
         ("cc" "hz" "bb - sqrt((bb * bb) - 1.0)"))
(CONSTANT "cc")
(INNER-LOOP-LOCALS "            double current;\n")
(INNER-LOOP "current = s1;
            prev = cc * (prev + current);
            output = (sample_type) prev;
            prev -= current")
)
