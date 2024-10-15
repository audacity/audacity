(ATONE-ALG
(NAME "atone")
(ARGUMENTS ("sound_type" "s") ("double" "hz"))
(START (MIN s))
(TERMINATE (MIN s))
(LOGICAL-STOP (MIN s))
(STATE ("double" "bb" "2.0 - cos(hz * PI2 / s->sr)" TEMP)
       ("double" "cc" "bb - sqrt((bb * bb) - 1.0)")
       ("double" "prev" "0.0"))
(CONSTANT "cc")
(INNER-LOOP-LOCALS "            double current;
")
(INNER-LOOP "current = s;
            /* use prev as temp variable ... */
            prev = cc * (prev + current);
            /* ... so we can do proper type conversion */
            output = (float) prev;
            prev -= current")
; old code was:
;	    prev = (output = cc * (prev + current)) - current;")
)

