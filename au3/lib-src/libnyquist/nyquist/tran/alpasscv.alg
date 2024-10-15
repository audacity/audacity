(ALPASSCV-ALG
(NAME "alpasscv")
(ARGUMENTS ("sound_type" "input") ("time_type" "delay") ("sound_type" "feedback"))
(SAMPLE-RATE (MAX input))
(START (MAX input feedback))
(STATE ("long" "delaylen" "max(1, ROUND32(input->sr * delay))")
       ("sample_type *" "delaybuf"
               "(sample_type *) calloc (susp->delaylen, sizeof(sample_type))")
       ("sample_type *" "delayptr" "susp->delaybuf")
       ("sample_type *" "endptr" "susp->delaybuf + susp->delaylen"))
(CONSTANT "delaylen" "endptr")
(NOT-REGISTER delaybuf)
(LINEAR input)
(TERMINATE (MIN input))
(INNER-LOOP-LOCALS "            register sample_type y, z, fb;\n")
(INNER-LOOP "y = *delayptr;
            *delayptr++ = z = 
                    ((fb = (sample_type) feedback) * y + input);
            output = (sample_type) (y - fb * z);
            if (delayptr >= endptr) delayptr = susp->delaybuf")
(FINALIZATION "free(susp->delaybuf);")
)

