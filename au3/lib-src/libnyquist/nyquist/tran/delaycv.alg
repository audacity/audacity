(DELAYCV-ALG
(NAME "delaycv")
(ARGUMENTS ("sound_type" "input") ("time_type" "delay")
           ("sound_type" "feedback"))
(START (MAX input feedback))
(STATE ("long" "delaylen" "ROUND32(input->sr * delay)")
       ("sample_type *" "delaybuf"
               "(sample_type *) calloc (sizeof(double), susp->delaylen)")
       ("sample_type *" "delayptr" "susp->delaybuf")
       ("sample_type *" "endptr" "susp->delaybuf + susp->delaylen"))
(CONSTANT "delaylen" "delaybuf")
(NOT-REGISTER delaybuf)
(LINEAR input)
(TERMINATE (MIN input))
(INNER-LOOP "output = *delayptr;
            *delayptr = *delayptr * (sample_type) feedback +
                        (sample_type) input;
            if (++delayptr >= endptr) delayptr = susp->delaybuf")
(FINALIZATION "free(susp->delaybuf);\n")
)

