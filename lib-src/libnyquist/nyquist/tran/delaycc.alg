(DELAY-ALG
(NAME "delay")
(ARGUMENTS ("sound_type" "input") ("time_type" "delay") ("double" "feedback"))
(START (MIN input))
(STATE ("double" "feedback" "feedback")
       ("long" "delaylen" "max(1, ROUND32(input->sr * delay))")
       ("sample_type *" "delaybuf"
               "(sample_type *) calloc (susp->delaylen, sizeof(sample_type))")
       ("sample_type *" "delayptr" "susp->delaybuf")
       ("sample_type *" "endptr" "susp->delaybuf + susp->delaylen"))
(CONSTANT "feedback" "delaylen" "endptr")
(NOT-REGISTER delaybuf)
(LINEAR input)
(TERMINATE (MIN input))
(INNER-LOOP "output = *delayptr;
            *delayptr = (sample_type) (*delayptr * feedback) + input;
            if (++delayptr >= endptr) delayptr = susp->delaybuf")
(FINALIZATION "free(susp->delaybuf);\n")
)

