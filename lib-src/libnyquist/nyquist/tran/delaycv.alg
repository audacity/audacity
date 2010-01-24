(DELAYCV-ALG
(NAME "delaycv")
(ARGUMENTS ("sound_type" "s") ("time_type" "delay") ("sound_type" "feedback"))
(START (MAX s feedback))
(STATE ("long" "delaylen" "round(s->sr * delay)")
       ("sample_type *" "delaybuf"
               "(sample_type *) calloc (sizeof(double), susp->delaylen)")
       ("sample_type *" "delayptr" "susp->delaybuf")
       ("sample_type *" "endptr" "susp->delaybuf + susp->delaylen"))
(CONSTANT "delaylen" "delaybuf")
(NOT-REGISTER delaybuf)
(LINEAR s)
(TERMINATE (MIN s))
(INNER-LOOP "output = *delayptr;
         *delayptr = *delayptr * feedback + s;
         if (++delayptr >= endptr) delayptr = susp->delaybuf;")
(FINALIZATION "free(susp->delaybuf);")
)

