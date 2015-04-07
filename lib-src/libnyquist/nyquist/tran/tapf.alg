(TAPF-ALG
(NAME "tapf")
(ARGUMENTS ("sound_type" "s1") ("double" "offset") ("sound_type" "vardelay")
           ("double" "maxdelay"))
(STEP-FUNCTION "vardelay")
(INTERNAL-SCALING vardelay)
;(ALWAYS-SCALE s1)
(START (MAX s1 vardelay))
(TERMINATE (MIN s1 vardelay))
(LOGICAL-STOP (MIN s1))
(STATE ("double" "offset" "offset * s1->sr")
       ("double" "vdscale" "vardelay->scale * s1->sr")
       ("long" "maxdelay" "(long)(maxdelay * s1->sr)")
       ("long" "bufflen" "max(2, (long) (susp->maxdelay + 0.5))")
       ("long" "index" "susp->bufflen")
       ("sample_type *" "buffer" 
        "(sample_type *) calloc(susp->bufflen + 1, sizeof(sample_type))"))
(SAMPLE-RATE (MAX s1))
(CONSTANT "maxdelay" "offset" "vdscale" "buffer")
(INNER-LOOP-LOCALS "            long phase;
")
(INNER-LOOP "phase = (long) (vardelay * vdscale + offset);
            /* now phase should give number of samples of delay */
            if (phase < 0) phase = 0;
            else if (phase > maxdelay) phase = maxdelay;
            phase = index - phase;
            /* now phase is a location in the buffer (before modulo) */
    
            /* Time out to update the buffer:
             * this is a tricky buffer: buffer[0] == buffer[bufflen]
             * the logical length is bufflen, but the actual length
             * is bufflen + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */ 
            buffer[index++] = s1;
            if (index >= bufflen) {
                index = 0;
            }
    
            /* back to the phase calculation: 
             * use conditional instead of modulo
             */
            if (phase < 0) phase += bufflen;
            output = (sample_type) (buffer[phase])")
(FINALIZATION "    free(susp->buffer);\n")
)
