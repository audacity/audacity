(TAPV-ALG
(NAME "tapv")
(ARGUMENTS ("sound_type" "s1") ("double" "offset") ("sound_type" "vardelay")
           ("double" "maxdelay"))
(INTERNAL-SCALING vardelay)
(START (MAX s1 vardelay))
(TERMINATE (MIN s1 vardelay))
(LOGICAL-STOP (MIN s1))
(STATE ("double" "offset" "offset * s1->sr")
       ("double" "vdscale" "vardelay->scale * s1->sr")
       ("double" "maxdelay" "maxdelay * s1->sr")
       ("long" "bufflen" "max(2, (long) (susp->maxdelay + 1.5))")
       ("long" "index" "susp->bufflen")
       ("sample_type *" "buffer" 
        "(sample_type *) calloc(susp->bufflen + 1, sizeof(sample_type))"))
(SAMPLE-RATE (MAX s1))
(CONSTANT "maxdelay" "offset" "vdscale" "buffer")
(INNER-LOOP-LOCALS "            double phase;
            long i;
")
(INNER-LOOP "phase = vardelay * vdscale + offset;
            /* now phase should give number of samples of delay */
            if (phase < 0) phase = 0;
            else if (phase > maxdelay) phase = maxdelay;
            phase = (double) index - phase;
            /* now phase is a location in the buffer (before modulo) */
    
            /* Time out to update the buffer:
             * this is a tricky buffer: buffer[0] == buffer[bufflen]
             * the logical length is bufflen, but the actual length
             * is bufflen + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */ 
            buffer[index++] = s1;
            if (index > bufflen) {
                buffer[0] = buffer[bufflen];
                index = 1;
            }
    
            /* back to the phase calculation: 
             * use conditional instead of modulo
             */
            if (phase < 0) phase += bufflen;
            i = (long) phase;    /* put integer part in i */
            phase -= (double) i; /* put fractional part in phase */	     
                output = (sample_type) (buffer[i] * (1.0 - phase) + 
                                        buffer[i + 1] * phase)")
(FINALIZATION "    free(susp->buffer);\n")
)

