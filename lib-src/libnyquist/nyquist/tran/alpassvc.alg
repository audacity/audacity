(ALPASSVC-ALG
;;
;; delay is variable -- but we don't want to reallocate the delay buffer, so
;; use an additional parameter for the maximum allowable delay. The sound will
;; be written into the buffer every sample, but read using linear 
;; interpolation. As in tapv, duplicate the sample at the first and last 
;; locations in the buffer.
;;
(NAME "alpassvc")
(ARGUMENTS ("sound_type" "input") ("sound_type" "delaysnd") ("double" "feedback") 
           ("double" "maxdelay"))
(SAMPLE-RATE (MAX input))
(START (MAX input delaysnd))
(STATE ("float" "delay_scale_factor" "(float) (input->sr * delaysnd->scale)")
       ("double" "feedback" "feedback")
       ("long" "buflen" "max(2, (long) (input->sr * maxdelay + 2.5))")
       ("sample_type *" "delaybuf"
               "(sample_type *) calloc (susp->buflen + 1, sizeof(sample_type))")
       ("sample_type *" "delayptr" "susp->delaybuf")
;; since we allocate one extra sample, endptr points to the last sample
       ("sample_type *" "endptr" "susp->delaybuf + susp->buflen"))
(CONSTANT "feedback" "delaylen" "endptr" "delay_scale_factor")
(NOT-REGISTER delaybuf)
(LINEAR input)
(INTERNAL-SCALING delaysnd)
(TERMINATE (MIN input))
(INNER-LOOP-LOCALS "            register sample_type y, z, delaysamp;
            register int delayi;
            register sample_type *yptr;\n")
(INNER-LOOP "/* compute where to read y, we want y to be delay_snd samples
             * after delay_ptr, where we write the new sample. First, 
             * convert from seconds to samples. Note: don't use actual sound_type
             * names in comments! The translator isn't smart enough.
             */
            delaysamp = (sample_type) delaysnd * delay_scale_factor;
            delayi = (int) delaysamp; /* get integer part */
            delaysamp = delaysamp - delayi; /* get phase */
            yptr = delayptr + buflen - (delayi + 1);
            if (yptr >= endptr) yptr -= buflen;
            /* now get y, the out-put of the delay, using interpolation */
            /* note that as phase increases, we use more of yptr[0] because
               positive phase means longer buffer means read earlier sample */
            y = (float) ((yptr[0] * delaysamp) + (yptr[1] * (1.0 - delaysamp)));
            /* WARNING: no check to keep delaysamp in range, so 
               do this in LISP */

            *delayptr++ = z = (sample_type) (feedback * y + input);
            /* Time out to update the buffer:
             * this is a tricky buffer: buffer[0] == buffer[bufflen]
             * the logical length is bufflen, but the actual length
             * is bufflen + 1 to allow for a repeated sample at the
             * end. This allows for efficient interpolation.
             */
            if (delayptr > endptr) {
                delayptr = susp->delaybuf;
                *delayptr++ = *endptr;
            }
            output = (sample_type) (y - feedback * z)")
(FINALIZATION "free(susp->delaybuf);\n")
)

