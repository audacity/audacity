(FOLLOW-ALG
(NAME "follow")
(SUPPORT-FUNCTIONS 
"/* Description: this is a sophisticated envelope follower.
    The input is an envelope, e.g. something produced with
    the AVG function. The purpose of this function is to
    generate a smooth envelope that is generally not less
    than the input signal. In other words, we want to \"ride\"
    the peaks of the signal with a smooth function. The 
    algorithm is as follows: keep a current output value
    (called the \"value\"). The value is allowed to increase
    by at most rise_factor and decrease by at most fall_factor.
    Therefore, the next value should be between
    value * rise_factor and value * fall_factor. If the input
    is in this range, then the next value is simply the input.
    If the input is less than value * fall_factor, then the
    next value is just value * fall_factor, which will be greater
    than the input signal. If the input is greater than value *
    rise_factor, then we compute a rising envelope that meets
    the input value by working bacwards in time, changing the
    previous values to input / rise_factor, input / rise_factor^2,
    input / rise_factor^3, etc. until this new envelope intersects
    the previously computed values. There is only a limited buffer
    in which we can work backwards, so if the new envelope does not
    intersect the old one, then make yet another pass, this time
    from the oldest buffered value forward, increasing on each 
    sample by rise_factor to produce a maximal envelope. This will 
    still be less than the input.
    
    The value has a lower limit of floor to make sure value has a 
    reasonable positive value from which to begin an attack.
    
    Because this algorithm can make 2 passes through the buffer on
    sharply rising input signals, it is not particularly fast. The
    assumption is that it operates on fairly short buffers at low
    sample rates appropriate for gain control, so this should not
    matter.
    */

static sample_type *create_buf(double floor, long lookahead)
{
    sample_type *buf = (sample_type *) malloc(lookahead * sizeof(sample_type));
    int i;
    
    for (i = 0; i < lookahead; i++) buf[i] = (sample_type) floor;
    return buf;
}
")
(ARGUMENTS ("sound_type" "sndin") ("double" "floor") ("double" "risetime") 
           ("double" "falltime") ("long" "lookahead"))
(START (MIN sndin))
(STATE ("long" "lookahead" "lookahead = lookahead + 1")
       ("sample_type *" "delaybuf" "create_buf(floor, lookahead)")
       ("sample_type *" "delayptr" "susp->delaybuf")
       ("sample_type *" "prevptr" "susp->delaybuf + lookahead - 1;
    *(susp->prevptr) = (sample_type) floor;")
       ("sample_type *" "endptr" "susp->delaybuf + lookahead")
           ("double" "floor" "floor; floor = log(floor);")
           ("double" "rise_factor" "exp(- floor / (sndin->sr * risetime + 0.5))")
           ("double" "fall_factor" "exp(floor / (sndin->sr * falltime + 0.5))")
       ("double" "value" "susp->floor"))
(CONSTANT "feedback" "rise_factor" "fall_factor" "endptr")
(NOT-REGISTER delaybuf)
(ALWAYS-SCALE sndin)
(TERMINATE (MIN sndin))
(INNER-LOOP "sample_type current = sndin;
            sample_type high = (sample_type) (*prevptr * rise_factor);
            sample_type low = (sample_type) (*prevptr * fall_factor);
            if (low < floor) low = (sample_type) floor;
            if (current < low) *delayptr = (sample_type) low;
            else if (current < high) *delayptr = current;
            else /* current > high */ {
                /* work back from current */
                double rise_inverse = 1.0 / rise_factor;
                double temp = current * rise_inverse;
                boolean ok = false;
                sample_type *ptr = prevptr;
                int i;
                               
                for (i = 0; i < lookahead - 2; i++) {
                    if (*ptr < temp) {
                    *ptr-- = (sample_type) temp;
                    temp *= rise_inverse;
                    if (ptr < susp->delaybuf)
                        ptr = endptr - 1;
                    } else {
                        ok = true;
                        break;
                    }
                }
                if (!ok && (*ptr < temp)) {
                    temp = *ptr;
                    for (i = 0; i < lookahead - 1; i++) {
                        ptr++;
                        if (ptr == endptr) ptr = susp->delaybuf;
                        temp *= rise_factor;
                        *ptr = (sample_type) temp;
                    }
                } else *delayptr = current;
            }
            prevptr = delayptr++;
            if (delayptr == endptr) delayptr = susp->delaybuf;
            output = *delayptr")
(FINALIZATION "free(susp->delaybuf);")
)

