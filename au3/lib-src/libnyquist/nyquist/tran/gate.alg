(GATE-ALG
(NAME "gate")
(ARGUMENTS ("sound_type" "signal") ("time_type" "lookahead") 
           ("double" "risetime")
           ("double" "falltime") ("double" "floor") ("double" "threshold"))
(START (MIN signal))
(SUPPORT-FUNCTIONS "#define ST_HOLD 0
#define ST_FALL 1
#define ST_FALL_UNTIL 2
#define ST_OFF 3
#define ST_OFF_UNTIL 4
#define ST_RISE 5

/* Overview:
This operation generates an exponential rise and decay suitable for 
implementing a noise gate. The decay starts when the signal drops 
below threshold and stays there for longer than lookahead. 
Decay continues until the value reaches floor, at which point the 
decay stops and the value is held constant. Either during the decay 
or after the floor is reached, if the signal goes above threshold, 
then the output value will rise to 1.0 (0dB) at the point the 
signal crosses the threshold. Again, lookahead is used, so the rise
actually starts before the signal crosses the threshold. The rise 
rate is constant and set so that a rise from floor to 0dB occurs 
in the specified risetime.  Similarly, the fall rate is constant 
such that a fall from 0dB to the floor takes falltime.

Rather than looking ahead, the output actually lags the input by 
lookahead. The caller should advance the time of the input signal 
in order to get a correct output signal, and this will be taken 
care of in Lisp code.

The implementation is a finite-state machine that simultaneously 
computes the value and scans ahead for threshold crossings. Time 
points, remembered as sample counts are saved in variables:
    on_count -- the time at which the rise should complete
    off_count -- the time at which the fall should begin
    rise_factor -- multiply by this to get exponential rise
    fall_factor -- multiply by this to get exponential fall
    rise_samps -- number of samples for a full rise
    fall_samps -- number of samples for a full fall
    floor -- the lowest value to output
    threshold -- compare the signal s to this value
    start_rise -- the sample count at which a rise begins
    delay_len -- number of samples to look ahead, length of buffer
    state -- the current state of finite state machine
        (see the individual 'case' statements for description of states)
    value -- the current output value
    
computing fall_factor:
    factor ^ (sample_rate * time) == floor
    log(factor) * sample_rate * time == log(floor)
    log(factor) == log(floor) / (sample_rate * time)
    factor == exp(log(floor) / (sample_rate * time))
    
*/

void compute_start_rise(gate_susp_type susp)
{
    /* to compute when to start rise to achieve 0dB at on_count:
    let frt = full rise time = rise_time, art = actual rise time, 
        fft = full fall time = fall_time, aft = actual fall time
    If there's no time for a fft + frt, scale both the the fall time
    and rise times proportionally by available time / (fft + frt).

    When you enter ST_FALL, set start_fall = now.
    Let avail = available time = (on_count - start_fall).
    If there is not enough time for a full fall and full rise,
    i.e. if avail < (fft + frt) then let
        art = frt * avail / (fft + frt)
    So start rise at
        on_time - rise_time * (on_count-start_fall)/(rise_time+fall_time)
    */
    int64_t total = susp->rise_samps + susp->fall_samps;
    if ((susp->on_count - susp->start_fall) < total) {
        susp->start_rise = susp->on_count - 
            (susp->rise_samps * (susp->on_count - susp->start_fall)) / total;
    } else susp->start_rise = susp->on_count - susp->rise_samps;
}
")
(STATE 
       ("int64_t" "rise_samps" "(int64_t) (signal->sr * risetime + 0.5)")
       ("int64_t" "fall_samps" "(int64_t) (signal->sr * falltime + 0.5)")
       ("double" "floor" "floor; floor = log(floor / signal->scale)")
       ("double" "threshold" "threshold; threshold /= signal->scale")
       ("int64_t" "on_count" "0")
       ("int64_t" "off_count" "0")
       ("double" "rise_factor" "exp(- floor / susp->rise_samps)")
       ("double" "fall_factor" "exp(floor / susp->fall_samps)")
       ("int64_t" "start_fall" "-susp->fall_samps")
       ("int64_t" "start_rise" "0")
       ("int64_t" "stop_count" "0")
       ("long" "delay_len" "max(1, ROUND32(signal->sr * lookahead))")
       ("int" "state" "ST_OFF")
       ("double" "value" "susp->floor"))

(CONSTANT "lookahead" "rise_time" "rise_samps" "fall_time" "fall_samps"
          "floor" "threshold" "delay_len" "end_ptr"
          "rise_factor" "fall_factor")
(NOT-REGISTER delay_buf rise_factor fall_factor rise_time rise_samps
              fall_time fall_samps floor on_count start_fall start_rise)
(TERMINATE (MIN signal))
(INTERNAL-SCALING signal)
(INNER-LOOP "{
            sample_type future = signal;
            int64_t now = susp->susp.current + cnt + togo - n;
            
            switch (state) {
              /* hold at 1.0 and look for the moment to begin fall: */
              case ST_HOLD:
                  if (future >= threshold) {
                      off_count = now + delay_len;
                  } else if (now >= off_count) {
                      state = ST_FALL;
                      stop_count = now + susp->fall_samps;
                      susp->start_fall = now;
                  }
                  break;
              /* fall until stop_count while looking for next rise time */
              case ST_FALL:
                value *= susp->fall_factor;
                if (future >= threshold) {
                    off_count = susp->on_count = now + delay_len;
                    compute_start_rise(susp);
                    state = ST_FALL_UNTIL;
                } else if (now == stop_count) {
                    state = ST_OFF;
                    value = susp->floor;
                }
                break;
              /* fall until start_rise while looking for next fall time */
              case ST_FALL_UNTIL:
                value *= susp->fall_factor;
                if (future >= threshold) {
                       off_count = now + delay_len;
                   }
                   if (now >= susp->start_rise) {
                       state = ST_RISE;
                   } else if (now >= stop_count) {
                       state = ST_OFF_UNTIL;
                       value = susp->floor;
                   }
                   break;
              /* hold at floor (minimum value) and look for next rise time */
              case ST_OFF:
                if (future >= threshold) {
                    off_count = susp->on_count = now + delay_len;
                    compute_start_rise(susp);
                    if (now >= susp->start_rise) {
                        state = ST_RISE;
                    } else {
                        state = ST_OFF_UNTIL;
                    }
                }
                break;
              /* hold at floor until start_rise and look for next fall time */
              case ST_OFF_UNTIL:
                  if (future >= threshold) {
                      off_count = now + delay_len;
                  }
                  if (now >= susp->start_rise) {
                      state = ST_RISE;
                  }
                  break;
              /* rise while looking for fall time */
              case ST_RISE:
                value *= susp->rise_factor;
                if (future >= threshold) {
                    off_count = now + delay_len;
                }
                if (now >= susp->on_count) {
                    value = 1.0;
                    state = ST_HOLD;
                }
                break;
              }
              output = (sample_type) value;
            }")
)

