;; this is the beginnings of a new function that just passes input to output until
;; a given "release time" at which point the output decays exponentially to zero.
;; this is hard to do in Nyquist without a new primitive because the amplitude of
;; the exponential decay depends on the value of the input at some given time.
;; (Yes, you can evaluate that point, but then you have to compute all the samples,
;; and they will be held in memory, which might not be a good thing.)

(EXPREL-ALG
(NAME "exprel")
(ARGUMENTS ("sound_type" "signal") ("time_type" "release_time") 
           ("double" "fall_time"))
(SUPPORT-FUNCTIONS "#define ST_HOLD 0
#define ST_FALL 1
#define ST_FALL_UNTIL 2
#define ST_OFF 3
#define ST_OFF_UNTIL 4
#define ST_RISE 5

/* Overview:
This operation passes its input to its output until the release time. 
Then, it takes the last sample output as a starting point for an 
exponential decay, with a duration of falltime.
*/

")
(STATE 
       ("long" "release_time" "signal->sr * release_time + 0.5")
       ("double" "fall_time" "signal->sr * falltime + 0.5")
       ("sample_type" "value" "0")
       ("bool" "falling" "0"))
(TERMINATE (MIN signal))
(LOGICAL-STOP "release_time")
(LINEAR signal)
(INNER-LOOP "{
              sample_type result;
              if (falling) {
                  value = value * decay;
                  result = value;
              } else {
                  result = signal;
                  if (release_time <= susp->susp.current + cnt + togo - n) {
                      value = result;
                      falling = 1;
                  }
              }
              output = (sample_type) value;
            }")
)

need to do logical stop time and termination time
 
