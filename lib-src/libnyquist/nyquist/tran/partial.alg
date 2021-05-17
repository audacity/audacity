(PARTIAL-ALG
(NAME "partial")
(ARGUMENTS ("rate_type" "sr") ("double" "hz") ("sound_type" "env"))
(SUPPORT-FUNCTIONS "
#include \"sine.h\"
")
(START (MIN env))
(STATE ("long" "phase" "0")
       ("long" "ph_incr" "ROUND32((hz * SINE_TABLE_LEN) * (1 << SINE_TABLE_SHIFT) / sr)"))
(TERMINATE (MIN env))
(LOGICAL-STOP (MIN env))
(INNER-LOOP "output = sine_table[phase >> SINE_TABLE_SHIFT] * (sample_type) env;
            phase += ph_incr;
            phase &= SINE_TABLE_MASK;")
(MAINTAIN ("phase" 
 "susp->phase = (susp->phase + susp->ph_incr * togo) & SINE_TABLE_MASK"))
(CONSTANT "ph_incr")
(SAMPLE-RATE "sr")
)

