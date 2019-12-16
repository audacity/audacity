(SINE-ALG
(NAME "sine")
(ARGUMENTS ("time_type" "t0") ("double" "hz") ("rate_type" "sr") ("time_type" "d"))
(STATE ("long" "phase" "0")
       ("long" "ph_incr" "ROUND32(((hz * SINE_TABLE_LEN) * (1 << SINE_TABLE_SHIFT) / sr))"))
(TERMINATE (AFTER "d"))
(INNER-LOOP "output = sine_table[phase >> SINE_TABLE_SHIFT];
            phase += ph_incr;
            phase &= SINE_TABLE_MASK")
(MAINTAIN ("phase" 
 "susp->phase = (susp->phase + susp->ph_incr * togo) & SINE_TABLE_MASK"))
(CONSTANT "ph_incr")
(SAMPLE-RATE "sr")
(SUPPORT-HEADER "#define SINE_TABLE_LEN 2048
#define SINE_TABLE_MASK 0x7FFFFFFF
#define SINE_TABLE_SHIFT 20
void sine_init();
extern sample_type sine_table[];
")
(SUPPORT-FUNCTIONS "
sample_type sine_table[SINE_TABLE_LEN + 1];

void sine_init()
{
    int i;
    for (i = 0; i <= SINE_TABLE_LEN; i++)
        sine_table[i] = (sample_type) (sin((PI * 2 * i) / SINE_TABLE_LEN));
}
")
)

