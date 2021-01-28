(BUZZ-ALG
(NAME "buzz")
(ARGUMENTS ("long" "n") ("rate_type" "sr") ("double" "hz")
           ("time_type" "t0") ("sound_type" "s_fm"))
(SUPPORT-FUNCTIONS "
#include \"sine.h\"
")
(STATE ("double" "ph_incr" "0")
       ("float" "n_2_r" "1.0F / (n * 2)")
       ("float" "n_2_p1" "(float) ((n * 2) + 1)")
	;; note: hz * 0.5 because this formula generates tones an octave up,
	;; we also have to correct for the modulation s_fm. If hz != 0, then
    	;; ph_incr is the increment per hz, so ph_incr/hz is the right scale
	;; factor. If hz == 0, then the ph_incr/hz is SINE_TABLE_LEN * 0.5 / sr.
       ("double" "phase" "compute_phase(PI*0.5, 69.0, SINE_TABLE_LEN,
        SINE_TABLE_LEN * 440.0, sr, hz * 0.5, &susp->ph_incr);
    s_fm->scale *= hz != 0 ? (sample_type) (susp->ph_incr / hz)
                           : (sample_type) (SINE_TABLE_LEN * 0.5 / sr)")) ;cancel 0/0

(ALWAYS-SCALE s_fm)
(STEP-FUNCTION s_fm)
(TERMINATE (MIN s_fm))
(LOGICAL-STOP (MIN s_fm))
(INNER-LOOP-LOCALS "            long table_index;
            double x1;
            sample_type num, denom, samp;\n")
(INNER-LOOP "table_index = (long) phase;
            x1 = sine_table[table_index];
            denom = (sample_type) (x1 + (phase - table_index) * 
                          (sine_table[table_index + 1] - x1));
            if (denom < 0.001 && denom > -0.005) {
                samp = 1.0F;
            } else {
                double phn2p1 = phase * n_2_p1 * (1.0/SINE_TABLE_LEN);
                phn2p1 = (phn2p1 - (long) phn2p1) * SINE_TABLE_LEN;
                table_index = (long) phn2p1;
                x1 = sine_table[table_index];
                num = (sample_type) (x1 + (phn2p1 - table_index) *
                        (sine_table[table_index + 1] - x1));
                samp = ((num / denom) - 1.0F) * n_2_r;
            }
            output = samp;
            phase += ph_incr + s_fm;
            while (phase > SINE_TABLE_LEN) phase -= SINE_TABLE_LEN;
            /* watch out for negative frequencies! */
            while (phase < 0) phase += SINE_TABLE_LEN")
(CONSTANT "ph_incr" "n_2_p1" "n_2_r")
(SAMPLE-RATE "sr")
)

