(FMOSC-ALG
(NAME "fmosc")
(ARGUMENTS ("sound_type" "s") ("double" "step") ("rate_type" "sr")
           ("double" "hz") ("time_type" "t0") ("sound_type" "s_fm")
           ("double" "phase"))
(TABLE "s")
(NOT-IN-INNER-LOOP "s")
(STATE ("table_type" "the_table" "sound_to_table(s)")
       ("double" "table_len" "susp->the_table->length")
       ("double" "ph_incr" "0")
       ("sample_type *" "table_ptr" "susp->the_table->samples")
       ("double" "phase" "compute_phase(phase, step, (long) susp->table_len,
        s->sr, sr, hz, &susp->ph_incr);
    s_fm->scale *= (sample_type) (hz != 0 ? (susp->ph_incr / hz)
                                          : s->sr / (sr * step_to_hz(step)))") ) ; cancel 0/0

(ALWAYS-SCALE s_fm)
(STEP-FUNCTION s_fm)
(TERMINATE (MIN s_fm))
(LOGICAL-STOP (MIN s_fm))
(INNER-LOOP-LOCALS "            long table_index;
            double x1;
")
(INNER-LOOP "table_index = (long) phase;
            x1 = table_ptr[table_index];
            output = (sample_type) (x1 + (phase - table_index) * 
                          (table_ptr[table_index + 1] - x1));
            phase += ph_incr + s_fm;
            while (phase > table_len) phase -= table_len;
            /* watch out for negative frequencies! */
            while (phase < 0) phase += table_len")
(CONSTANT "ph_incr" "table_len" "table_ptr" "table")

(SAMPLE-RATE "sr")
(FINALIZATION "    table_unref(susp->the_table);\n")
)
