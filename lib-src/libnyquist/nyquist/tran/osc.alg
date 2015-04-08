(OSC-ALG
(NAME "osc")
(ARGUMENTS ("sound_type" "input") ("double" "step") ("rate_type" "sr")
       ("double" "hz") ("time_type" "t0") ("time_type" "d")
       ("double" "phase"))
(TABLE "input")
(NOT-IN-INNER-LOOP "input")
(STATE 
       ("double" "ph_incr" "0")
       ("table_type" "the_table" "sound_to_table(input)")
       ("sample_type *" "table_ptr" "susp->the_table->samples")
       ("double" "table_len" "susp->the_table->length")
       ("double" "phase" "compute_phase(phase, step, (long) susp->table_len,
    input->sr, sr, hz, &susp->ph_incr)") )
;	"((hz * susp->table_len) / sr)")
(TERMINATE (AFTER "d"))
(INNER-LOOP "long table_index = (long) phase;
            double x1 = table_ptr[table_index];
            output = (sample_type) (x1 + (phase - table_index) * 
                  (table_ptr[table_index + 1] - x1));
            phase += ph_incr;
            while (phase >= table_len) phase -= table_len")
(CONSTANT "ph_incr" "table_len" "table_ptr")
(SAMPLE-RATE "sr")
(FINALIZATION "    table_unref(susp->the_table);\n")
)

