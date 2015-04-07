(AMOSC-ALG
(NAME "amosc")
(ARGUMENTS ("sound_type" "input") ("double" "step") ("rate_type" "sr")
           ("double" "hz") ("time_type" "t0") ("sound_type" "amod")
           ("double" "phase"))
(TABLE "input")
(NOT-IN-INNER-LOOP "input")
(START (MIN input))
(STATE 
       ("double" "ph_incr" "0")
       ("table_type" "the_table" "sound_to_table(input)")
       ("sample_type *" "table_ptr" "susp->the_table->samples")
       ("double" "table_len" "susp->the_table->length")
       ("double" "phase" "compute_phase(phase, step, (long) susp->table_len,
        input->sr, sr, hz, &susp->ph_incr)") )
(ALWAYS-SCALE amod)
(TERMINATE (MIN amod))
(LOGICAL-STOP (MIN amod))
(INNER-LOOP "{
\t\tlong table_index = (long) phase;
\t\tdouble x1 = (double) (table_ptr[table_index]);
\t\toutput = (sample_type) ((x1 + (phase - table_index) * 
\t\t          (table_ptr[table_index + 1] - x1)) * amod);
\t\tphase += ph_incr;
\t\twhile (phase > table_len) phase -= table_len;
\t     }")
(CONSTANT "ph_incr" "table_len" "table_ptr" "the_table")
(SAMPLE-RATE "sr")
(FINALIZATION "    table_unref(susp->the_table);\n")
)

