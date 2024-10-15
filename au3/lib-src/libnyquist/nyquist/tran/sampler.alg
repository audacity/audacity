(SAMPLER-ALG
(NAME "sampler")
(ARGUMENTS ("sound_type" "s") ("double" "step") ("double" "loop_start") 
        ("rate_type" "sr") ("double" "hz") ("time_type" "t0") 
        ("sound_type" "s_fm") ("long" "npoints"))
(TABLE "s")
(NOT-IN-INNER-LOOP "s")
(STATE 
       ("double" "loop_to" "loop_start * s->sr")
       ("table_type" "the_table" "sound_to_table(s)")
       ("sample_type *" "table_ptr" "susp->the_table->samples")
       ("double" "table_len" "susp->the_table->length;")
       ("double" "phase" "0.0")
       ("double" "ph_incr" "(s->sr / sr) * hz / step_to_hz(step);
    s_fm->scale = (sample_type) (s_fm->scale * (susp->ph_incr / hz));
    /* make sure parameters are ok
     *  the table must have samples
     *  the loop_start must be before the end of the table
     *  ph_incr must be positive
     *  sr should be positive
     */
    { long index = (long) susp->loop_to;
      double frac = susp->loop_to - index;
       if (susp->the_table->length <= 1) { 
             xlfail(\"sampler table length <= 1\");
       }
      if (index > ROUND32(susp->table_len) - 2 ||
          index < 0) {
          xlfail(\"sampler loop start not within samples\");
      }
      if (susp->ph_incr <= 0) {
          xlfail(\"sampler phase increment <= 0\");
      }
      if (sr <= 0) {
          xlfail(\"sampler sample rate <= 0\");
      }
      /* copy interpolated start to last entry */
      susp->table_ptr[ROUND32(susp->table_len)] =
          (sample_type) (susp->table_ptr[index] * (1.0 - frac) + 
                         susp->table_ptr[index + 1] * frac);}")
 )

(ALWAYS-SCALE s_fm)
(STEP-FUNCTION s_fm)
(TERMINATE (MIN s_fm))
(LOGICAL-STOP (MIN s_fm))
(INNER-LOOP-LOCALS "	    long table_index;
            double x1;
")
(INNER-LOOP "table_index = (long) phase;
            x1 = table_ptr[table_index];
            output = (sample_type) (x1 + (phase - table_index) * 
                          (table_ptr[table_index + 1] - x1));
            phase += ph_incr + s_fm;
            while (phase > table_len) phase -= (table_len - loop_to);
            /* watch out for negative frequencies! */
            if (phase < 0) phase = 0")
(CONSTANT "ph_incr" "table_len" "table_ptr" "loop_to")
(SAMPLE-RATE "sr")
(FINALIZATION "    table_unref(susp->the_table);\n")
)

