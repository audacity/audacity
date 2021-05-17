(FMFBV-ALG
  (NAME "fmfbv") ; FM feedback unit generator
  (ARGUMENTS ("time_type" "t0") ("double" "hz") ("rate_type" "sr")
             ("sound_type" "index")) 
  (START (MIN index))
  (TERMINATE (MIN index))
  (LOGICAL-STOP (MIN index))
  (STEP-FUNCTION index)
  (STATE ("double" "yy" "0.0")
         ("double" "sin_y" "0.0")
         ("double" "phase" "0.0")
	 ("double" "ph_incr" "hz * SINE_TABLE_LEN / sr;
    index->scale *= SINE_TABLE_LEN / (sample_type) PI2
"))
  (INNER-LOOP "phase += ph_incr;
            if (phase > SINE_TABLE_LEN) phase -= SINE_TABLE_LEN;
            /* PHASE is incremented and INDEX scaled to table INDEX, and
               sin_y is a signal (-1 to +1) */
            yy = phase + index * sin_y;
            /* so yy is a table index */
            while (yy > SINE_TABLE_LEN) yy -= SINE_TABLE_LEN;
            while (yy < 0) yy += SINE_TABLE_LEN;
            sin_y = sine_table[(int) yy]; /* truncation gets valid index */
            /* sin_y is now a signal ready for table lookup */
            output =  (sample_type) sin_y")
  (CONSTANT "ph_incr")
  (SAMPLE-RATE "sr")
  (SUPPORT-HEADER "#include \"sine.h\" /* sine_table and SINE_TABLE_LEN */
")
)
