(FMFB-ALG
  (NAME "fmfb")
  (ARGUMENTS ("time_type" "t0") ("double" "hz") ("rate_type" "sr") 
             ("double" "index") ("time_type" "d")) 
  (TERMINATE (AFTER "d"))
  (STATE ("double" "yy" "0.0")
         ("double" "sin_y" "0.0")
         ("double" "xx" "0.0")
         ("double" "x_incr" 
                   "hz * SINE_TABLE_LEN / sr")
	 ("double" "index" "index * SINE_TABLE_LEN / PI2"))
  (INNER-LOOP "xx += x_incr;
            if (xx > SINE_TABLE_LEN) xx -= SINE_TABLE_LEN;
            /* xx incremented and index scaled to table index, and
               sin_y is a signal (-1 to +1) */
            yy = xx + index * sin_y;
            /* so yy is a table index */
            while (yy > SINE_TABLE_LEN) yy -= SINE_TABLE_LEN;
            while (yy < 0) yy += SINE_TABLE_LEN;
            sin_y = sine_table[(int) yy]; /* truncation gets valid index */
            /* sin_y is now a signal not ready for table lookup */
            output = (sample_type) sin_y")
  (CONSTANT "x_incr")
  (SAMPLE-RATE "sr")
  (SUPPORT-HEADER "#include \"sine.h\" /* sine_table and SINE_TABLE_LEN */\n")
)
