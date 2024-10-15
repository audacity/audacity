(CONVOLVE-ALG
(NAME "convolve")
(SUPPORT-FUNCTIONS "
void h_reverse(sample_type *h, long len)
{
    sample_type temp;
    int i;
    
    for (i = 0; i < len; i++) {
        temp = h[i];
        h[i] = h[len - 1];
        h[len - 1] = temp;
        len--;
    }
}
")

(ARGUMENTS ("sound_type" "x_snd") ("sound_type" "h_snd") )
(TABLE "h_snd")
(START (MIN x_snd))
(NOT-IN-INNER-LOOP "h_snd")
(STATE 
        ("table_type" "table" "sound_to_table(h_snd)")
        ("sample_type *" "h_buf" "susp->table->samples")
        ("double" "length_of_h" "susp->table->length")
        ("long" "h_len" "(long) susp->length_of_h;
         h_reverse(susp->h_buf, susp->h_len)")
        ("long" "x_buf_len" "2 * susp->h_len")
        ("sample_type *" "x_buffer_pointer" 
         "calloc((2 * (susp->h_len)), sizeof(float))")
        ("sample_type *" "x_buffer_current" "susp->x_buffer_pointer")  )
(ALWAYS-SCALE x_snd)
(TERMINATE (MIN x_snd))
(LOGICAL-STOP (MIN x_snd))
(INNER-LOOP-LOCALS 
  "            long i; double sum;")
(INNER-LOOP "/* see if we've reached end of x_buffer */
            if ((x_buffer_pointer + x_buf_len) <= (x_buffer_current + h_len)) {
                /* shift x_buffer from current back to base */
                for (i = 1; i < h_len; i++) {
                    x_buffer_pointer[i-1] = x_buffer_current[i];
                }    
                /* this will be incremented back to x_buffer_pointer below */
                x_buffer_current = x_buffer_pointer - 1;
            }
    
            x_buffer_current++;
    
            x_buffer_current[h_len - 1] = x_snd;
    
            sum = 0.0;
            for (i = 0; i < h_len; i++) {
                sum += x_buffer_current[i] * h_buf[i];
            }
            output = (sample_type) sum")
(CONSTANT "h_buf" "h_len" "x_buf_len" "table")
(SAMPLE-RATE "x_snd->sr")
(FINALIZATION "    table_unref(susp->table); 
    free(susp->x_buffer_pointer);\n")
)
