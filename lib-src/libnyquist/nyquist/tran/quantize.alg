(QUANTIZE-ALG
  (NAME "quantize")
  (ARGUMENTS ("sound_type" "s1") ("long" "steps"))
  (INTERNAL-SCALING s1)
  (START (MIN s1))
  (STATE ("double" "factor" "s1->scale * steps;
    scale_factor = (sample_type) (1.0 / steps);"))
  (INNER-LOOP "{
\t\tfloat x = s1 * factor;
\t\tlong xx;
\t\tx = (x > 0.0F ? x + 0.5 : x - 0.5);
\t\txx = (long) x;
\t\toutput = (float) xx;
\t    }")
  (TERMINATE (MIN s1))
  (CONSTANT "factor")
  (LOGICAL-STOP (MIN s1))
)
