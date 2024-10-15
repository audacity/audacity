(QUANTIZE-ALG
  (NAME "quantize")
  (ARGUMENTS ("sound_type" "s1") ("long" "steps"))
  (INTERNAL-SCALING s1)
  (START (MIN s1))
  (STATE ("sample_type" "factor" "s1->scale * steps;
    scale_factor = (sample_type) (1.0 / steps);"))
  (INNER-LOOP "{
            float x = s1 * factor;
            long xx;
            x = (x > 0.0F ? x + 0.5F : x - 0.5F);
            xx = (long) x;
            output = (float) xx;
            }")
  (TERMINATE (MIN s1))
  (CONSTANT "factor")
  (LOGICAL-STOP (MIN s1))
)
