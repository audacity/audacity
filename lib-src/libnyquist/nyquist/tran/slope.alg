(SLOPE-ALG
  (NAME "slope")
  (ARGUMENTS ("sound_type" "input"))
  (STATE ("sample_type" "prev" "0.0F")
          ("double" "scale" "input->sr * input->scale"))
  (INTERNAL-SCALING input)
  (CONSTANT "scale")
  (DELAY 1)
  (START (MIN input))
  (INNER-LOOP "{ register sample_type x = input;
              output = (sample_type) ((x - prev) * scale);
              prev = x;}")
  (TERMINATE (MIN input))
  (LOGICAL-STOP (MIN input))
)
