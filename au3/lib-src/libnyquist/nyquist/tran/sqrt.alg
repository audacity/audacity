(SQRT-ALG
  (NAME "sqrt")
  (ARGUMENTS ("sound_type" "input"))
  (ALWAYS-SCALE input)
  (START (MIN input))
  (INNER-LOOP "{ sample_type i = input; 
              if (i < 0) i = 0; 
              output = (sample_type) sqrt(i); }")
  (TERMINATE (MIN input))
  (LOGICAL-STOP (MIN input))
)
