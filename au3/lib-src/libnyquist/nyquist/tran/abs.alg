(ABS-ALG
  (NAME "abs")
  (ARGUMENTS ("sound_type" "input"))
  (linear input)
  (START (MIN input))
  (INNER-LOOP "{ sample_type s = input; 
              sample_type o = s; 
              if (o < 0.0) o = -o; 
              output = o; }")
  (TERMINATE (MIN input))
  (LOGICAL-STOP (MIN input))
)
