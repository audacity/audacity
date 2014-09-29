(EXP-ALG
  (NAME "exp")
  (ARGUMENTS ("sound_type" "in"))
  (ALWAYS-SCALE in)
  (START (MIN in))
  (INNER-LOOP "output = (sample_type) exp(in)")
  (TERMINATE (MIN in))
  (LOGICAL-STOP (MIN in))
)
