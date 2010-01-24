(LOG-ALG
  (NAME "log")
  (ARGUMENTS ("sound_type" "input"))
  (ALWAYS-SCALE input)
  (START (MIN input))
  (INNER-LOOP "output = (sample_type) log(input)")
  (TERMINATE (MIN input))
  (LOGICAL-STOP (MIN input))
)
