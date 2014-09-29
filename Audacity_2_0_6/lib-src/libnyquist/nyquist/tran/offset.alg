(SCALE-ALG
  (NAME "offset")
  (ARGUMENTS ("sound_type" "s1") ("double" "offset"))
  (STATE ("sample_type" "offset" "(sample_type) offset"))
  (CONSTANT "offset")
  (START (MIN s1))
  (INNER-LOOP "output = s1 + offset")
  (TERMINATE (MIN s1))
  (LOGICAL-STOP (MIN s1))
)

