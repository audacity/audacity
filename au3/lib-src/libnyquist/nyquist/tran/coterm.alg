(COTERM-ALG
  (NAME "coterm")
  (ARGUMENTS ("sound_type" "s1") ("sound_type" "s2"))
  (SAMPLE-RATE (MAX s1))
  (START (MAX s1 s2))
  (INNER-LOOP "{ /* ignore s2 */ output = s1;}")
  (LINEAR s1)
  (TERMINATE (MIN s1 s2))
  (LOGICAL-STOP (MIN s1 s2))
  (INTERNAL-SCALING s2)
)

