(MAXV-ALG
  (NAME "maxv")
  (ARGUMENTS ("sound_type" "s1") ("sound_type" "s2")) 
  (SAMPLE-RATE (MAX s1 s2))
  (COMMUTATIVE (s1 s2))
  (START (MAX s1 s2))
  (INNER-LOOP "{
\t\tdouble x1 = s1;
\t\tdouble x2 = s2;
\t\toutput = (sample_type) (x1 > x2 ? x1 : x2);
\t    }")
  (TERMINATE (MIN s1 s2))
  (LOGICAL-STOP (MIN s1 s2))
)
