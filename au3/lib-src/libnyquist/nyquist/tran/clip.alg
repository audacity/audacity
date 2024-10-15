(CLIP-ALG
  (NAME "clip")
  (ARGUMENTS ("sound_type" "s") ("double" "level")) 
  (STATE ("sample_type" "level" "(sample_type) level"))
  (START (MIN s))
  (INNER-LOOP "double x = s; 
            output = (sample_type) 
            (x > level ? 
             level : (x < -level ? -level : x))")
  (TERMINATE (MIN s))
  (LOGICAL-STOP (MIN s))
)
