(CHASE-ALG
  (NAME "chase")
  (ARGUMENTS ("sound_type" "input") ("double" "risetime") 
             ("double" "falltime")) 
  (STATE ("double" "level" "0.0")
         ("double" "upslope" "1.0 / (input->sr * risetime)")
         ("double" "downslope" "1.0 / (input->sr * falltime)"))
  (START (MIN input))
  (INNER-LOOP "double x = input;
            if (x > level) {
                level += upslope;
                if (x < level) level = x;
            } else {
                level -= downslope;
                if (x > level) level = x;
            }
            output = (sample_type) level")
  (TERMINATE (MIN input))
  (LOGICAL-STOP (MIN input))
)
