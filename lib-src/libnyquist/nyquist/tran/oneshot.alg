(ONESHOT-ALG
  (NAME "oneshot")
  (ARGUMENTS ("sound_type" "input") ("double" "level") ("double" "ontime")) 
  ; (INTERNAL-SCALING input) ; scale factor not handled in level because scale could be negative
  (STATE ("double" "lev" "level")
         ("long" "oncount" "round(ontime * input->sr)")
         ("long" "cnt" "0"))
  (START (MIN input))
  (INNER-LOOP "double x = input;
            if (x > lev) cnt = oncount;
            cnt--;
            output = (cnt >= 0 ? 1.0F : 0.0F);")
  (CONSTANT "lev" "oncount")
  (TERMINATE (MIN input))
  (LOGICAL-STOP (MIN input))
)
