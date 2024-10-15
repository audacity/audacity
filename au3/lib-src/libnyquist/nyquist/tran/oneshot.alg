(ONESHOT-ALG
  (NAME "oneshot")
  (ARGUMENTS ("sound_type" "input") ("double" "level") ("double" "ontime")) 
  ; (INTERNAL-SCALING input) ; scale factor not handled in level because scale could be negative
  (STATE ("double" "lev" "level")
  ; maybe we need a ROUNDLONG: long is int64 on macOS but int32 on Win64
  ; the cast to long eliminates a warning on Visual Studio for Windows
         ("long" "oncount" "(long) ROUNDBIG(ontime * input->sr)")
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
