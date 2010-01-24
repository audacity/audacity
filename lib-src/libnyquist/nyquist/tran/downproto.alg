(DOWNSAMPLE-PROTO-ALG
  (NAME "down")
  (ARGUMENTS ("rate_type" "sr") ("sound_type" "s"))
  (SAMPLE-RATE "sr")
  (START (MIN s))
  (INLINE-INTERPOLATION T)
  (STATE )
  (INNER-LOOP "output = s")
  (TERMINATE (MIN s))
  (LOGICAL-STOP (MIN s))
  (TYPE-CHECK
"    if (s->sr < sr) {
        sound_unref(s);
        xlfail(\"snd-down: output sample rate must be lower than input\");
    }
")
)

