(INTEGRATE-ALG
(NAME "integrate")
(ARGUMENTS ("sound_type" "input"))
(START (MIN input))
(TERMINATE (MIN input))
(LINEAR input)
(LOGICAL-STOP (MIN input))
(STATE ("double" "integral" "0.0;
    scale_factor = (sample_type) (scale_factor / input->sr)"))
(INNER-LOOP "output = (sample_type) integral; integral += input")
)

