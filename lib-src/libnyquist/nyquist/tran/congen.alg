(CONGEN
(NAME "congen")
(ARGUMENTS ("sound_type" "sndin") ("double" "risetime") ("double" "falltime"))
(START (MIN sndin))
(STATE ("double" "value" "0")
       ("double" "rise_factor" "exp(log(0.5) / (sndin->sr * risetime))")
       ("double" "fall_factor" "exp(log(0.5) / (sndin->sr * falltime))"))
(CONSTANT "fall_factor" "rise_factor")
(TERMINATE (MIN sndin))
(INNER-LOOP "sample_type current = sndin;
            if (current > value) {
                value = current - (current - value) * rise_factor;
            } else {
                value = current - (current - value) * fall_factor;
            }
            output = (sample_type) value")
)
