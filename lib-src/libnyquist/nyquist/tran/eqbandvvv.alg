; general order-2 IIR filter.
; a0 is assumed to be unity.
; for a1 and a2, our sign convention is opposite to Matlab's.

(EQBANDVVV-ALG
(NAME "eqbandvvv")
(ARGUMENTS ("sound_type" "input") ("sound_type" "hz") 
           ("sound_type" "gain") ("sound_type" "width") )
(SUPPORT-FUNCTIONS "#define log_of_2_over_2 0.3465735902799726547086\n")
(START (MIN input hz gain width))
(TERMINATE (MIN input hz gain width))
(LOGICAL-STOP (MIN input hz gain width))
(SAMPLE-RATE (MAX input))
;;(INTERNAL-SCALING input hz gain width)
(MATCHED-SAMPLE-RATE hz gain width)
(LINEAR input)
(ALWAYS-SCALE hz gain width)
(STEP-FUNCTION hz gain width)
(STATE	("double" "inp_scale" "input->scale")
        ("double" "w1" "0.0")
        ("double" "sw" "0.0")
        ("double" "cw" "0.0")
        ("double" "J" "0.0")
        ("double" "gg" "0.0")
        ("double" "b0" "0.0")
        ("double" "b1" "0.0")
        ("double" "b2" "0.0")
        ("double" "a0" "0.0")
        ("double" "a1" "0.0")
        ("double" "a2" "0.0")
        ("double" "z1" "0.0")
        ("double" "z2" "0.0")
        ("boolean" "recompute" "false")
        ("double" "inp_period" "1.0 / input->sr"))
(DEPENDS ("w1" "hz" "PI2 * hz * inp_period")
         ("sw" "hz" "sin(w1)")
         ("cw" "hz" "cos(w1)")
         ("b1" "hz" "-2.0 * cw")
         ("a1" "hz" "-b1")
         ("J" "gain" "sqrt(gain)")
         ("recompute" "width" "true")
         ("recompute" "hz" "true")
         ("recompute" "gain" "true")
)
(JOINT-DEPENDENCY (("width" "hz")
"if (recompute) {"
"    /* a0 = 1.0 + gg / J; */"
"    double a_0_recip = J / (J + gg);"
"    recompute = false;"
"    gg = sw * sinh(log_of_2_over_2 * "
"                   width * w1 / sw);"
"    b0 = (1.0 + gg * J) * a_0_recip;"
"    b1 *= a_0_recip;"
"    b2 = (1.0 - gg * J) * a_0_recip;"
"    a1 *= a_0_recip;"
"    a2 = (gg / J - 1.0) * a_0_recip;"
"}"))
(FORCE-INTO-REGISTER inp_period cw)
(CONSTANT "w1" "sw" "cw" "J" "gg" "b0" "b1" "b2" "b3" "a0" "a1" "a2" "inp_period")
(INNER-LOOP-LOCALS "            double z0;\n")
(INNER-LOOP "z0 = input + a1*z1 + a2*z2;
            output = (sample_type) (z0*b0 + z1*b1 + z2*b2);
            z2 = z1; z1 = z0")
)

