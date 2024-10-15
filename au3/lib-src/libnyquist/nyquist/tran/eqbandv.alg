; general order-2 IIR filter.
; a0 is assumed to be unity.
; for a1 and a2, our sign convention is opposite to Matlab's.

(EQBANDVVV-ALG
(NAME "eqbandvvv")
(ARGUMENTS ("sound_type" "s1") ("sound_type" "hz"))
;;           ("sound_type" "gain") ("sound_type" "width") )
(START (MIN s1 hz)) ;; gain width))
(TERMINATE (MIN s1 hz )) ;; gain width))
(LOGICAL-STOP (MIN s1 hz)) ;; gain width))
(SAMPLE-RATE (MAX s1))
(INTERNAL-SCALING s1)
(ALWAYS-SCALE hz)
(STEP-FUNCTION hz gain width)
(STATE	("double" "scale1" "s1->scale")
        ("double" "w1" "0.0")
        ("double" "sw" "0.0")
        ("double" "cw" "0.0")
        ("double" "J" "0.0")
        ("double" "g" "0.0")
        ("double" "b0" "0.0")
        ("double" "b1" "0.0")
        ("double" "b2" "0.0")
        ("double" "a0" "0.0")
        ("double" "a1" "0.0")
        ("double" "a2" "0.0"))
(DEPENDS ("w1" "hz" "PI2 * hz / s1->sr")
         ("sw" "hz" "sin(w1)")
         ("cw" "hz" "cos(w1)")
         ("b1" "hz" "-2.0 * cw")
         ("a1" "hz" "-b1")
         ;; ("J" "gain" "sqrt(gain)")
         ;; ("recompute" "width" "true")
         ("recompute" "hz" "true")
         ;; ("recompute" "gain" "true")
)
;; (JOINT-DEPENDENCY (("width" "hz")
;;"if (recompute) {"
;;"    recompute = false;"
;;"    g = sw * sinh(log_of_2_over_2 * width * w1 / sw);"
;;"    b0 = 1.0 + g * J;"
;;"    b2 = 1.0 - g * J;"
;;"    a0 = 1.0 + g / J;"
;;"    a2 = g / J - 1.0;"
;;"}"))
(FORCE-INTO-REGISTER recompute)
(STEP-FUNCTION hz) ;; gain width)
(CONSTANT "w1" "sw" "cw" "J" "g" "b0" "b1" "b2" "b3" "a0" "a1" "a2")
(INNER-LOOP-LOCALS "            double z0;")
(INNER-LOOP "z0 = s + a1*z1 + a2*z2;
            output = (sample_type) (z0*b0 + z1*b1 + z2*b2);
            z2 = z1; z1 = z0")
)

