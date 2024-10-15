; general order-2 IIR filter.
; a0 is assumed to be unity.
; for a1 and a2, our sign convention is opposite to Matlab's.

(SNDBIQUAD-ALG
(NAME "biquadfilt")
(LISPNAME "biquad")
(ARGUMENTS ("sound_type" "s") 
        ("double" "b0") ("double" "b1") ("double" "b2") 
        ("double" "a1") ("double" "a2")
        ("double" "z1init") ("double" "z2init"))
(LINEAR s)
(START (MIN s))
(TERMINATE (MIN s))
(LOGICAL-STOP (MIN s))
(STATE	("double" "z1" "z1init")
        ("double" "z2" "z2init")
        ("double" "b0" "b0")
        ("double" "b1" "b1")
        ("double" "b2" "b2")
        ("double" "a1" "a1")
        ("double" "a2" "a2"))
(CONSTANT "b0" "b1" "b2" "a1" "a2")
(INNER-LOOP-LOCALS "            double z0;")
(INNER-LOOP "z0 = s + a1*z1 + a2*z2;
            output = (sample_type) (z0*b0 + z1*b1 + z2*b2);
            z2 = z1; z1 = z0")
)

