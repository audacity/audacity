(STKPITSHIFT-ALG
(NAME "stkpitshift")
(ARGUMENTS ("sound_type" "s1") ("double" "shift") ("double" "mix"))
(STATE 	("struct stkEffect *" "mych"  "initStkPitShift(shift, ROUND32(sr));
stkEffectSetMix(susp->mych, mix)"))
(ALWAYS-SCALE s1)
(START (MIN s1))
(TERMINATE (MIN s1))
(LOGICAL-STOP (MIN s1))
(NOT-IN-INNER-LOOP "mych" "shift" "mix" "sr")
(SAMPLE-RATE (MAX s1))
(SUPPORT-FUNCTIONS "
#include \"stkint.h\"
")
(INNER-LOOP "output = (sample_type) (stkEffectTick(mych, s1))")
(FINALIZATION "    deleteStkEffect(susp->mych);\n")
)
