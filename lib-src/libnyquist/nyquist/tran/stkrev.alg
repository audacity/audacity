(STKREV-ALG
(NAME "stkrev")
(ARGUMENTS ("int" "rev_type")("sound_type" "s1")("time_type" "trev")("double" "mix")("rate_type" "sr"))
(STATE 	("struct stkEffect *" "myrv"  "initStkEffect(rev_type, trev, round(sr));
stkEffectSetMix(susp->myrv, mix)"))
(START (MIN s1))
(TERMINATE (MIN s1))
(LOGICAL-STOP (MIN s1))
(NOT-IN-INNER-LOOP "myrv" "trev")
(SAMPLE-RATE "sr")
(SUPPORT-FUNCTIONS "
      #include \"stkint.h\"
")
(INNER-LOOP "
	output = (sample_type) (stkEffectTick(myrv, s1))
")
(FINALIZATION "
	deleteStkEffect(susp->myrv);
")
)