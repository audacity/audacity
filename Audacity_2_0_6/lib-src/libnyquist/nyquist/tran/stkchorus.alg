(STKCHORUS-ALG
(NAME "stkchorus")
(ARGUMENTS ("sound_type" "s1")("double" "baseDelay")("double" "depth")("double" "freq")
	("double" "mix")("rate_type" "sr"))
(STATE 	("struct stkEffect *" "mych"  "initStkChorus(baseDelay, depth, freq,  round(sr));
stkEffectSetMix(susp->mych, mix)"))
(START (MIN s1))
(TERMINATE (MIN s1))
(LOGICAL-STOP (MIN s1))
(NOT-IN-INNER-LOOP "mych" "baseDelay" "depth" "freq" "mix" "sr")
(SAMPLE-RATE "sr")
(SUPPORT-FUNCTIONS "
      #include \"stkint.h\"
")
(INNER-LOOP "
	output = (sample_type) (stkEffectTick(mych, s1))
")
(FINALIZATION "
	deleteStkEffect(susp->mych);
")
)