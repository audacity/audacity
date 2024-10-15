(STKCHORUS-ALG
(NAME "stkchorus")
(ARGUMENTS ("sound_type" "s1") ("double" "baseDelay") ("double" "depth") 
           ("double" "freq") ("double" "mix"))
(STATE ("struct stkEffect *" "mych" 
        "initStkChorus(baseDelay, depth, freq,  ROUND32(sr));
stkEffectSetMix(susp->mych, mix)"))
(LINEAR s1)
(START (MIN s1))
(TERMINATE (MIN s1))
(LOGICAL-STOP (MIN s1))
(NOT-IN-INNER-LOOP "mych" "baseDelay" "depth" "freq" "mix")
(SAMPLE-RATE (MAX s1))
(SUPPORT-FUNCTIONS "#include \"stkint.h\"\n")
(INNER-LOOP "output = (sample_type) (stkEffectTick(mych, s1))")
(FINALIZATION "    deleteStkEffect(susp->mych);\n")
)
