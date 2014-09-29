(INSTRBOW-ALG
(NAME "bowed")
(ARGUMENTS ("double" "freq") ("sound_type" "bowpress_env") ("rate_type" "sr"))
(STATE ("struct instr *" "mybow" "initInstrument(BOWED, round(sr));
    controlChange(susp->mybow, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->mybow, freq, 1.0)"))
(START (min bowpress_env))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value")
(SAMPLE-RATE "sr")
(ALWAYS-SCALE bowpress_env)
(TERMINATE (min bowpress_env))
(INNER-LOOP "
	    controlChange(mybow, 128, BOW_CONTROL_CHANGE_CONST * bowpress_env);
	    output = (sample_type) tick(mybow)")
(SUPPORT-HEADER "
	    #define BOW_CONTROL_CHANGE_CONST	128
")
(SUPPORT-FUNCTIONS "
	    #include \"instr.h\"
")
(FINALIZATION "
	    deleteInstrument(susp->mybow);
")
)

