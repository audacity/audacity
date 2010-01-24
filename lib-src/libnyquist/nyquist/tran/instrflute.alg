(INSTRFLUTE-ALG
(NAME "flute")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("rate_type" "sr"))
(STATE ("struct instr *" "myflute" "initInstrument(FLUTE, round(sr));
    controlChange(susp->myflute, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->myflute, freq, 1.0)"))
(START (min breath_env))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value")
(SAMPLE-RATE "sr")
(ALWAYS-SCALE breath_env)
(TERMINATE (min breath_env))
(INNER-LOOP "
	    controlChange(myflute, 128, FLUTE_CONTROL_CHANGE_CONST * breath_env);
	    output = (sample_type) tick(myflute)")
(SUPPORT-HEADER "
	    #define FLUTE_CONTROL_CHANGE_CONST	128
")
(SUPPORT-FUNCTIONS "
	    #include \"instr.h\"
")
(FINALIZATION "
	    deleteInstrument(susp->myflute);
")
)

