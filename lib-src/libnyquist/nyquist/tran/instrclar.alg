(INSTRCLAR-ALG
(NAME "clarinet")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("rate_type" "sr"))
(STATE ("struct instr *" "clar" "initInstrument(CLARINET, round(sr));
    controlChange(susp->clar, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->clar, freq, 1.0)"))
(START (min breath_env))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value")
(SAMPLE-RATE "sr")
(ALWAYS-SCALE breath_env)
(TERMINATE (min breath_env))
(INNER-LOOP "
	    controlChange(clar, 128, CLAR_CONTROL_CHANGE_CONST * breath_env);
	    output = (sample_type) tick(clar)")
(SUPPORT-HEADER "
	    #define CLAR_CONTROL_CHANGE_CONST	128
")
(SUPPORT-FUNCTIONS "
	    #include \"instr.h\"
")
(FINALIZATION "
	    deleteInstrument(susp->clar);
")
)

