(INSTRSAX-ALG
(NAME "sax")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("rate_type" "sr"))
(STATE ("struct instr *" "sax" "initInstrument(SAXOFONY, round(sr));
    controlChange(susp->sax, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->sax, freq, 1.0)"))
(START (min breath_env))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "
	    controlChange(sax, 128, SAX_CONTROL_CHANGE_CONST * breath_env);
	    output = (sample_type) tick(sax)")
(SUPPORT-HEADER "
	    #define SAX_CONTROL_CHANGE_CONST	128
")
(SUPPORT-FUNCTIONS "
	    #include \"instr.h\"
")
(FINALIZATION "
	    deleteInstrument(susp->sax);
")
)

