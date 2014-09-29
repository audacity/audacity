(INSTRSAX-FREQ-ALG
(NAME "sax_freq")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("sound_type" "freq_env") ("rate_type" "sr"))
(STATE ("struct instr *" "sax" "initInstrument(SAXOFONY, round(sr));
    controlChange(susp->sax, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->sax, freq, 1.0)")
       ("double" "frequency" "freq"))
(START (min breath_env))
(NOT-IN-INNER-LOOP "temp_ret_value")
(CONSTANT "frequency")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "
	    controlChange(sax, 128, SAX_CONTROL_CHANGE_CONST * breath_env);
	    setFrequency(sax, frequency + freq_env);
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

