(INSTRCLAR-FREQ-ALG
(NAME "clarinet_freq")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("sound_type" "freq_env") 
           ("rate_type" "sr"))
(STATE ("struct instr *" "clar" "initInstrument(CLARINET, round(sr));
    controlChange(susp->clar, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->clar, freq, 1.0)")
       ("double" "frequency" "freq"))
(START (min breath_env))
(NOT-IN-INNER-LOOP "temp_ret_value")
(CONSTANT "frequency")
(SAMPLE-RATE "sr")
(MATCHED-SAMPLE-RATE freq_env breath_env)
(TERMINATE (min breath_env))
(INNER-LOOP "
	    controlChange(clar, 128, CLAR_CONTROL_CHANGE_CONST * breath_env);
	    setFrequency(clar, frequency + freq_env);
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

