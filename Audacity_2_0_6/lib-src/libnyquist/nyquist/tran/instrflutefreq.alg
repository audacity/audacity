(INSTRFLUTE-FREQ--ALG
(NAME "flute_freq")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env")
        	("sound_type" "freq_env") ("rate_type" "sr"))
(STATE ("struct instr *" "myflute" "initInstrument(FLUTE, round(sr));
    controlChange(susp->myflute, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->myflute, freq, 1.0)")
	("double" "frequency" "freq"))
(START (min breath_env))
(NOT-IN-INNER-LOOP  "temp_ret_value")
(CONSTANT "frequency")
(SAMPLE-RATE "sr")
(MATCHED-SAMPLE-RATE freq_env breath_env)
(TERMINATE (min breath_env))
(INNER-LOOP "
	    controlChange(myflute, 128, FLUTE_CONTROL_CHANGE_CONST * breath_env);
            setFrequency(myflute, frequency + freq_env);
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
