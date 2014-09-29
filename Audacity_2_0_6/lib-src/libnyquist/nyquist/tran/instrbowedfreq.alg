(INSTRBOWED-FREQ-ALG
(NAME "bowed_freq")
(ARGUMENTS ("double" "freq") ("sound_type" "bowpress_env")
        	("sound_type" "freq_env") ("rate_type" "sr"))
(STATE ("struct instr *" "mybow" "initInstrument(BOWED, round(sr));
    controlChange(susp->mybow, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->mybow, freq, 1.0)")
	("double" "frequency" "freq"))
(START (min bowpress_env))
(NOT-IN-INNER-LOOP  "temp_ret_value")
(CONSTANT "frequency")
(SAMPLE-RATE "sr")
(MATCHED-SAMPLE-RATE freq_env bowpress_env)
(TERMINATE (min bowpress_env))
(INNER-LOOP "
	    controlChange(mybow, 128, BOW_CONTROL_CHANGE_CONST * bowpress_env);
            setFrequency(mybow, frequency + freq_env);
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
