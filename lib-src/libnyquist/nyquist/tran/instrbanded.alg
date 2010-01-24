(INSTRBANDED-ALG
(NAME "bandedwg")
(ARGUMENTS ("double" "freq") ("sound_type" "bowpress_env") ("int" "preset")("rate_type" "sr"))
(STATE ("struct instr *" "mybanded" "initInstrument(BANDEDWG, round(sr));
    controlChange(susp->mybanded, 16, preset);")
       ("int" "temp_ret_value" "noteOn(susp->mybanded, freq, 1.0)"))
(START (min bowpress_env))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value" "preset")
(SAMPLE-RATE "sr")
(ALWAYS-SCALE bowpress_env)
(TERMINATE (min bowpress_env))
(INNER-LOOP "
	    controlChange(mybanded, 2, BANDEDWG_CONTROL_CHANGE_CONST * bowpress_env);
	    output = (sample_type) tick(mybanded)")
(SUPPORT-HEADER "
	    #define BANDEDWG_CONTROL_CHANGE_CONST	128
")
(SUPPORT-FUNCTIONS "
	    #include \"instr.h\"
")
(FINALIZATION "
	    deleteInstrument(susp->mybanded);
")
)
