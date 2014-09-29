(INSTRSAX-COMPLETE-ALG
(NAME "sax_all")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("sound_type" "freq_env")
        ("double" "vibrato_freq") ("double" "vibrato_gain")
	("sound_type" "reed_stiffness") ("sound_type" "noise") 
        ("sound_type" "blow_pos") ("sound_type" "reed_table_offset")
        ("rate_type" "sr"))
(STATE ("struct instr *" "sax" "initInstrument(SAXOFONY, round(sr));
    noteOn(susp->sax, freq, 1.0);
    controlChange(susp->sax, 29, SAX_CONTROL_CHANGE_CONST * vibrato_freq);
    controlChange(susp->sax, 1, SAX_CONTROL_CHANGE_CONST * vibrato_gain);")
       ("double" "frequency" "freq"))
(START (min breath_env))
(MATCHED-SAMPLE-RATE freq_env breath_env 
                     reed_stiffness noise blow_pos reed_table_offset)
(CONSTANT "frequency")
(ALWAYS-SCALE freq_env breath_env reed_stiffness noise blow_pos reed_table_offset)
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "
	    controlChange(sax, 128, SAX_CONTROL_CHANGE_CONST * breath_env);
	    controlChange(sax, 2, SAX_CONTROL_CHANGE_CONST * reed_stiffness);
	    controlChange(sax, 4, SAX_CONTROL_CHANGE_CONST * noise);
	    controlChange(sax, 11, SAX_CONTROL_CHANGE_CONST * blow_pos);
	    controlChange(sax, 26, SAX_CONTROL_CHANGE_CONST * reed_table_offset);
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

