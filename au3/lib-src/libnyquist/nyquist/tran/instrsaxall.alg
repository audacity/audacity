(INSTRSAX-COMPLETE-ALG
(NAME "sax_all")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("sound_type" "freq_env")
        ("double" "vibrato_freq") ("double" "vibrato_gain")
	("sound_type" "reed_stiffness") ("sound_type" "noise_env") 
        ("sound_type" "blow_pos") ("sound_type" "reed_table_offset")
        ("rate_type" "sr"))
(STATE ("struct instr *" "sax" "initInstrument(SAXOFONY, ROUND32(sr));
    noteOn(susp->sax, freq, 1.0);
    controlChange(susp->sax, 29, SAX_CONTROL_CHANGE_CONST * vibrato_freq);
    controlChange(susp->sax, 1, SAX_CONTROL_CHANGE_CONST * vibrato_gain);")
       ("double" "frequency" "freq")
       ("float" "breath_scale" "breath_env->scale * SAX_CONTROL_CHANGE_CONST")
       ("float" "reed_scale" "reed_stiffness->scale * SAX_CONTROL_CHANGE_CONST")
       ("float" "noise_scale" "noise_env->scale * SAX_CONTROL_CHANGE_CONST")
       ("float" "blow_scale" "blow_pos->scale * SAX_CONTROL_CHANGE_CONST")
       ("float" "offset_scale" 
        "reed_table_offset->scale * SAX_CONTROL_CHANGE_CONST"))

(START (min breath_env))
; matched doesn't help much because controlChange() is called in inner loop
;(MATCHED-SAMPLE-RATE freq_env breath_env 
;                     reed_stiffness noise_env blow_pos reed_table_offset)
(CONSTANT "frequency" "breath_scale" "reed_scale" "noise_scale" 
          "blow_scale" "offset_scale")
(ALWAYS-SCALE freq_env)
(INTERNAL-SCALING breath_env reed_stiffness noise_env 
                  blow_pos reed_table_offset)
(INLINE-INTERPOLATION no)
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "controlChange(sax, 128, breath_scale * breath_env);
            controlChange(sax, 2, reed_scale * reed_stiffness);
            controlChange(sax, 4, noise_scale * noise_env);
            controlChange(sax, 11, blow_scale * blow_pos);
            controlChange(sax, 26, offset_scale * reed_table_offset);
            setFrequency(sax, frequency + freq_env);
            output = (sample_type) tick(sax)")
(SUPPORT-HEADER "#define SAX_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "    deleteInstrument(susp->sax);\n")
)

