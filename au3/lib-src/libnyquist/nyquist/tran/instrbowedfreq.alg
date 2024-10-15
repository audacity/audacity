(INSTRBOWED-FREQ-ALG
(NAME "bowed_freq")
(ARGUMENTS ("double" "freq") ("sound_type" "bowpress_env")
           ("sound_type" "freq_env") ("rate_type" "sr"))
(STATE ("struct instr *" "mybow" "initInstrument(BOWED, ROUND32(sr));
    controlChange(susp->mybow, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->mybow, freq, 1.0)")
       ("float" "bow_scale" "bowpress_env->scale * BOW_CONTROL_CHANGE_CONST")
       ("double" "frequency" "freq"))
(INLINE-INTERPOLATION no)
(ALWAYS-SCALE freq_env)
(INTERNAL-SCALING bowpress_env)
(START (min bowpress_env))
(NOT-IN-INNER-LOOP  "temp_ret_value")
(CONSTANT "frequency" "bow_scale")
(SAMPLE-RATE "sr")
(TERMINATE (min bowpress_env))
(INNER-LOOP "controlChange(mybow, 128, bow_scale * bowpress_env);
            setFrequency(mybow, frequency + freq_env);
            output = (sample_type) tick(mybow)")
(SUPPORT-HEADER "#define BOW_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "    deleteInstrument(susp->mybow);\n")
)
