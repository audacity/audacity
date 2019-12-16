(INSTRCLAR-FREQ-ALG
(NAME "clarinet_freq")
(ARGUMENTS ("double" "freq1") ("sound_type" "breath_env") 
           ("sound_type" "freq_env") ("rate_type" "sr"))
(STATE ("struct instr *" "clar" "initInstrument(CLARINET, ROUND32(sr));
    controlChange(susp->clar, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->clar, freq1, 1.0)")
       ("double" "frequency" "freq1")
       ("float" "breath_scale" "breath_env->scale * CLAR_CONTROL_CHANGE_CONST"))
(INLINE-INTERPOLATION no)
(INTERNAL-SCALING breath_env)
(ALWAYS-SCALE freq_env)
(START (min breath_env))
(NOT-IN-INNER-LOOP "temp_ret_value")
(CONSTANT "frequency" "breath_scale")
(SAMPLE-RATE "sr")
;(MATCHED-SAMPLE-RATE freq_env breath_env)
(TERMINATE (min breath_env))
(INNER-LOOP "controlChange(clar, 128, breath_scale * breath_env);
            setFrequency(clar, frequency + freq_env);
            output = (sample_type) tick(clar)")
(SUPPORT-HEADER "#define CLAR_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "    deleteInstrument(susp->clar);\n")
)

