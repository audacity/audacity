(INSTRSAX-ALG
(NAME "sax")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("rate_type" "sr"))
(STATE ("struct instr *" "sax" "initInstrument(SAXOFONY, ROUND32(sr));
    controlChange(susp->sax, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->sax, freq, 1.0)")
       ("float" "breath_scale" "breath_env->scale * SAX_CONTROL_CHANGE_CONST"))
(INLINE-INTERPOLATION no)
(INTERNAL-SCALING breath_env)
(START (min breath_env))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "controlChange(sax, 128, breath_scale * breath_env);
            output = (sample_type) tick(sax)")
(SUPPORT-HEADER "#define SAX_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "    deleteInstrument(susp->sax);\n")
)

