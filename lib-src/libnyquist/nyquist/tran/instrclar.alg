(INSTRCLAR-ALG
(NAME "clarinet")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("rate_type" "sr"))
(STATE ("struct instr *" "clar" "initInstrument(CLARINET, ROUND32(sr));
    controlChange(susp->clar, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->clar, freq, 1.0)")
       ("float" "breath_scale" "breath_env->scale * CLAR_CONTROL_CHANGE_CONST"))
(INLINE-INTERPOLATION no)
(INTERNAL-SCALING breath_env)
(START (min breath_env))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value")
(CONSTANT "breath_scale")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "controlChange(clar, 128, breath_scale * breath_env);
            output = (sample_type) tick(clar)")
(SUPPORT-HEADER "#define CLAR_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "    deleteInstrument(susp->clar);\n")
)

