(INSTRFLUTE-ALG
(NAME "flute")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("rate_type" "sr"))
(STATE ("struct instr *" "myflute" "initInstrument(FLUTE, ROUND32(sr));
    controlChange(susp->myflute, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->myflute, freq, 1.0)")
       ("float" "breath_scale" 
        "breath_env->scale * FLUTE_CONTROL_CHANGE_CONST"))
(INLINE-INTERPOLATION no)
(INTERNAL-SCALING breath_env)
(START (min breath_env))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value")
(CONSTANT "breath_scale")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "controlChange(myflute, 128, breath_scale * breath_env);
            output = (sample_type) tick(myflute)")
(SUPPORT-HEADER "#define FLUTE_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "    deleteInstrument(susp->myflute);\n")
)

