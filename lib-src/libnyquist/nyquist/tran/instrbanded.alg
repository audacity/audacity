(INSTRBANDED-ALG
(NAME "bandedwg")
(ARGUMENTS ("double" "freq") ("sound_type" "bowpress_env") ("int" "preset")
           ("rate_type" "sr"))
(STATE ("struct instr *" "mybanded" "initInstrument(BANDEDWG, ROUND32(sr));
    controlChange(susp->mybanded, 16, preset);")
       ("int" "temp_ret_value" "noteOn(susp->mybanded, freq, 1.0)")
       ("float" "bowpress_scale" 
        "bowpress_env->scale * BANDEDWG_CONTROL_CHANGE_CONST"))
(INLINE-INTERPOLATION no)
(INTERNAL-SCALING bowpress_env)
(START (min bowpress_env))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value" "preset")
(CONSTANT "bowpress_scale")
(SAMPLE-RATE "sr")
(TERMINATE (min bowpress_env))
(INNER-LOOP "controlChange(mybanded, 2, bowpress_scale * bowpress_env);
            output = (sample_type) tick(mybanded)")
(SUPPORT-HEADER "#define BANDEDWG_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "	   deleteInstrument(susp->mybanded);\n")
)
