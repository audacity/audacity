(INSTRSAX-FREQ-ALG
(NAME "sax_freq")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") 
           ("sound_type" "freq_env") ("rate_type" "sr"))
(STATE ("struct instr *" "sax" "initInstrument(SAXOFONY, ROUND32(sr));
    controlChange(susp->sax, 1, 0.0);")
       ("int" "temp_ret_value" "noteOn(susp->sax, freq, 1.0)")
       ("float" "breath_scale" "breath_env->scale * SAX_CONTROL_CHANGE_CONST")
       ("double" "frequency" "freq"))
; since sax is a physical model calculated one sample at a time, there is
; relatively little advantage to inner loop optimization, so always do 
; scaling in the inner loop and do not do interpolation inline. This should
; generate just one implementation of the fetch function, saving code space.
(INLINE-INTERPOLATION no) 
(INTERNAL-SCALING breath_env)
(ALWAYS-SCALE freq_env)
(START (min breath_env))
(NOT-IN-INNER-LOOP "temp_ret_value")
(CONSTANT "frequency" "breath_scale")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "controlChange(sax, 128, breath_scale * breath_env);
            setFrequency(sax, frequency + freq_env);
            output = (sample_type) tick(sax)")
(SUPPORT-HEADER "#define SAX_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "    deleteInstrument(susp->sax);\n")
)

