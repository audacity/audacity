(INSTRCLAR-COMPLETE-ALG

;; parameters are:
;;   freq_env -- frequency modulation, aftertouch 128
;;   breath_env -- amplitude envelope, aftertouch 128
;;   vibrato_freq -- vibrato frequency, ModFreq 11
;;   vibrato_gain -- vibrato gain, ModWheel 1
;;   reed_stiffness -- reed stiffness, ReedStiff 2
;;   noise -- noise, Noise 4
;;
(NAME "clarinet_all")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env")
           ("sound_type" "freq_env")
           ("double" "vibrato_freq") ("double" "vibrato_gain")
           ("sound_type" "reed_stiffness") ("sound_type" "noise_env") 
           ("rate_type" "sr"))
;; use a constant rate of 1.0 because it will actually be conrolled
;; by breath_env
(STATE ("struct instr *" "clar" "initInstrument(CLARINET, ROUND32(sr));
    noteOn(susp->clar, freq, 1.0);
    controlChange(susp->clar, 11, CLAR_CONTROL_CHANGE_CONST * vibrato_freq);
    controlChange(susp->clar, 1, CLAR_CONTROL_CHANGE_CONST * vibrato_gain);")
       ("double" "frequency" "freq")
       ("float" "breath_scale" "breath_env->scale * CLAR_CONTROL_CHANGE_CONST")
       ("float" "reed_scale" "reed_stiffness->scale * CLAR_CONTROL_CHANGE_CONST")
       ("float" "noise_scale" "noise_env->scale * CLAR_CONTROL_CHANGE_CONST"))
(START (min breath_env))
; matched doesn't help much because controlChange() is called in inner loop
;(MATCHED-SAMPLE-RATE freq_env breath_env reed_stiffness noise_env)
(ALWAYS-SCALE freq_env)
(INTERNAL-SCALING breath_env reed_stiffness noise_env)
(INLINE-INTERPOLATION no)
(CONSTANT "frequency" "breath_scale" "reed_scale" "noise_scale")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "controlChange(clar, 128, breath_scale * breath_env);
            controlChange(clar, 2, reed_scale * reed_stiffness);
            controlChange(clar, 4, noise_scale * noise_env);
            setFrequency(clar, frequency + freq_env);
            output = (sample_type) tick(clar)")
(SUPPORT-HEADER "#define CLAR_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "	   deleteInstrument(susp->clar);\n")
)

