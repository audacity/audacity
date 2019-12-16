(INSTRFLUTEALL-ALG

;; parameters are:
;;   freq_env -- frequency modulation, aftertouch 128
;;   breath_env -- amplitude envelope, aftertouch 128
;;   vibrato_freq -- vibrato frequency, ModFreq 11
;;   vibrato_gain -- vibrato gain, ModWheel 1
;;   jet_delay -- jet delay, 2
;;   noise -- noise, Noise 4
;;
(NAME "flute_all")
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env")
    ("sound_type" "freq_env")
    ("double" "vibrato_freq") ("double" "vibrato_gain")
    ("sound_type" "jet_delay") ("sound_type" "noise_env") ("rate_type" "sr"))
;; use a constant rate of 1.0 because it will actually be conrolled
;; by breath_env
(STATE ("struct instr *" "myflute" "initInstrument(FLUTE, ROUND32(sr));
    noteOn(susp->myflute, freq, 1.0);
    controlChange(susp->myflute, 11, FLUTE_CONTROL_CHANGE_CONST * vibrato_freq);
    controlChange(susp->myflute, 1, FLUTE_CONTROL_CHANGE_CONST * vibrato_gain);")
       ("double" "frequency" "freq")
       ("float" "breath_scale" "breath_env->scale * FLUTE_CONTROL_CHANGE_CONST")
       ("float" "jet_scale" "jet_delay->scale * FLUTE_CONTROL_CHANGE_CONST")
       ("float" "noise_scale" "noise_env->scale * FLUTE_CONTROL_CHANGE_CONST"))
(START (min breath_env))
; matched doesn't help much because controlChange() is called in inner loop
;(MATCHED-SAMPLE-RATE freq_env breath_env jet_delay noise_env)
(ALWAYS-SCALE freq_env)
(INTERNAL-SCALING breath_env jet_delay noise_env)
(INLINE-INTERPOLATION no)
(CONSTANT "frequency" "breath_scale" "jet_scale" "noise_scale")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "controlChange(myflute, 128, breath_scale * breath_env);
            controlChange(myflute, 2, jet_scale * jet_delay);
            controlChange(myflute, 4, noise_scale * noise_env);
            setFrequency(myflute, frequency + freq_env);
            output = (sample_type) tick(myflute)")
(SUPPORT-HEADER "#define FLUTE_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n#include \"upsample.h\"\n")
(FINALIZATION "    deleteInstrument(susp->myflute);\n")
)
