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
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("sound_type" "freq_env")
    ("double" "vibrato_freq") ("double" "vibrato_gain")
    ("sound_type" "reed_stiffness") ("sound_type" "noise") ("rate_type" "sr"))
;; use a constant rate of 1.0 because it will actually be conrolled
;; by breath_env
(STATE ("struct instr *" "clar" "initInstrument(CLARINET, round(sr));
    noteOn(susp->clar, freq, 1.0);
    controlChange(susp->clar, 11, CLAR_CONTROL_CHANGE_CONST * vibrato_freq);
    controlChange(susp->clar, 1, CLAR_CONTROL_CHANGE_CONST * vibrato_gain);")
       ("double" "frequency" "freq"))
(START (min breath_env))
(MATCHED-SAMPLE-RATE freq_env breath_env reed_stiffness noise)
(ALWAYS-SCALE freq_env breath_env reed_stiffness noise)
(CONSTANT "frequency")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "
	    controlChange(clar, 128, CLAR_CONTROL_CHANGE_CONST * breath_env);
	    controlChange(clar, 2, CLAR_CONTROL_CHANGE_CONST * reed_stiffness);
	    controlChange(clar, 4, CLAR_CONTROL_CHANGE_CONST * noise);
	    setFrequency(clar, frequency + freq_env);
	    output = (sample_type) tick(clar)")
(SUPPORT-HEADER "
	    #define CLAR_CONTROL_CHANGE_CONST	128
")
(SUPPORT-FUNCTIONS "
	    #include \"instr.h\"
")
(FINALIZATION "
	    deleteInstrument(susp->clar);
")
)

