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
(ARGUMENTS ("double" "freq") ("sound_type" "breath_env") ("sound_type" "freq_env")
    ("double" "vibrato_freq") ("double" "vibrato_gain")
    ("sound_type" "jet_delay") ("sound_type" "noise") ("rate_type" "sr"))
;; use a constant rate of 1.0 because it will actually be conrolled
;; by breath_env
(STATE ("struct instr *" "myflute" "initInstrument(FLUTE, round(sr));
    noteOn(susp->myflute, freq, 1.0);
    controlChange(susp->myflute, 11, FLUTE_CONTROL_CHANGE_CONST * vibrato_freq);
    controlChange(susp->myflute, 1, FLUTE_CONTROL_CHANGE_CONST * vibrato_gain);")
       ("double" "frequency" "freq"))
(START (min breath_env))
(MATCHED-SAMPLE-RATE freq_env breath_env jet_delay noise)
(ALWAYS-SCALE freq_env breath_env jet_delay noise)
(CONSTANT "frequency")
(SAMPLE-RATE "sr")
(TERMINATE (min breath_env))
(INNER-LOOP "
	    controlChange(myflute, 128, FLUTE_CONTROL_CHANGE_CONST * breath_env);
	    controlChange(myflute, 2, FLUTE_CONTROL_CHANGE_CONST * jet_delay);
	    controlChange(myflute, 4, FLUTE_CONTROL_CHANGE_CONST * noise);
	    setFrequency(myflute, frequency + freq_env);
	    output = (sample_type) tick(myflute)")
(SUPPORT-HEADER "
	    #define FLUTE_CONTROL_CHANGE_CONST	128
")
(SUPPORT-FUNCTIONS "
	    #include \"instr.h\"
")
(FINALIZATION "
	    deleteInstrument(susp->myflute);
")
)
