(INSTRMODALBAR-ALG
(NAME "modalbar")
(ARGUMENTS ("time_type" "t0")("double" "freq") ("int" "preset")("time_type" "dur") ("rate_type" "sr"))
(STATE ("struct instr *" "mymbar" "initInstrument(MODALBAR, ROUND32(sr));
                  controlChange(susp->mymbar, 16, preset);")
       ("int" "temp_ret_value" "noteOn(susp->mymbar, freq, 1.0);"))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value")
(SAMPLE-RATE "sr")
(TERMINATE (after "dur"))
(INNER-LOOP "output = (sample_type) tick(mymbar)")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n")
(FINALIZATION "    deleteInstrument(susp->mymbar);\n")
)

