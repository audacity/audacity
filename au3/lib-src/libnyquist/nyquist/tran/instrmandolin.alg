(INSTRMANDOLIN-ALG
(NAME "mandolin")
(ARGUMENTS ("time_type" "t0") ("double" "freq") ("time_type" "d") 
           ("double" "body_size") ("double" "detune") ("rate_type" "sr"))
(STATE ("struct instr *" "mymand" "initInstrument(MANDOLIN, ROUND32(sr));
      controlChange(susp->mymand, 1, detune);
      controlChange(susp->mymand, 2, MAND_CONTROL_CHANGE_CONST * body_size);")
       ("int" "temp_ret_value" "noteOn(susp->mymand, freq, 1.0)"))
(NOT-IN-INNER-LOOP "freq" "temp_ret_value"  "body_size" "detune")
(SAMPLE-RATE "sr")
(TERMINATE (after "d"))
(INNER-LOOP "output = (sample_type) tick(mymand)")
(SUPPORT-HEADER "#define MAND_CONTROL_CHANGE_CONST 128\n")
(SUPPORT-FUNCTIONS "#include \"instr.h\"\n")
(FINALIZATION "    deleteInstrument(susp->mymand);\n")
)
