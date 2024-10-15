(SHAPE-ALG
        (NAME "shape") 
        (ARGUMENTS ("sound_type" "sin") ("sound_type" "fn") 
                   ("double" "origin"))
        (START (MIN sin))       
        (TABLE "fn")
        (NOT-IN-INNER-LOOP "fn")
        (STATE 
               ("double" "time_to_index" "fn->sr")
               ("double" "origin" "origin")
               ("table_type" "the_table" "sound_to_table(fn)")
               ("sample_type *" "fcn_table" "susp->the_table->samples")         
               ("double" "table_len" "susp->the_table->length") )
        (TERMINATE (MIN sin))
        (LOGICAL-STOP (MIN sin))
        (INNER-LOOP "register double offset, x1;
            register long table_index;
            register double phase = sin;
            if (phase > 1.0) phase = 1.0;
            else if (phase < -1.0) phase = -1.0;
            offset = (phase + origin) * time_to_index;
            table_index = (long) offset;
            if (table_index < 0) { 
                table_index = 0; 
                offset = 0;
            }
            if (table_index >= table_len) {
                offset = table_len - 1;
                table_index = (long) offset;
            }
            x1 = fcn_table[table_index];
            output = (sample_type) (x1 + (offset - table_index) * 
                     (fcn_table[table_index + 1] - x1))")
        (ALWAYS-SCALE sin)
        (CONSTANT "table_len" "time_to_index" "origen" "fcn_table" "the_table")
        (FINALIZATION "table_unref(susp->the_table);\n")
)
