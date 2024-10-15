(SIOSC-ALG
(NAME "siosc")

; wave tables are provided in the argument "lis" as follows:
; (tab0 t1 tab1 t2 tab2 t3 tab3 ... tN tabN)
; where tab0 is the initial table, the table is linearly interpolated until
; sample t1, at which point the table is tab1.  From there, tab1 is interpolated
; to tab2 at t2, etc.  The sound stops at the terminate time of s_fm, so
; if that comes before tN, tabN is used for the remainder of the sound.
; t1, t2, ... tN are fixnums with sample counts

(ARGUMENTS ("LVAL" "lis") ("rate_type" "sr")
           ("double" "hz") ("time_type" "t0") ("sound_type" "s_fm"))
(SUPPORT-FUNCTIONS "
char *siosc_bad_table_list = \"bad table list in SIOSC\";

/* sisosc_table_init -- set up first two tables for interpolation */
/**/
void siosc_table_init(siosc_susp_type susp)
{
    sound_type snd;
    if (!susp->lis || !consp(susp->lis) || !soundp(car(susp->lis)))
        xlfail(siosc_bad_table_list);
    snd = getsound(car(susp->lis));
    susp->table_b_ptr_ptr = sound_to_table(snd);
    susp->table_b_samps = susp->table_b_ptr_ptr->samples;
    susp->lis = cdr(susp->lis);
    susp->table_sr = snd->sr;
    susp->table_len = susp->table_b_ptr_ptr->length;
}

/* siosc_table_update -- outer loop processing, get next table */
/**/
int64_t siosc_table_update(siosc_susp_type susp, int64_t cur)
{
    int64_t n;

    /* swap ampramps: */
    susp->ampramp_a = 1.0;
    susp->ampramp_b = 0.0;

    /* swap tables: */
    table_unref(susp->table_a_ptr);
    susp->table_a_ptr = susp->table_b_ptr_ptr;
    susp->table_a_samps = susp->table_b_samps;
    susp->table_b_ptr_ptr = NULL; /* so we do not try to unref it */

    if (susp->lis) {
        sound_type snd;

        /* compute slope */
        if (!consp(susp->lis) || !fixp(car(susp->lis)))
            xlfail(siosc_bad_table_list);
        susp->next_breakpoint = getfixnum(car(susp->lis));
        susp->lis = cdr(susp->lis);
        n = susp->next_breakpoint - cur;
        susp->ampslope = 1.0 / n;

        /* build new table: */
        if (!susp->lis || !consp(susp->lis) || !soundp(car(susp->lis)))
            xlfail(\"bad table list in SIOSC\");
        snd = getsound(car(susp->lis));
        susp->table_b_ptr_ptr = sound_to_table(snd);
        susp->table_b_samps = susp->table_b_ptr_ptr->samples;
        if (susp->table_b_ptr_ptr->length != susp->table_len || susp->table_sr != snd->sr)
            xlfail(\"mismatched tables passed to SIOSC\") ;
        susp->lis = cdr(susp->lis);
    } else { /* use only table a */
        susp->ampslope = 0.0;
        susp->next_breakpoint = n = 0x7FFFFFFFFFFFFFFF;
    }
    return n;
}
")

(STATE ("double" "table_len" "0.0")
       ("double" "ph_incr" "0.0")
       ("table_type" "table_a_ptr" "NULL")
       ("table_type" "table_b_ptr_ptr" "NULL")
       ("sample_type *" "table_a_samps" "NULL")
       ("sample_type *" "table_b_samps" "NULL")
       ("double" "table_sr" "0.0")
       ("double" "phase" "0.0")
       ("LVAL" "lis" "lis")
       ("int64_t" "next_breakpoint" "0")
       ("double" "ampramp_a" "1.0")
       ("double" "ampramp_b" "0.0")
       ("double" "ampslope" "0.0;
    siosc_table_init(susp);
    susp->ph_incr = hz * susp->table_len / sr;
    s_fm->scale = (sample_type) (s_fm->scale * (susp->table_len / sr))") 
)

(ALWAYS-SCALE s_fm)
(STEP-FUNCTION s_fm)
(TERMINATE (MIN s_fm))
(LOGICAL-STOP (MIN s_fm))
(INNER-LOOP-LOCALS "            long table_index;
            double xa, xb;
")

; Implementation notes:
;  "lis" points to the next time to be used, or NULL

(OUTER-LOOP "
        { int64_t cur = susp->susp.current + cnt;
          int64_t n64 = susp->next_breakpoint - cur;
          if (n64 == 0) n64 = siosc_table_update(susp, cur);
          if (n64 < togo) togo = (int) n64;
        }
")

(INNER-LOOP "table_index = (long) phase;
            xa = table_a_samps[table_index];
            xb = table_b_samps[table_index];
            output = (sample_type) 
                      (ampramp_a * (xa + (phase - table_index) * 
                          (table_a_samps[table_index + 1] - xa)) +
                       ampramp_b * (xb + (phase - table_index) * 
                          (table_b_samps[table_index + 1] - xb)));
            ampramp_a -= ampslope;
            ampramp_b += ampslope;
            phase += ph_incr + s_fm;
            while (phase > table_len) phase -= table_len;
            /* watch out for negative frequencies! */
            while (phase < 0) phase += table_len")
(CONSTANT "ph_incr" "table_len" "table_ptr" "table_a_samps"
          "table_b_samps" "ampslope" "table_a_ptr" "table_b_ptr_ptr")
(SAMPLE-RATE "sr")
(FINALIZATION "    table_unref(susp->table_a_ptr);
    table_unref(susp->table_b_ptr_ptr);\n")
)

