sound_type snd_make_gate(sound_type signal, time_type lookahead, double risetime, double falltime, double floor, double threshold);
sound_type snd_gate(sound_type signal, time_type lookahead, double risetime, double falltime, double floor, double threshold);
    /* LISP: (snd-gate SOUND ANYNUM ANYNUM ANYNUM ANYNUM ANYNUM) */
