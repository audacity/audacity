sound_type snd_make_avg(sound_type s, long blocksize, long stepsize, long op);
sound_type snd_avg(sound_type s, long blocksize, long stepsize, long op);
    /* LISP: (snd-avg SOUND LONG LONG LONG) */
#define op_average 1
#define op_peak 2
/* LISP-SRC: (setf OP-AVERAGE 1) (setf OP-PEAK 2) */
