/* fileio.h -- Nyquist code to read sound files */

/* for multiple channel files, one susp is shared by all sounds */
/* the susp in turn must point back to all sound list tails */

typedef struct read_susp_struct {
    snd_susp_node susp;
    SNDFILE *sndfile;
    SF_INFO sf_info;
    snd_list_type *chan; /* array of back pointers */
    int64_t cnt;	 /* how many sample frames to read */
} read_susp_node, *read_susp_type;


LVAL snd_make_read(unsigned char *filename, time_type offset, time_type t0,
        long *format, long *channels, long *mode, long *bits, long *swap,
        double *srate, double *dur, long *flags);
/* LISP: (SND-READ STRING ANYNUM ANYNUM LONG* LONG* LONG* LONG* LONG* ANYNUM* ANYNUM* LONG^) */

void read_free(snd_susp_type a_susp);
