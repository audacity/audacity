/* sndwrite.h -- header to write sounds to files */

double sound_save(LVAL snd_expr, int64_t n,
                  unsigned char *filename, long format,
                  long mode, long bits, long swap, double *sr, long *nchans,
                  double *duration, LVAL play, int64_t progress);
/* LISP: (SND-SAVE ANY FIXNUM STRING LONG LONG LONG LONG ANYNUM^ LONG^ ANYNUM^ ANY FIXNUM) */

double sound_overwrite(LVAL snd_expr, int64_t n, unsigned char *filename,
                       double offset_secs, double *duration, int64_t progress);
/* LISP: (SND-OVERWRITE ANY FIXNUM STRING ANYNUM ANYNUM^ FIXNUM) */
