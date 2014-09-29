/* sndwrite.h -- header to write sounds to files */

double sound_save(LVAL snd_expr, long n,
          unsigned char *filename, long format,
          long mode, long bits, long swap, double *sr, long *nchans,
          double *duration, LVAL play);
/* LISP: (SND-SAVE ANY FIXNUM STRING FIXNUM FIXNUM FIXNUM FIXNUM ANYNUM^ FIXNUM^ ANYNUM^ ANY) */

double sound_overwrite(LVAL snd_expr, long n,
               unsigned char *filename, double offset_secs, long format,
               long mode, long bits, long swap, double *duration);
/* LISP: (SND-OVERWRITE ANY FIXNUM STRING ANYNUM FIXNUM FIXNUM FIXNUM FIXNUM ANYNUM^) */
