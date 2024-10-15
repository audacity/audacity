/* samples.h -- convert sound (prefix) to lisp array */

/* these are used by snd_fromobject and snd_fromarraystream: */
extern LVAL s_next;
extern LVAL s_send;

void samples_symbols(void);

sound_type snd_from_array(double t0, double sr, LVAL array);
  /* LISP: (SND-FROM-ARRAY ANYNUM ANYNUM ANY) */

LVAL snd_samples(sound_type s, int64_t len); /* LISP: (SND-SAMPLES SOUND FIXNUM) */
int64_t snd_length(sound_type s, int64_t len); /* LISP: (SND-LENGTH SOUND FIXNUM) */

double snd_maxsamp(sound_type s); /* LISP: (SND-MAXSAMP SOUND) */

LVAL snd_fetch(sound_type s); /* LISP: (SND-FETCH SOUND) */

LVAL snd_fetch_array(sound_type s, long len, long step); 
  /* LISP: (SND-FETCH-ARRAY SOUND LONG LONG) */
