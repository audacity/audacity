/* fft.h -- fft returned through a lisp array */

LVAL snd_fft(sound_type s, long len, long step, LVAL w); 
  /* LISP: (SND-FFT SOUND FIXNUM FIXNUM ANY) */
