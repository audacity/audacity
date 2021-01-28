/* fft.h -- fft returned through a lisp array */

void fft_shift(float *x, int len);

LVAL snd_fft(sound_type s, long len, long step, LVAL w); 
  /* LISP: (SND-FFT SOUND LONG LONG ANY) */
