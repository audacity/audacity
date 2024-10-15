/* sndsliders.h -- support for graphical sliders in Nyquist IDE */
/* Roger Dannenberg 
 * Jan 2013
 */

/* access from Nyquist unit generators to shared data */
sound_type snd_make_slider(int index, time_type t0, rate_type sr, time_type d);
sound_type snd_slider(int index, time_type t0, rate_type sr, time_type d);
    /* LISP: (SND-SLIDER LONG ANYNUM ANYNUM ANYNUM) */
