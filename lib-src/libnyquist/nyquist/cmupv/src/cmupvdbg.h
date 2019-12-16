/* cmupvdbg.h -- cmupv debug function declarations */

/* Normally, this function(s) are not called and the corresponding
 * cmupvdbg.c need not be linked
 */

/* See comments in cmupvdbg.c and definitions of MAX_FILES and MAX_SAVE */

/* write_pv_frame -- saves an array of floats as a sound file so you
 * can see what is going into the phase vocoder
 */
void write_pv_frame(long zeros, float *ana_frame, long fftsize, char *prefix);
