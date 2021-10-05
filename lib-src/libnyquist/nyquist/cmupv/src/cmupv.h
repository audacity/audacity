//
//  cmupv.h
//  

#ifndef __cmupv_h__
#define __cmupv_h__

#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

// Protocol: The following is the order of calls with 
//           optional calls [like_this], and {iteration in braces}
//   pv_create
//   optional: pv_set_blocksize
//   optional: pv_set_fftsize
//   pv_initialize
//   repeat: { 
//     optional: [pv_get_effective_pos]
//     optional: [pv_set_ratio]
//     pv_get_input_count
//     pv_put_input (size must match result of pv_get_input_count)
//     pv_get_output }
//   pv_end
//
// Alternatively, you can use the "absolute" interface, where the
// Phase_vocoder object tells you when it needs a frame of audio
// and what output time corresponds to the center of the frame.
// The "client" then returns the frame of samples and tells the
// Phase_vocoder what hopsize was used. Of course, these callbacks
// will demand samples for "future" output, so the client must know
// how to find input samples for near-future points in the output:
//   pv_create2
//   optional: pv_set_blocksize
//   optional: pv_set_fftsize
//   pv_initialize
//   optional: pv_set_callback
//   repeat: { 
//     pv_get_output2 } ... during these calls, callback will get samples


typedef void *Phase_vocoder; // a phase vocoder
// the callback is given a sample count which is the output sample number
// that will be at the center of the window. The callee should fill
// samples with len samples from the input such that the center of the
// len samples should be output at the given output sample count.
// the hopsize should be returned.
// Return value is the hopsize used (hop from previous input frame)
typedef int (*Pv_callback)(int64_t out_count, float *samples, int len, 
                           void *rock);

// create a phase vocoder. Pass in function pointers to allocate
// and deallocate memory, or NULL to use default malloc()
// and free():
Phase_vocoder pv_create(void *(*mallocfn)(size_t), void (*freefn)(void *));

// Use pv_create2() to use the "absolute" interface and pv_set_callback().
// As with pv_create, pass in custom malloc and free functions or NULL to
// use default malloc() and free(). The callback is called to get input
// frames, and the "rock" context parameter will be passed to the callback:
Phase_vocoder pv_create2(void *(*mallocfn)(size_t), void (*freefn)(void *),
                         Pv_callback callback, void *rock);

void pv_set_callback(Phase_vocoder x, Pv_callback callback, void *rock);

// call at the end of the program to free the memory
void pv_end(Phase_vocoder *x);

// set output block size (call before computing first samples)
// must be a power of 2
// Blocksize should be set beforehand and can't be changed
// when program begins
void pv_set_blocksize(Phase_vocoder x, int n);

// set FFT size, n must be power of 2 (call before computing
// first samples)
// FFT size should be set beforehand and can't be changed
// when program begins
void pv_set_fftsize(Phase_vocoder x, int n);

int round_log(int fftsize); //calculate the round num of log2_fft

// set ratio (call before pv_get_input_count())
// ratio is the stretch factor: >1 means output sound will be
// longer than input sound
void pv_set_ratio(Phase_vocoder x, float ratio);

// set synthesis hopsize, 'n' must be power of 2.
// It's better to restrict syn_hopsize can only be
// fftsize/2, fftsize/4, fftsize/8, fftsize/16.
// Synthesis hopsize should be set beforehand and can't be
// changed when program begins
void pv_set_syn_hopsize(Phase_vocoder x, int n);

// initialize: allocate space for window, set default window,
// allocate space for other parameters
// Note that the user won't have the default window and don't
// need to free it.
void pv_initialize(Phase_vocoder x);

// set analysis window (if it's not called, default is Hanning window)
// A copy of the window is made and managed by the Phase_vocoder, this
// copy will be freed by the phase vocoder( by calling pv_end() )
// this function can be called many times, but only the newest window set
// by the user is valid.
void pv_set_ana_window(Phase_vocoder x, float *window);

// set synthesis window, same requirement as "pv_set_ana_window"
void pv_set_syn_window(Phase_vocoder x, float *window);

#define PV_MODE_STANDARD 0
#define PV_MODE_PHASEFIX 1
#define PV_MODE_ROBOVOICE 2

// set mode: to allow for variations in the algorithm
//   mode values are: PV_MODE_STANDARD -- standard phase vocoder
//   PV_MODE_PHASEFIX -- preserves the phase relationship between bins
//      and their nearest peak. This is an experimental mode that may
//      be refined in the future. It is inspired by Jean Laroche and
//      Mark Dolson, "Phase-Vocoder: About this phasiness business"
//   PV_MODE_ROBOVOICE, fixes the phases to create a deliberate
//      modulation, generating a pitch due to the hopsize period.
//      Thanks to M.C.Sharma for this idea.
//   illegal mode values are ignored
void pv_set_mode(Phase_vocoder x, int mode);

// allocate a window and initialize it with the window_type function
// (see hann() and hamm() as example window_type functions)
// caller becomes the owner of the result, so the owner should free
// the window eventually.
// Note the output is after normalized to make sure the sum of w1*w2
// equals to 1 rather than other constants.
float *pv_window(Phase_vocoder x, float (*window_type)(double x));

// inquire how many samples needed to compute next output
int pv_get_input_count(Phase_vocoder x);

// get effective position of the next output sample  measured in 
// samples of input. This may depend on the ratio (pv_set_ratio()).
// Initially, the center of the analysis window will be input 0,
// which will appear at output fftsize/2. There are ratio input
// samples per output sample, so the input sample number corresponding
// to output 0 is -ratio * fftsize / 2.
double pv_get_effective_pos(Phase_vocoder x);

// send input samples - writes input to be stretched into
// the phase vocoder. Phase vocoder saves as much of the
// signal as needed so that input is always sequential.
// note the "size" should be the output of pv_get_input_count().
void pv_put_input(Phase_vocoder x, int size, float *samples);

// get output samples: returns a pointer to n samples, where
// n is the value from pv_set_block_size(). Pointer should
// not be freed by the user.
float *pv_get_output(Phase_vocoder x);
// use pv_get_output2() if you used pv_create2():
float *pv_get_output2(Phase_vocoder x);

#ifdef __cplusplus
}
#endif

#endif /* defined(__cmupv_h__) */
