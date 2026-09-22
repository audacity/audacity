#ifndef VAMP_KISS_FFTR_H
#define VAMP_KISS_FFTR_H

#include "vamp_kiss_fft.h"

#ifndef VAMP_KISSFFT_USE_CPP_LINKAGE
#ifdef __cplusplus
extern "C" {
#endif
#endif

    
/* 
 
 Real optimized version can save about 45% cpu time vs. complex fft of a real seq.

 
 
 */

typedef struct vamp_kiss_fftr_state *vamp_kiss_fftr_cfg;


vamp_kiss_fftr_cfg vamp_kiss_fftr_alloc(int nfft,int inverse_fft,void * mem, size_t * lenmem);
/*
 nfft must be even

 If you don't care to allocate space, use mem = lenmem = NULL 
*/


void vamp_kiss_fftr(vamp_kiss_fftr_cfg cfg,const vamp_kiss_fft_scalar *timedata,vamp_kiss_fft_cpx *freqdata);
/*
 input timedata has nfft scalar points
 output freqdata has nfft/2+1 complex points
*/

void vamp_kiss_fftri(vamp_kiss_fftr_cfg cfg,const vamp_kiss_fft_cpx *freqdata,vamp_kiss_fft_scalar *timedata);
/*
 input freqdata has  nfft/2+1 complex points
 output timedata has nfft scalar points
*/

void vamp_kiss_fftr_free(void *);

#ifndef VAMP_KISSFFT_USE_CPP_LINKAGE
#ifdef __cplusplus
}
#endif
#endif

#ifdef VAMP_KISSFFT_USE_CPP_LINKAGE
#define VAMP_KISSFFT_USED_CPP_LINKAGE 1
#endif

#endif
