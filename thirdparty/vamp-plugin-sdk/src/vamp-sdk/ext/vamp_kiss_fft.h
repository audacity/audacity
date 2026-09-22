#ifndef VAMP_KISS_FFT_H
#define VAMP_KISS_FFT_H

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#ifndef _VAMP_IN_PLUGINSDK
#  ifndef _VAMP_IN_HOSTSDK
#    error "Do not use the Vamp SDK mangled KissFFT header except through the Vamp SDK API"
#  endif
#endif

#ifdef VAMP_KISSFFT_USE_CPP_LINKAGE
#define VAMP_KISS_FFT_MALLOC ::malloc
#define VAMP_KISS_FFT_FREE ::free
#else
#ifdef __cplusplus
extern "C" {
#define VAMP_KISS_FFT_MALLOC malloc
#define VAMP_KISS_FFT_FREE free
#endif
#endif

# ifndef vamp_kiss_fft_scalar
#   error "vamp_kiss_fft_scalar must be defined before inclusion, we don't have a default in this build"
# endif

typedef struct {
    vamp_kiss_fft_scalar r;
    vamp_kiss_fft_scalar i;
} vamp_kiss_fft_cpx;

typedef struct vamp_kiss_fft_state* vamp_kiss_fft_cfg;

/* 
 *  kiss_fft_alloc
 *  
 *  Initialize a FFT (or IFFT) algorithm's cfg/state buffer.
 *
 *  typical usage:      kiss_fft_cfg mycfg=kiss_fft_alloc(1024,0,NULL,NULL);
 *
 *  The return value from fft_alloc is a cfg buffer used internally
 *  by the fft routine or NULL.
 *
 *  If lenmem is NULL, then kiss_fft_alloc will allocate a cfg buffer using malloc.
 *  The returned value should be free()d when done to avoid memory leaks.
 *  
 *  The state can be placed in a user supplied buffer 'mem':
 *  If lenmem is not NULL and mem is not NULL and *lenmem is large enough,
 *      then the function places the cfg in mem and the size used in *lenmem
 *      and returns mem.
 *  
 *  If lenmem is not NULL and ( mem is NULL or *lenmem is not large enough),
 *      then the function returns NULL and places the minimum cfg 
 *      buffer size in *lenmem.
 * */

vamp_kiss_fft_cfg vamp_kiss_fft_alloc(int nfft,int inverse_fft,void * mem,size_t * lenmem); 

/*
 * kiss_fft(cfg,in_out_buf)
 *
 * Perform an FFT on a complex input buffer.
 * for a forward FFT,
 * fin should be  f[0] , f[1] , ... ,f[nfft-1]
 * fout will be   F[0] , F[1] , ... ,F[nfft-1]
 * Note that each element is complex and can be accessed like
    f[k].r and f[k].i
 * */
void vamp_kiss_fft(vamp_kiss_fft_cfg cfg,const vamp_kiss_fft_cpx *fin,vamp_kiss_fft_cpx *fout);

/*
 A more generic version of the above function. It reads its input from every Nth sample.
 * */
void vamp_kiss_fft_stride(vamp_kiss_fft_cfg cfg,const vamp_kiss_fft_cpx *fin,vamp_kiss_fft_cpx *fout,int fin_stride);

/* If kiss_fft_alloc allocated a buffer, it is one contiguous 
   buffer and can be simply free()d when no longer needed*/
void vamp_kiss_fft_free(void *);

/*
 Cleans up some memory that gets managed internally. Not necessary to call, but it might clean up 
 your compiler output to call this before you exit.
*/
void vamp_kiss_fft_cleanup(void);
	

/*
 * Returns the smallest integer k, such that k>=n and k has only "fast" factors (2,3,5)
 */
int vamp_kiss_fft_next_fast_size(int n);

/* for real ffts, we need an even size */
#define vamp_kiss_fftr_next_fast_size_real(n) \
        (vamp_kiss_fft_next_fast_size( ((n)+1)>>1)<<1)

#ifndef VAMP_KISSFFT_USE_CPP_LINKAGE
#ifdef __cplusplus
} 
#endif
#endif

#ifdef VAMP_KISSFFT_USE_CPP_LINKAGE
#define VAMP_KISSFFT_USED_CPP_LINKAGE 1
#endif

#endif
