
// Override C linkage for KissFFT headers. So long as we have already
// included all of the other (system etc) headers KissFFT depends on,
// this should work out OK
#define VAMP_KISSFFT_USE_CPP_LINKAGE 1

namespace Kiss {

#ifdef SINGLE_PRECISION_FFT
#pragma message("Using single-precision FFTs")
typedef float vamp_kiss_fft_scalar;
#define vamp_kiss_fft_scalar float
#else
typedef double vamp_kiss_fft_scalar;
#define vamp_kiss_fft_scalar double
#endif

#include "ext/vamp_kiss_fft.c"
#include "ext/vamp_kiss_fftr.c"

#undef vamp_kiss_fft_scalar // leaving only the namespaced typedef

}

// Check that this worked, i.e. that we have our own suitably
// hacked KissFFT header which set this after making the
// appropriate change
#ifndef VAMP_KISSFFT_USED_CPP_LINKAGE
#error "KissFFT header lacks specific linkage adjustment needed for Vamp SDK"
#endif
