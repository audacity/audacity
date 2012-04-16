#include "fft.h"

namespace _sbsms_ {

void fft128(t_fft *x) 
{
  fft<128,1>(x);
}

void ifft128(t_fft *x) 
{
  fft<128,-1>(x);
}

void fft256(t_fft *x) 
{
  fft<256,1>(x);
}

void ifft256(t_fft *x) 
{
  fft<256,-1>(x);
}

void fft384(t_fft *x) 
{
  fft<384,1>(x);
}

void fft512(t_fft *x) 
{
  fft<512,1>(x);
}

}
