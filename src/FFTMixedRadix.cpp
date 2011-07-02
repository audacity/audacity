#include "FFTMixedRadix.h"

const int FFTMixedRadixN[nFFTMixedRadixN] = {8, 12, 16, 24, 32, 48, 64, 90, 128, 180, 256, 360, 512, 720, 1024, 1440, 2048, 2916, 4096, 5760, 8192, 11520, 16384, 23328, 32768 };

void fftr8(float *x) { fftr<8,1>(x); }
void fftr12(float *x) { fftr<12,1>(x); }
void fftr16(float *x) { fftr<16,1>(x); }
void fftr24(float *x) { fftr<24,1>(x); }
void fftr32(float *x) { fftr<32,1>(x); }
void fftr48(float *x) { fftr<48,1>(x); }
void fftr64(float *x) { fftr<64,1>(x); }
void fftr90(float *x) { fftr<90,1>(x); }
void fftr128(float *x) { fftr<128,1>(x); }
void fftr180(float *x) { fftr<180,1>(x); }
void fftr256(float *x) { fftr<256,1>(x); }
void fftr360(float *x) { fftr<360,1>(x); }
void fftr512(float *x) { fftr<512,1>(x); }
void fftr720(float *x) { fftr<720,1>(x); }
void fftr1024(float *x) { fftr<1024,1>(x); }
void fftr1440(float *x) { fftr<1440,1>(x); }
void fftr2048(float *x) { fftr<2048,1>(x); }
void fftr2916(float *x) { fftr<2916,1>(x); }
void fftr4096(float *x) { fftr<4096,1>(x); }
void fftr5760(float *x) { fftr<5760,1>(x); }
void fftr8192(float *x) { fftr<8192,1>(x); }
void fftr11520(float *x) { fftr<11520,1>(x); }
void fftr16384(float *x) { fftr<16384,1>(x); }
void fftr23328(float *x) { fftr<23328,1>(x); }
void fftr32768(float *x) { fftr<32768,1>(x); }

MixedRadixFFTFunc GetMixedRadixFFTFunc(int n)
{
   switch(n) {
   case 8: return &fftr8; break;
   case 12: return &fftr12; break;
   case 16: return &fftr16; break;
   case 24: return &fftr24; break;
   case 32: return &fftr32; break;
   case 48: return &fftr48; break;
   case 64: return &fftr64; break;
   case 90: return &fftr90; break;
   case 128: return &fftr128; break;
   case 180: return &fftr180; break;
   case 256: return &fftr256; break;
   case 360: return &fftr360; break;
   case 512: return &fftr512; break;
   case 720: return &fftr720; break;
   case 1024: return &fftr1024; break;
   case 1440: return &fftr1440; break;
   case 2048: return &fftr2048; break;
   case 2916: return &fftr2916; break;
   case 4096: return &fftr4096; break;
   case 5760: return &fftr5760; break;
   case 8192: return &fftr8192; break;
   case 11520: return &fftr11520; break;
   case 16384: return &fftr16384; break;
   case 23328: return &fftr23328; break;
   case 32768: return &fftr32768; break;
   }
}

int FFTMixedRadixGetIndex(int size)
{
   for(int k=0; k<nFFTMixedRadixN; k++) {
      if(FFTMixedRadixN[k] == size) {
         return k;
      }
   }
   return -1;
}

int FFTMixedRadixGetSize(int index)
{
   if(index >= 0 && index < nFFTMixedRadixN) {
      return FFTMixedRadixN[index];
   } else {
      return -1;
   }
}
