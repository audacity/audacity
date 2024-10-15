// -*- mode: c++ -*-
#ifndef FFT_H
#define FFT_H

#include <math.h>
#include <string.h>
#include "utils.h"

#if defined(ENABLE_SSE) && !defined(APPLE_PPC) && !defined(_M_ARM64)
#include "sse.h"
#endif

namespace _sbsms_ {

typedef void (*fftplan)(t_fft *x);
void fft128(t_fft *x);
void ifft128(t_fft *x);
void fft256(t_fft *x);
void ifft256(t_fft *x);
void fft384(t_fft *x);
void fft512(t_fft *x);

template<int N>
class Factor {
public:
  enum { value = (N%8==0)?8:(N%7==0)?7:(N%4==0)?4:(N%5==0)?5:(N%6==0)?6:(N%3==0)?3:(N%2==0)?2:1 };
};

template<int N>
class LastFactor {
public:
  enum { radix = Factor<N>::value,
         stride = N / radix,
         value = (stride==1?radix:LastFactor<stride>::value) };
};

template <>
class LastFactor<1> {
public:
  enum { value = 1 };
};

template<int istride, int ostride, int radix, int dir>
class __fft {
public:
  static inline void execute(t_fft *x, t_fft *y, int step);
};

template <int N, int dir> 
class FloatTwiddles {
public:
  float c[N];
  float s[N];
  FloatTwiddles() {
    for(int k=0; k<N; k++) {
      c[k] = cos(TWOPI * (float)k / (float)N);
      s[k] = sin(TWOPI * (float)(-dir*k) / (float)N);
    }
  }
};

template <int N, int dir> 
class FloatTwiddle {
 public:
  static const float *c;
  static const float *s;
  static const FloatTwiddles<N,dir> t;
  static inline void twiddle(int k, t_fft *x, float r, float i) {
    float cc = c[k];
    float ss = s[k];
    (*x)[0] = cc * r - ss * i;
    (*x)[1] = ss * r + cc * i;
  }
};

template <int N, int dir>
const FloatTwiddles<N,dir> FloatTwiddle<N,dir>::t;

template <int N, int dir>
const float* FloatTwiddle<N,dir>::c = t.c;

template <int N, int dir>
const float* FloatTwiddle<N,dir>::s = t.s;

template<int istride, int ostride, int dir>
class __fft<istride,ostride,2,dir> {
public:
  enum { i1 = istride, o1 = ostride, 
         ir0 = 0, ii0 = 1, or0 = 0, oi0 = 1,
         ir1 = i1<<1, ii1 = ir1 + 1, or1 = o1<<1, oi1 = or1 + 1 };
  static inline void execute(t_fft *_x, t_fft *_y, int step) {
    float *x = (float*)_x;
    float *y = (float*)_y;
    float y0 = x[ir0] - x[ir1]; float y1 = x[ii0] - x[ii1];
    y[or0] = x[ir0] + x[ir1]; y[oi0] = x[ii0] + x[ii1];
    y[or1] = y0; y[oi1] = y1;
  }
};

template<int istride, int ostride, int dir>
class __fft<istride,ostride,3,dir> {
public:
  enum { N = istride * 3,
         i1_ = istride, o1 = ostride,
         i2_ = istride + istride, o2 = o1 + ostride,
         i1 = (dir==1?i1_:i2_),
         i2 = (dir==1?i2_:i1_),
         ir0 = 0, ii0 = 1, or0 = 0, oi0 = 1,
         ir1 = i1<<1, ii1 = ir1 + 1, or1 = o1<<1, oi1 = or1 + 1,
         ir2 = i2<<1, ii2 = ir2 + 1, or2 = o2<<1, oi2 = or2 + 1 };
  static inline void execute(t_fft *_x, t_fft *_y, int step) {
    float *x = (float*)_x;
    float *y = (float*)_y;
    float z00 = x[ir1] + x[ir2]; float z01 = x[ii1] + x[ii2];
    float z10 = x[ir0] - 0.5f*z00; float z11 = x[ii0] - 0.5f*z01;
    float z20 = 0.86602540378444f*(x[ir2] - x[ir1]); float z21 = 0.86602540378444f*(x[ii2] - x[ii1]);
    y[or0] = x[ir0] + z00; y[oi0] = x[ii0] + z01;
    if(step) {
      FloatTwiddle<N,dir>::twiddle(step, _y+o1,z10 - z21,z11 + z20);
      FloatTwiddle<N,dir>::twiddle(step+step, _y+o2,z10 + z21,z11 - z20);
    } else {
      y[or1] = z10 - z21; y[oi1] = z11 + z20;
      y[or2] = z10 + z21; y[oi2] = z11 - z20;
    }
  }
};

template<int istride, int ostride, int dir> 
  class __fft<istride,ostride,5,dir> {
public:
  enum { N = istride*5,
         i0 = 0, o0 = 0,
         i1_ = i0 + istride, o1 = o0 + ostride,
         i2_ = i1_ + istride, o2 = o1 + ostride,
         i3_ = i2_ + istride, o3 = o2 + ostride,
         i4_ = i3_ + istride, o4 = o3 + ostride,
         i1 = (dir==1?i4_:i1_),
         i2 = (dir==1?i3_:i2_),
         i3 = (dir==1?i2_:i3_),
         i4 = (dir==1?i1_:i4_),
         ir0 = i0<<1, ii0 = ir0 + 1, or0 = o0<<1, oi0 = or0 + 1,
         ir1 = i1<<1, ii1 = ir1 + 1, or1 = o1<<1, oi1 = or1 + 1,
         ir2 = i2<<1, ii2 = ir2 + 1, or2 = o2<<1, oi2 = or2 + 1,
         ir3 = i3<<1, ii3 = ir3 + 1, or3 = o3<<1, oi3 = or3 + 1,
         ir4 = i4<<1, ii4 = ir4 + 1, or4 = o4<<1, oi4 = or4 + 1 };
  static inline void execute(t_fft *_x, t_fft *_y, int step) {
    float *x = (float*)_x;
    float *y = (float*)_y;
    float z00, z01, z10, z11, z20, z21, z30, z31, z40, z41, z50, z51, z60, z61, z70, z71, z80, z81, z90, z91, z100, z101;
    z00 = x[ir1] + x[ir4]; z01 = x[ii1] + x[ii4];
    z10 = x[ir2] + x[ir3]; z11 = x[ii2] + x[ii3];
    z20 = x[ir1] - x[ir4]; z21 = x[ii1] - x[ii4];
    z30 = x[ir2] - x[ir3]; z31 = x[ii2] - x[ii3];
    z40 = z00 + z10; z41 = z01 + z11;
    z50 = 0.55901699437495f*(z00 - z10); z51 = 0.55901699437495f*(z01 - z11);
    z60 = x[ir0] - 0.25f*z40; z61 = x[ii0] - 0.25f*z41;
    z70 = z50 + z60; z71 = z51 + z61;
    z80 = z60 - z50; z81 = z61 - z51;
    z90 = 0.95105651629515f*z20 + 0.58778525229247f*z30; z91 = 0.95105651629515f*z21 + 0.58778525229247f*z31;
    z100 = 0.58778525229247f*z20 - 0.95105651629515f*z30; z101 = 0.58778525229247f*z21 - 0.95105651629515f*z31;
    y[or0] = x[ir0] + z40; y[oi0] = x[ii0] + z41;
    if(step) {
      int step2 = step + step;
      FloatTwiddle<N,dir>::twiddle(step,_y+o1,z70 - z91,z71 + z90);
      FloatTwiddle<N,dir>::twiddle(step2,_y+o2,z80 - z101,z81 + z100);
      FloatTwiddle<N,dir>::twiddle(step2+step,_y+o3,z80 + z101,z81 - z100);
      FloatTwiddle<N,dir>::twiddle(step2+step2,_y+o4,z70 + z91,z71 - z90);
    } else {
      y[or1] = z70 - z91; y[oi1] = z71 + z90;
      y[or2] = z80 - z101; y[oi2] = z81 + z100;
      y[or3] = z80 + z101; y[oi3] = z81 - z100;
      y[or4] = z70 + z91; y[oi4] = z71 - z90;
    }
  }
};

template<int istride, int ostride, int dir> 
class __fft<istride,ostride,6,dir> {
public:
  enum { N = istride*6,
         i0 = 0, o0 = 0,
         i1_ = i0 + istride, o1 = o0 + ostride,
         i2_ = i1_ + istride, o2 = o1 + ostride,
         i3 = i2_ + istride, o3 = o2 + ostride,
         i4_ = i3 + istride, o4 = o3 + ostride,
         i5_ = i4_ + istride, o5 = o4 + ostride,
         i1 = (dir==1?i1_:i5_),
         i2 = (dir==1?i2_:i4_),
         i4 = (dir==1?i4_:i2_),
         i5 = (dir==1?i5_:i1_),
         ir0 = i0<<1, ii0 = ir0 + 1, or0 = o0<<1, oi0 = or0 + 1,
         ir1 = i1<<1, ii1 = ir1 + 1, or1 = o1<<1, oi1 = or1 + 1,
         ir2 = i2<<1, ii2 = ir2 + 1, or2 = o2<<1, oi2 = or2 + 1,
         ir3 = i3<<1, ii3 = ir3 + 1, or3 = o3<<1, oi3 = or3 + 1,
         ir4 = i4<<1, ii4 = ir4 + 1, or4 = o4<<1, oi4 = or4 + 1,
         ir5 = i5<<1, ii5 = ir5 + 1, or5 = o5<<1, oi5 = or5 + 1 };
  static inline void execute(t_fft *_x, t_fft *_y, int step) {
    float *x = (float*)_x;
    float *y = (float*)_y;
    float za00, za01, za10, za11, za20, za21;
    float a00, a01, a10, a11, a20, a21;
    float zb00, zb01, zb10, zb11, zb20, zb21;
    float b00, b01, b10, b11, b20, b21;
    za00 = x[ir2] + x[ir4]; za01 = x[ii2] + x[ii4];
    za10 = x[ir0] - 0.5f*za00; za11 = x[ii0] - 0.5f*za01;
    za20 = 0.86602540378444f*(x[ir4] - x[ir2]); za21 = 0.86602540378444f*(x[ii4] - x[ii2]);
    a00 = x[ir0] + za00; a01 = x[ii0] + za01;
    a10 = za10 - za21; a11 = za11 + za20;
    a20 = za10 + za21; a21 = za11 - za20;
    zb00 = x[ir1] + x[ir5]; zb01 = x[ii1] + x[ii5];
    zb10 = x[ir3] - 0.5f*zb00; zb11 = x[ii3] - 0.5f*zb01;
    zb20 = 0.86602540378444f*(x[ir1] - x[ir5]); zb21 = 0.86602540378444f*(x[ii1] - x[ii5]);
    b00 = x[ir3] + zb00; b01 = x[ii3] + zb01;
    b10 = zb10 - zb21; b11 = zb11 + zb20;
    b20 = zb10 + zb21; b21 = zb11 - zb20;
    y[or0] = a00 + b00; y[oi0] = a01 + b01;
    if(step) {
      FloatTwiddle<N,dir>::twiddle(step,_y+o1,a10 - b10,a11 - b11);
      int step2 = step + step;
      FloatTwiddle<N,dir>::twiddle(step2,_y+o2,a20 + b20,a21 + b21);
      int step3 = step2 + step;
      FloatTwiddle<N,dir>::twiddle(step3,_y+o3,a00 - b00,a01 - b01);
      FloatTwiddle<N,dir>::twiddle(step2+step2,_y+o4,a10 + b10,a11 + b11);
      FloatTwiddle<N,dir>::twiddle(step3+step2,_y+o5,a20 - b20,a21 - b21);
    } else {
      y[or1] = a10 - b10; y[oi1] = a11 - b11;
      y[or2] = a20 + b20; y[oi2] = a21 + b21;
      y[or3] = a00 - b00; y[oi3] = a01 - b01;
      y[or4] = a10 + b10; y[oi4] = a11 + b11;
      y[or5] = a20 - b20; y[oi5] = a21 - b21;
    }
  }
};

template<int istride, int ostride, int dir> 
class __fft<istride,ostride,7,dir> {
public:
  enum { N = istride*7,
         i0 = 0, o0 = 0,
         i1 = i0 + istride, o1 = o0 + ostride,
         i2 = i1 + istride, o2 = o1 + ostride,
         i3 = i2 + istride, o3 = o2 + ostride,
         i4 = i3 + istride, o4 = o3 + ostride,
         i5 = i4 + istride, o5 = o4 + ostride,
         i6 = i5 + istride, o6 = o5 + ostride,
         ir0 = i0<<1, ii0 = ir0 + 1, or0 = o0<<1, oi0 = or0 + 1,
         ir1 = i1<<1, ii1 = ir1 + 1, or1 = o1<<1, oi1 = or1 + 1,
         ir2 = i2<<1, ii2 = ir2 + 1, or2 = o2<<1, oi2 = or2 + 1,
         ir3 = i3<<1, ii3 = ir3 + 1, or3 = o3<<1, oi3 = or3 + 1,
         ir4 = i4<<1, ii4 = ir4 + 1, or4 = o4<<1, oi4 = or4 + 1,
         ir5 = i5<<1, ii5 = ir5 + 1, or5 = o5<<1, oi5 = or5 + 1,
         ir6 = i6<<1, ii6 = ir6 + 1, or6 = o6<<1, oi6 = or6 + 1 };
  static inline void execute(t_fft *_x, t_fft *_y, int step) {
    float *x = (float*)_x;
    float *y = (float*)_y;
    float u00, u01, u10, u11, u20, u21, u30, u31, u40, u41, u50, u51, u60, u61, u70, u71;
    float b00, b01, b10, b11, b20, b21, b30, b31, b40, b41, b50, b51, b60, b61, b70, b71, b80, b81;
    float T00, T01, T10, T11, T20, T21, T30, T31, T40, T41, T50, T51, T60, T61, T70, T71, T80, T81, T90, T91, T100, T101, T110, T111, T120, T121;
    u00 = x[ir1] + x[ir6]; u01 = x[ii1] + x[ii6];
    u10 = x[ir1] - x[ir6]; u11 = x[ii1] - x[ii6];
    u20 = x[ir2] + x[ir5]; u21 = x[ii2] + x[ii5];
    u30 = x[ir2] - x[ir5]; u31 = x[ii2] - x[ii5];
    u40 = x[ir4] + x[ir3]; u41 = x[ii4] + x[ii3];
    u50 = x[ir4] - x[ir3]; u51 = x[ii4] - x[ii3];
    u60 = u20 + u00; u61 = u21 + u01;
    u70 = u50 + u30; u71 = u51 + u31;
    b00 = x[ir0] + u60 + u40; b01 = x[ii0] + u61 + u41;
    b10 = -1.16666666666667f*(u60 + u40); b11 = -1.16666666666667f*(u61 + u41);
    b20 = 0.79015646852540f*(u00 - u40); b21 = 0.79015646852540f*(u01 - u41);
    b30 = 0.05585426728965f*(u40 - u20); b31 = 0.05585426728965f*(u41 - u21);
    b40 = 0.73430220123575f*(u20 - u00); b41 = 0.73430220123575f*(u21 - u01);
    if(dir==1) {
      b50 = 0.44095855184410f*(u70 + u10);
      b51 = 0.44095855184410f*(u71 + u11);
      b60 = 0.34087293062393f*(u10 - u50);
      b61 = 0.34087293062393f*(u11 - u51);
      b70 = -0.53396936033773f*(u50 - u30);
      b71 = -0.53396936033773f*(u51 - u31);
      b80 = 0.87484229096166f*(u30 - u10);
      b81 = 0.87484229096166f*(u31 - u11);
    } else {
      b50 = -0.44095855184410f*(u70 + u10);
      b51 = -0.44095855184410f*(u71 + u11);
      b60 = 0.34087293062393f*(u50 - u10);
      b61 = 0.34087293062393f*(u51 - u11);
      b70 = -0.53396936033773f*(u30 - u50);
      b71 = -0.53396936033773f*(u31 - u51);
      b80 = 0.87484229096166f*(u10 - u30);
      b81 = 0.87484229096166f*(u11 - u31);
    }
    T00 = b00 + b10; T01 = b01 + b11;
    T10 = b20 + b30; T11 = b21 + b31;
    T20 = b40 - b30; T21 = b41 - b31;
    T30 = -b20 - b40; T31 = -b21 - b41;
    T40 = b60 + b70; T41 = b61 + b71;
    T50 = b80 - b70; T51 = b81 - b71;
    T60 = -b80 - b60; T61 = -b81 - b61;
    T70 = T00 + T10; T71 = T01 + T11;
    T80 = T00 + T20; T81 = T01 + T21;    
    T90 = T00 + T30; T91 = T01 + T31;
    T100 = T40 + b50; T101 = T41 + b51;
    T110 = T50 + b50; T111 = T51 + b51;
    T120 = T60 + b50; T121 = T61 + b51;
    y[or0] = b00; y[oi0] = b01;
    if(step) {
      FloatTwiddle<N,dir>::twiddle(step,_y+o1,T70 + T101,T71 - T100);
      int step2 = step + step;
      FloatTwiddle<N,dir>::twiddle(step2,_y+o2,T90 + T121,T91 - T120);
      int step3 = step2 + step;
      FloatTwiddle<N,dir>::twiddle(step3,_y+o3,T80 - T111,T81 + T110);
      FloatTwiddle<N,dir>::twiddle(step2+step2,_y+o4,T80 + T111,T81 - T110);
      FloatTwiddle<N,dir>::twiddle(step3+step2,_y+o5,T90 - T121,T91 + T120);
      FloatTwiddle<N,dir>::twiddle(step3+step3,_y+o6,T70 - T101,T71 + T100);
    } else {
       y[or1] = T70 + T101; y[oi1] = T71 - T100;
       y[or2] = T90 + T121; y[oi2] = T91 - T120;
       y[or3] = T80 - T111; y[oi3] = T81 + T110;
       y[or4] = T80 + T111; y[oi4] = T81 - T110;
       y[or5] = T90 - T121; y[oi5] = T91 + T120;
       y[or6] = T70 - T101; y[oi6] = T71 + T100;
    }
  }
};

#if defined(ENABLE_SSE) && !defined(APPLE_PPC) && !defined(_M_ARM64)

template <int N, int dir> 
class SSETwiddles {
public:
  simd_vector cs[N];
  SSETwiddles() {
    for(int k=0; k<N; k++) {
      float c = cos(TWOPI * (float)k / (float)N);
      float s = sin(TWOPI * (float)(-dir*k) / (float)N);
      simd_vector v = {c,s,c,-s};
      cs[k] = v;
    }
  }
};

template <int N, int dir> 
class SSETwiddle {
public:
  static const SSETwiddles<N,dir> t;
  static const simd_vector *cs;
  static inline simd_vector twiddle(int k, const simd_vector &v) {
    return VMUL(cs[k],v);
  }
  static inline void twiddle(int k, t_fft *x, const simd_vector &v) {
    simd_vector y = twiddle(k,v);
    simd_vector z = VADD(SHUFFLE(y,y,0,1,0,1),SHUFFLE(y,y,3,2,3,2));
    STOREL(z,x);
  }
  static inline void twiddle2(int k1, int k2, t_fft *x1, t_fft *x2, const simd_vector &y1, const simd_vector &y2) {
    simd_vector v1 = VMUL(cs[k1],y1);
    simd_vector v2 = VMUL(cs[k2],y2);
    simd_vector z = VADD(SHUFFLE(v1,v2,0,1,0,1),SHUFFLE(v1,v2,3,2,3,2));
    STOREL(z,x1);
    STOREH(z,x2);
  }
};

template <int N, int dir>
  const SSETwiddles<N,dir> SSETwiddle<N,dir>::t;

template <int N, int dir>
  const simd_vector* SSETwiddle<N,dir>::cs = t.cs;

template<int istride, int ostride, int dir>
class __fft<istride,ostride,4,dir> {
public:
  enum { N = istride * 4,
         ir0 = 0, or0 = 0,
         _ir1 = ir0 + istride, or1 = or0 + ostride,
         ir2 = _ir1 + istride, or2 = or1 + ostride,
         _ir3 = ir2 + istride, or3 = or2 + ostride,
         ir1 = (dir==1?_ir3:_ir1),
         ir3 = (dir==1?_ir1:_ir3) };
  static inline void execute(t_fft *x, t_fft *y, int step) {
    simd_vector v1 = {};
    simd_vector v2 = {};
    simd_vector v3;
    simd_vector v4;
    simd_vector v5;
    v1 = LOADH(LOADL(v1,x+ir0),x+ir1);
    v2 = LOADH(LOADL(v2,x+ir2),x+ir3);
    v3 = VADD(v1,v2);
    v4 = VSUB(v1,v2);
    v1 = SHUFFLE(v4,v3,0,1,0,1);
    v2 = SHUFFLE(v4,v3,3,2,2,3);
    v3 = VADD(v1,v2);
    STOREH(v3,y+or0);
    v4 = VSUB(v1,v2);
    v1 = SHUFFLE(v4,v3,0,0,1,1);
    v2 = SHUFFLE(v4,v4,2,2,3,3);
    v5 = SHUFFLE(v3,v4,0,0,1,1);
    if(step) {
      v1 = SSETwiddle<N,dir>::twiddle(step,v1);
      int step2 = step + step;
      v2 = SSETwiddle<N,dir>::twiddle(step2,v2);
      v3 = SSETwiddle<N,dir>::twiddle(step2+step,v5);
      v3 = VADD(v3,SHUFFLE(v3,v3,3,2,1,0));
      STOREL(v3,y+or3);
      v5 = VADD(SHUFFLE(v1,v2,0,1,0,1),SHUFFLE(v1,v2,3,2,3,2));
      STOREL(v5,y+or1);
      STOREH(v5,y+or2);
    } else {
      v5 = SHUFFLE(v5,v5,0,2,0,2);
      STOREL(v5,y+or3);
      v3 = SHUFFLE(v1,v2,0,2,0,2);
      STOREL(v3,y+or1);
      STOREH(v3,y+or2);
    }
  }
};

template<int istride, int ostride> 
class __fft<istride,ostride,8,1> {
public:
  enum { N = istride*8,
         i0 = 0, o0 = 0,
         i1 = i0 + istride, o1 = o0 + ostride,
         i2 = i1 + istride, o2 = o1 + ostride,
         i3 = i2 + istride, o3 = o2 + ostride,
         i4 = i3 + istride, o4 = o3 + ostride,
         i5 = i4 + istride, o5 = o4 + ostride,
         i6 = i5 + istride, o6 = o5 + ostride,
         i7 = i6 + istride, o7 = o6 + ostride };
  static inline void execute(t_fft *x, t_fft *y, int step) {
    simd_vector v1 = {}, v2 = {};
    simd_vector v3,v4,v5,v6,v7,v8;
    simd_vector x02,x37,x15,x17,x53,x46;
    simd_vector w1 = {}, w2 = {};
    simd_vector w3, w4;
    w1 = LOADH(LOADL(w1,x+i0),x+i6);
    w2 = LOADH(LOADL(w2,x+i4),x+i2);	
    w3 = VADD(w1,w2);
    w4 = VSUB(w1,w2);
    v1 = LOADH(LOADL(v1,x+i1),x+i7);
    v2 = LOADH(LOADL(v2,x+i5),x+i3);
    v3 = VADD(v1,v2);
    v4 = VSUB(v1,v2);
    v5 = SHUFFLE(v3,v4,2,1,0,1); //(z10,z01,z20,z21)
    v6 = SHUFFLE(v3,v4,0,3,3,2); //(z00,z11,z31,z30)
    v7 = VADD(v5,v6); // (x1,y30,y11)
    v8 = VSUB(v5,v6); // (x51,x50,y10,y31)
    x15 = SHUFFLE(v7,v8,0,1,1,0); //(x1,x5)
    v2 = SHUFFLE(v8,v8,3,3,3,2); //(-,-,y31,y10)
    v3 = VADD(v7,v2);
    v4 = VSUB(v7,v2);
    v5 = SHUFFLE(v3,v4,2,3,2,3);
    x37 = VMUL(SET(0.7071067811865475f),v5); //(-x71,x30,-x70,x31)
    v5 = SHUFFLE(w3,w4,0,1,0,1); //(z00,z01,z20,z21)
    v6 = SHUFFLE(w3,w4,2,3,3,2); //(z10,z11,z31,z30)
    v7 = VADD(v5,v6); //(x0,y30,y11)
    v8 = VSUB(v5,v6); //(x4,y10,y31)
    v1 = SHUFFLE(v7,v8,0,1,0,1); //(x0,x4)
    v2 = SHUFFLE(v8,v7,3,2,2,3); //(x61,x20,x60,x21)
    x02 = VADD(v1,x15);
    x46 = VSUB(v1,x15);
    x17 = VADD(v2,x37);
    x53 = VSUB(v2,x37);
    STOREL(x02,y+o0);
    if(step) {
      simd_vector y1;
      simd_vector y2;
      int step2 = step + step;
      y1 = SHUFFLE(x02,x02,2,2,3,3);
      SSETwiddle<N,1>::twiddle(step2,y+o2,y1);
      int step3 = step2 + step;
      y1 = SHUFFLE(x53,x53,1,1,3,3);
      y2 = SHUFFLE(x53,x53,2,2,0,0);
      SSETwiddle<N,1>::twiddle2(step3+step2,step3,y+o5,y+o3,y1,y2);
      int step4 = step2 + step2;
      y1 = SHUFFLE(x46,x46,0,0,1,1);
      y2 = SHUFFLE(x46,x46,2,2,3,3);
      SSETwiddle<N,1>::twiddle2(step4,step3+step3,y+o4,y+o6,y1,y2);
      y1 = SHUFFLE(x17,x17,1,1,3,3);
      y2 = SHUFFLE(x17,x17,2,2,0,0);
      SSETwiddle<N,1>::twiddle2(step,step4+step3,y+o1,y+o7,y1,y2);
    } else {
      simd_vector y1;
      y1 = SHUFFLE(x02,x02,2,3,3,3);
      STOREL(y1,y+o2);
      y1 = SHUFFLE(x53,x53,1,3,2,0);
      STOREL(y1,y+o5);
      STOREH(y1,y+o3);
      STOREL(x46,y+o4);
      STOREH(x46,y+o6);
      y1 = SHUFFLE(x17,x17,1,3,2,0);
      STOREL(y1,y+o1);
      STOREH(y1,y+o7);
    }
  }
};

template<int istride, int ostride> 
class __fft<istride,ostride,8,-1> {
public:
  enum { N = istride*8,
         i0 = 0, o0 = 0,
         i1 = i0 + istride, o1 = o0 + ostride,
         i2 = i1 + istride, o2 = o1 + ostride,
         i3 = i2 + istride, o3 = o2 + ostride,
         i4 = i3 + istride, o4 = o3 + ostride,
         i5 = i4 + istride, o5 = o4 + ostride,
         i6 = i5 + istride, o6 = o5 + ostride,
         i7 = i6 + istride, o7 = o6 + ostride };
  static inline void execute(t_fft *x, t_fft *y, int step) {
    simd_vector v1 = {}, v2 = {};
    simd_vector v3,v4,v5,v6,v7,v8;
    simd_vector x02,x37,x15,x17,x53,x46;    
    simd_vector w1 = {}, w2 = {};
    simd_vector w3, w4;
    w1 = LOADH(LOADL(w1,x+i0),x+i2);
    w2 = LOADH(LOADL(w2,x+i4),x+i6);	
    w3 = VADD(w1,w2);
    w4 = VSUB(w1,w2);
    v1 = LOADH(LOADL(v1,x+i1),x+i3);
    v2 = LOADH(LOADL(v2,x+i5),x+i7);
    v3 = VADD(v1,v2);
    v4 = VSUB(v1,v2);
    v5 = SHUFFLE(v3,v4,0,3,0,1);
    v6 = SHUFFLE(v3,v4,2,1,3,2);
    v7 = VADD(v5,v6);
    v8 = VSUB(v5,v6);
    x15 = SHUFFLE(v7,v8,0,1,1,0);
    v2 = SHUFFLE(v8,v8,3,3,3,2);
    v3 = VADD(v7,v2);
    v4 = VSUB(v2,v7);
    v5 = SHUFFLE(v4,v3,2,3,2,3);
    x37 = VMUL(SET(0.7071067811865475f),v5);
    v5 = SHUFFLE(w3,w4,0,1,0,1);
    v6 = SHUFFLE(w3,w4,2,3,3,2);
    v7 = VADD(v5,v6);
    v8 = VSUB(v5,v6);
    v1 = SHUFFLE(v7,v8,0,1,0,1);
    v2 = SHUFFLE(v8,v7,3,2,2,3);
    x02 = VADD(v1,x15);
    x46 = VSUB(v1,x15);
    x17 = VADD(v2,x37);
    x53 = VSUB(v2,x37);
    STOREL(x02,y+o0);
    if(step) {
      simd_vector y1;
      simd_vector y2;
      int step2 = step + step;
      y1 = SHUFFLE(x02,x02,2,2,3,3);
      SSETwiddle<N,-1>::twiddle(step2,y+o2,y1);
      int step3 = step2 + step;
      y1 = SHUFFLE(x53,x53,1,1,3,3);
      y2 = SHUFFLE(x53,x53,2,2,0,0);
      SSETwiddle<N,-1>::twiddle2(step3+step2,step3,y+o5,y+o3,y1,y2);
      int step4 = step2 + step2;
      y1 = SHUFFLE(x46,x46,0,0,1,1);
      y2 = SHUFFLE(x46,x46,2,2,3,3);
      SSETwiddle<N,-1>::twiddle2(step4,step3+step3,y+o4,y+o6,y1,y2);
      y1 = SHUFFLE(x17,x17,1,1,3,3);
      y2 = SHUFFLE(x17,x17,2,2,0,0);
      SSETwiddle<N,-1>::twiddle2(step,step4+step3,y+o1,y+o7,y1,y2);
    } else {
      simd_vector y1;
      y1 = SHUFFLE(x02,x02,2,3,3,3);
      STOREL(y1,y+o2);
      y1 = SHUFFLE(x53,x53,1,3,2,0);
      STOREL(y1,y+o5);
      STOREH(y1,y+o3);
      STOREL(x46,y+o4);
      STOREH(x46,y+o6);
      y1 = SHUFFLE(x17,x17,1,3,2,0);
      STOREL(y1,y+o1);
      STOREH(y1,y+o7);
    }
  }
};

#else // !ENABLE_SSE

template<int istride, int ostride, int dir> 
  class __fft<istride,ostride,4,dir> {
 public:
  enum { N = istride*4,
         i0 = 0, o0 = 0,
         i1_ = i0 + istride, o1 = o0 + ostride,
         i2 = i1_ + istride, o2 = o1 + ostride,
         i3_ = i2 + istride, o3 = o2 + ostride,
         i1 = (dir==1?i1_:i3_),
         i3 = (dir==1?i3_:i1_),
         ir0 = i0<<1, ii0 = ir0 + 1, or0 = o0<<1, oi0 = or0 + 1,
         ir1 = i1<<1, ii1 = ir1 + 1, or1 = o1<<1, oi1 = or1 + 1,
         ir2 = i2<<1, ii2 = ir2 + 1, or2 = o2<<1, oi2 = or2 + 1,
         ir3 = i3<<1, ii3 = ir3 + 1, or3 = o3<<1, oi3 = or3 + 1 };
  static inline void execute(t_fft *_x, t_fft *_y, int step) {
    float *x = (float*)_x;
    float *y = (float*)_y;
    float z00, z01, z10, z11, z20, z21, z30, z31;
    z20 = x[ir0] - x[ir2]; z21 = x[ii0] - x[ii2];
    z00 = x[ir0] + x[ir2]; z01 = x[ii0] + x[ii2];
    z10 = x[ir1] + x[ir3]; z11 = x[ii1] + x[ii3];
    y[or0] = z00 + z10; y[oi0] = z01 + z11;
    z30 = x[ir3] - x[ir1]; z31 = x[ii3] - x[ii1];
    if(step) {
      int step2 = step + step;
      FloatTwiddle<N,dir>::twiddle(step,_y+o1,z20 - z31,z21 + z30);
      FloatTwiddle<N,dir>::twiddle(step2,_y+o2,z00 - z10,z01 - z11);
      FloatTwiddle<N,dir>::twiddle(step2+step,_y+o3,z20 + z31,z21 - z30);
    } else {
      y[or1] = z20 - z31; y[oi1] = z21 + z30;
      y[or2] = z00 - z10; y[oi2] = z01 - z11;
      y[or3] = z20 + z31; y[oi3] = z21 - z30;
    }
  }
};

template<int istride, int ostride> 
class __fft<istride,ostride,8,1> {
public:
  enum { N = istride*8,
         i0 = 0, o0 = 0,
         i1 = i0 + istride, o1 = o0 + ostride,
         i2 = i1 + istride, o2 = o1 + ostride,
         i3 = i2 + istride, o3 = o2 + ostride,
         i4 = i3 + istride, o4 = o3 + ostride,
         i5 = i4 + istride, o5 = o4 + ostride,
         i6 = i5 + istride, o6 = o5 + ostride,
         i7 = i6 + istride, o7 = o6 + ostride,         
         ir0 = i0<<1, ii0 = ir0 + 1, or0 = o0<<1, oi0 = or0 + 1,
         ir1 = i1<<1, ii1 = ir1 + 1, or1 = o1<<1, oi1 = or1 + 1,
         ir2 = i2<<1, ii2 = ir2 + 1, or2 = o2<<1, oi2 = or2 + 1,
         ir3 = i3<<1, ii3 = ir3 + 1, or3 = o3<<1, oi3 = or3 + 1,
         ir4 = i4<<1, ii4 = ir4 + 1, or4 = o4<<1, oi4 = or4 + 1,
         ir5 = i5<<1, ii5 = ir5 + 1, or5 = o5<<1, oi5 = or5 + 1,
         ir6 = i6<<1, ii6 = ir6 + 1, or6 = o6<<1, oi6 = or6 + 1,
         ir7 = i7<<1, ii7 = ir7 + 1, or7 = o7<<1, oi7 = or7 + 1 };
  static inline void execute(t_fft *_x, t_fft *_y, int step) {
    float *x = (float*)_x;
    float *y = (float*)_y;
		float z00, z01, z10, z11, z20, z21, z30, z31, y10, y11, y30, y31;
    float yr0, yi0, yr1, yi1, yr2, yi2, yr3, yi3, yr4, yi4, yr5, yi5, yr6, yi6, yr7, yi7;
		z00 = x[ir1] + x[ir5]; z01 = x[ii1] + x[ii5];
		z10 = x[ir7] + x[ir3]; z11 = x[ii7] + x[ii3];
		z20 = x[ir1] - x[ir5]; z21 = x[ii1] - x[ii5];
    z30 = x[ir7] - x[ir3]; z31 = x[ii7] - x[ii3];
		y10 = z20 - z31; y11 = z21 + z30;
		y30 = z20 + z31; y31 = z21 - z30;
		yr1 = z00 + z10; yi1 = z01 + z11;
		yr5 = z01 - z11; yi5 = z10 - z00;
		yr3 = 0.7071067811865475f * (y10 + y11); yi3 = 0.7071067811865475f * (y11 - y10);
		yr7 = 0.7071067811865475f * (y31 - y30); yi7 = -0.7071067811865475f * (y30 + y31);
		z20 = x[ir0] - x[ir4]; z21 = x[ii0] - x[ii4];
    z00 = x[ir0] + x[ir4]; z01 = x[ii0] + x[ii4];
		z10 = x[ir6] + x[ir2]; z11 = x[ii6] + x[ii2];
		z30 = x[ir6] - x[ir2]; z31 = x[ii6] - x[ii2];
		yr0 = z00 + z10; yi0 = z01 + z11;
		yr4 = z00 - z10; yi4 = z01 - z11;
		yr2 = z20 - z31; yi2 = z21 + z30;
		yr6 = z20 + z31; yi6 = z21 - z30;
		y[or0] = yr0 + yr1; y[oi0] = yi0 + yi1;
    if(step) {
      FloatTwiddle<N,1>::twiddle(step,_y+o1,yr2 + yr3, yi2 + yi3);
      int step2 = step + step;
      FloatTwiddle<N,1>::twiddle(step2,_y+o2,yr4 + yr5, yi4 + yi5);
      int step3 = step2 + step;
      FloatTwiddle<N,1>::twiddle(step3,_y+o3,yr6 + yr7, yi6 + yi7);
      FloatTwiddle<N,1>::twiddle(step2+step2,_y+o4,yr0 - yr1, yi0 - yi1);
      FloatTwiddle<N,1>::twiddle(step3+step2,_y+o5,yr2 - yr3, yi2 - yi3);
      FloatTwiddle<N,1>::twiddle(step3+step3,_y+o6,yr4 - yr5, yi4 - yi5);
      FloatTwiddle<N,1>::twiddle(step3+step2+step2,_y+o7,yr6 - yr7, yi6 - yi7);
    } else {
      y[or1] = yr2 + yr3; y[oi1] = yi2 + yi3;
      y[or2] = yr4 + yr5; y[oi2] = yi4 + yi5;
      y[or3] = yr6 + yr7; y[oi3] = yi6 + yi7;
      y[or4] = yr0 - yr1; y[oi4] = yi0 - yi1;
      y[or5] = yr2 - yr3; y[oi5] = yi2 - yi3;
      y[or6] = yr4 - yr5; y[oi6] = yi4 - yi5;
      y[or7] = yr6 - yr7; y[oi7] = yi6 - yi7;
    }
  }
};

template<int istride, int ostride> 
class __fft<istride,ostride,8,-1> {
public:
  enum { N = istride*8,
         i0 = 0, o0 = 0,
         i1 = i0 + istride, o1 = o0 + ostride,
         i2 = i1 + istride, o2 = o1 + ostride,
         i3 = i2 + istride, o3 = o2 + ostride,
         i4 = i3 + istride, o4 = o3 + ostride,
         i5 = i4 + istride, o5 = o4 + ostride,
         i6 = i5 + istride, o6 = o5 + ostride,
         i7 = i6 + istride, o7 = o6 + ostride,
         ir0 = i0<<1, ii0 = ir0 + 1, or0 = o0<<1, oi0 = or0 + 1,
         ir1 = i1<<1, ii1 = ir1 + 1, or1 = o1<<1, oi1 = or1 + 1,
         ir2 = i2<<1, ii2 = ir2 + 1, or2 = o2<<1, oi2 = or2 + 1,
         ir3 = i3<<1, ii3 = ir3 + 1, or3 = o3<<1, oi3 = or3 + 1,
         ir4 = i4<<1, ii4 = ir4 + 1, or4 = o4<<1, oi4 = or4 + 1,
         ir5 = i5<<1, ii5 = ir5 + 1, or5 = o5<<1, oi5 = or5 + 1,
         ir6 = i6<<1, ii6 = ir6 + 1, or6 = o6<<1, oi6 = or6 + 1,
         ir7 = i7<<1, ii7 = ir7 + 1, or7 = o7<<1, oi7 = or7 + 1 };
  static inline void execute(t_fft *_x, t_fft *_y, int step) {
    float *x = (float*)_x;
    float *y = (float*)_y;
		float z00, z01, z10, z11, z20, z21, z30, z31, y10, y11, y30, y31;
    float yr0, yi0, yr1, yi1, yr2, yi2, yr3, yi3, yr4, yi4, yr5, yi5, yr6, yi6, yr7, yi7;
		z00 = x[ir1] + x[ir5]; z01 = x[ii1] + x[ii5];
		z10 = x[ir3] + x[ir7]; z11 = x[ii3] + x[ii7];
		z20 = x[ir1] - x[ir5]; z21 = x[ii1] - x[ii5];
		z30 = x[ir3] - x[ir7]; z31 = x[ii3] - x[ii7];
		y10 = z20 - z31; y11 = z21 + z30;
		y30 = z20 + z31; y31 = z21 - z30;
		yr1 = z00 + z10; yi1 = z01 + z11;
    yr5 = z11 - z01; yi5 = z00 - z10;
		yr3 = 0.7071067811865475f * (y10 - y11); yi3 = 0.7071067811865475f * (y10 + y11);
		yr7 = -0.7071067811865475f * (y30 + y31); yi7 = 0.7071067811865475f * (y30 - y31);
		z00 = x[ir0] + x[ir4]; z01 = x[ii0] + x[ii4];
		z10 = x[ir2] + x[ir6]; z11 = x[ii2] + x[ii6];
		z20 = x[ir0] - x[ir4]; z21 = x[ii0] - x[ii4];
		z30 = x[ir2] - x[ir6]; z31 = x[ii2] - x[ii6];
		yr0 = z00 + z10; yi0 = z01 + z11;
		yr4 = z00 - z10; yi4 = z01 - z11;
		yr2 = z20 - z31; yi2 = z21 + z30;
		yr6 = z20 + z31; yi6 = z21 - z30;
		y[or0] = yr0 + yr1;
		y[oi0] = yi0 + yi1;
    if(step) {
      FloatTwiddle<N,-1>::twiddle(step,_y+o1,yr2 + yr3, yi2 + yi3);
      int step2 = step + step;
      FloatTwiddle<N,-1>::twiddle(step2,_y+o2,yr4 + yr5, yi4 + yi5);
      int step3 = step2 + step;
      FloatTwiddle<N,-1>::twiddle(step3,_y+o3,yr6 + yr7, yi6 + yi7);
      FloatTwiddle<N,-1>::twiddle(step2+step2,_y+o4,yr0 - yr1, yi0 - yi1);
      FloatTwiddle<N,-1>::twiddle(step3+step2,_y+o5,yr2 - yr3, yi2 - yi3);
      FloatTwiddle<N,-1>::twiddle(step3+step3,_y+o6,yr4 - yr5, yi4 - yi5);
      FloatTwiddle<N,-1>::twiddle(step3+step2+step2,_y+o7,yr6 - yr7, yi6 - yi7);
    } else {
      y[or1] = yr2 + yr3; y[oi1] = yi2 + yi3;
      y[or2] = yr4 + yr5; y[oi2] = yi4 + yi5;
      y[or3] = yr6 + yr7; y[oi3] = yi6 + yi7;
      y[or4] = yr0 - yr1; y[oi4] = yi0 - yi1;
      y[or5] = yr2 - yr3; y[oi5] = yi2 - yi3;
      y[or6] = yr4 - yr5; y[oi6] = yi4 - yi5;
      y[or7] = yr6 - yr7; y[oi7] = yi6 - yi7;
    }
  }
};

#endif // ENABLE_SSE

template<int k, int stride, int radix, int dir>
class _fft {
public:
  enum { N = radix * stride,
         _radix = Factor<stride>::value,
         _stride = stride / _radix };
  static inline void execute(t_fft *x) {
    for(int step=0; step<stride; step++) {
      __fft<stride,stride,radix,dir>::execute(x+step,x+step,step);
    }
    _fft<N,_stride,_radix,dir>::loop(x+N);
  }
  static inline void loop(t_fft *x) {
    _fft<k-N,stride,radix,dir>::execute(x-N);
    _fft<k-N,stride,radix,dir>::loop(x-N);
  }
};

template<int stride, int radix, int dir>
class _fft<0,stride,radix,dir> {
public:
  enum { N = radix * stride,
         _radix = Factor<stride>::value,
         _stride = stride / _radix };
  static inline void execute(t_fft *x) {
    for(int step=0; step<stride; step++) {
      __fft<stride,stride,radix,dir>::execute(x+step,x+step,step);
    }
    _fft<N,_stride,_radix,dir>::loop(x+N);
  }
  static inline void loop(t_fft *x) { }
};

template <int k, int radix, int dir>
class _fft<k,1,radix,dir> {
public:
  static inline void execute(t_fft *x) { }
  static inline void loop(t_fft *x) { }
};

template <int radix, int dir>
class _fft<0,1,radix,dir> {
public:
  static inline void execute(t_fft *x) { }
  static inline void loop(t_fft *x) { }
};

template<int N>
class fft_order_ {
public:
  static inline int iterate(int k) {
    static const int N2 = Factor<N>::value;
    static const int N1 = N/Factor<N>::value;
    return (k%N2)*N1 + fft_order_<N1>::iterate(k/N2);
  }
};

template<>
class fft_order_<1> {
public:
  static inline int iterate(int k0) {
    return 0;
  }
};

template<int N>
class fft_order {
public:
  fft_order<N>() {
    for(int k=0;k<N;k++) {
      int kr = fft_order_<N>::iterate(k);
      order[kr] = k;
    }
  }
  int order[N];
};

template<int N, int dir>
class fft_reorder {
public:
  enum { radix = LastFactor<N>::value, 
         ostride = N / radix,
         s = N * sizeof(t_fft) };
  static const fft_order<N> order;
  static inline void reorder(t_fft *x) {
    t_fft y[N];
    int *o = (int*)order.order;
    memcpy(y,x,s);
    for(int r=0; r<N; r+=radix) {
      __fft<1,ostride,radix,dir>::execute(y+r,x+o[r],0);
    }
  }
};

template<int N, int dir>
const fft_order<N> fft_reorder<N,dir>::order;

template<int N, int dir>
void fft(t_fft *x) {
  enum { radix = Factor<N>::value, 
         stride = N / radix };
  _fft<0,stride,radix,dir>::execute(x);
  fft_reorder<N,dir>::reorder(x);
}

}

#endif
