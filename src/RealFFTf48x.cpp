/**********************************************************************

   Audacity: A Digital Audio Editor

   RealFFT48x.cpp

   Philip Van Baren
   Andrew Hallendorff (SSE Mods)

*******************************************************************//**

   \file RealFFT48x.cpp
   \brief Real FFT with SSE acceleration.

*//****************************************************************/

/*
*     Program: REALFFTF.C
*      Author: Philip Van Baren
*        Date: 2 September 1993
*
* Description: These routines perform an FFT on real data to get a conjugate-symmetric
*              output, and an inverse FFT on conjugate-symmetric input to get a real
*              output sequence.
*
*              This code is for floating point data.
*
*              Modified 8/19/1998 by Philip Van Baren
*                 - made the InitializeFFT and EndFFT routines take a structure
*                   holding the length and pointers to the BitReversed and SinTable
*                   tables.
*              Modified 5/23/2009 by Philip Van Baren
*                 - Added GetFFT and ReleaseFFT routines to retain common SinTable
*                   and BitReversed tables so they don't need to be reallocated
*                   and recomputed on every call.
*                 - Added Reorder* functions to undo the bit-reversal
*              Modified 15 April 2016 Paul Licameli
*                 - C++11 smart pointers
*
*  Copyright (C) 2009  Philip VanBaren
*
*  This program is free software; you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation; either version 2 of the License, or
*  (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software
*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


#include "RealFFTf48x.h"



#ifdef EXPERIMENTAL_EQ_SSE_THREADED

// at the moment these two are mutually exclusive
//#define USE_SSEMATHFUNC
//#define TEST_COSSINBIT_TABLES

#ifndef USE_SSE2
#define  USE_SSE2
#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "RealFFTf.h"
#ifdef __WXMSW__
#pragma warning(disable:4305)
#else

#endif 
#include "SseMathFuncs.h"
#include <intrin.h>

#ifndef M_PI
#define	M_PI		3.14159265358979323846  /* pi */
#endif

int tableMask=0;
bool useBitReverseTable=false;
bool useSinCosTable=false;

void TableUsage(int iMask)
{
   tableMask=iMask;
   useBitReverseTable=(iMask&1)!=0;
   useSinCosTable=(iMask&2)!=0;
}

SinCosTable::SinCosTable() :
mSinCosTablePow(13)
{
   size_t tableSize=1<<mSinCosTablePow;
   mSinCosTable.reinit(tableSize);
   for(size_t i=0;i<tableSize;i++) {
      mSinCosTable[i].mSin=(float)-sin(((float)i)*M_PI/tableSize);
      mSinCosTable[i].mCos=(float)-cos(((float)i)*M_PI/tableSize);
   }
};

static SinCosTable sSinCosTable;

static unsigned char sSmallRBTable[256];

class BitReverser {
   public:
      BitReverser()
      {
         for(int i=0;i<256;i++) {
            sSmallRBTable[i]=0;
            for(int maskLow=1, maskHigh=128;maskLow<256;maskLow<<=1,maskHigh>>=1) 
               if(i&maskLow)
                  sSmallRBTable[i]|=maskHigh;
         }
      };
   };
static BitReverser sBitReverser;

/* array of functions there prob is a better way to do this */
int SmallVRB0(int bits) { return bits; }; int SmallVRB1(int bits) { return sSmallRBTable[bits]>>7; };
int SmallVRB2(int bits) { return sSmallRBTable[bits]>>6; }; int SmallVRB3(int bits) { return sSmallRBTable[bits]>>5; };
int SmallVRB4(int bits) { return sSmallRBTable[bits]>>4; }; int SmallVRB5(int bits) { return sSmallRBTable[bits]>>3; };
int SmallVRB6(int bits) { return sSmallRBTable[bits]>>2; }; int SmallVRB7(int bits) { return sSmallRBTable[bits]>>1; };
int SmallVRB8(int bits) { return sSmallRBTable[bits]; };
int SmallVRB9(int bits) { return (sSmallRBTable[*((unsigned char *)&bits)]<<1)+(sSmallRBTable[*(((unsigned char *)&bits)+1)]>>7); };
int SmallVRB10(int bits) { return (sSmallRBTable[*((unsigned char *)&bits)]<<2)+(sSmallRBTable[*(((unsigned char *)&bits)+1)]>>6); };
int SmallVRB11(int bits) { return (sSmallRBTable[*((unsigned char *)&bits)]<<3)+(sSmallRBTable[*(((unsigned char *)&bits)+1)]>>5); };
int SmallVRB12(int bits) { return (sSmallRBTable[*((unsigned char *)&bits)]<<4)+(sSmallRBTable[*(((unsigned char *)&bits)+1)]>>4); };
int SmallVRB13(int bits) { return (sSmallRBTable[*((unsigned char *)&bits)]<<5)+(sSmallRBTable[*(((unsigned char *)&bits)+1)]>>3); };
int SmallVRB14(int bits) { return (sSmallRBTable[*((unsigned char *)&bits)]<<6)+(sSmallRBTable[*(((unsigned char *)&bits)+1)]>>2); };
int SmallVRB15(int bits) { return (sSmallRBTable[*((unsigned char *)&bits)]<<7)+(sSmallRBTable[*(((unsigned char *)&bits)+1)]>>1); };
int SmallVRB16(int bits) { return (sSmallRBTable[*((unsigned char *)&bits)]<<8)+(sSmallRBTable[*(((unsigned char *)&bits)+1)]); };

int (*SmallVRB[])(int bits) = { SmallVRB0, SmallVRB1, SmallVRB2, SmallVRB3, SmallVRB4,
   SmallVRB5, SmallVRB6, SmallVRB7, SmallVRB8, SmallVRB9, SmallVRB10,
   SmallVRB11, SmallVRB12, SmallVRB13, SmallVRB14,SmallVRB15, SmallVRB16 };

int SmallRB(int bits, int numberBits)
{
//   return  ( (sSmallRBTable[*((unsigned char *)&bits)]<<16) + (sSmallRBTable[*(((unsigned char *)&bits)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&bits)+2)] ))>>(24-numberBits);
   return ( (sSmallRBTable[*((unsigned char *)&bits)]<<8) + (sSmallRBTable[*(((unsigned char *)&bits)+1)]) )>>(16-numberBits);
};

/* wrapper functions. If passed -1 function choice will be made locally */
void RealFFTf1x(fft_type *buffer, FFTParam *h, int functionType)
{
   switch(functionType) {
   case FFT_SinCosTableVBR16:
      RealFFTf1xSinCosTableVBR16( buffer, h);
      break;
   case FFT_SinCosTableBR16:
      RealFFTf1xSinCosTableBR16( buffer, h);
      break;
   case FFT_FastMathBR16:
      RealFFTf1xFastMathBR16( buffer, h);
      break;
   case FFT_FastMathBR24:
      RealFFTf1xFastMathBR24( buffer, h);
      break;
   case FFT_SinCosBRTable:
   default:
      RealFFTf1xSinCosBRTable( buffer, h);
   };
}

void InverseRealFFTf1x(fft_type *buffer, FFTParam *h, int functionType)
{
   switch(functionType) {
   case FFT_SinCosTableVBR16:
      InverseRealFFTf1xSinCosTableVBR16( buffer, h);
      break;
   case FFT_SinCosTableBR16:
      InverseRealFFTf1xSinCosTableBR16( buffer, h);
      break;
   case FFT_FastMathBR16:
      InverseRealFFTf1xFastMathBR16( buffer, h);
      break;
   case FFT_FastMathBR24:
      InverseRealFFTf1xFastMathBR24( buffer, h);
      break;
   case FFT_SinCosBRTable:
   default:
      InverseRealFFTf1xSinCosBRTable( buffer, h);
   };
}

void ReorderToTime1x(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut, int functionType)
{
   switch(functionType) {
   case FFT_SinCosTableVBR16:
      ReorderToTime1xSinCosTableVBR16( hFFT, buffer, TimeOut);
      break;
   case FFT_SinCosTableBR16:
      ReorderToTime1xSinCosTableBR16( hFFT, buffer, TimeOut);
      break;
   case FFT_FastMathBR16:
      ReorderToTime1xFastMathBR16( hFFT, buffer, TimeOut);
      break;
   case FFT_FastMathBR24:
      ReorderToTime1xFastMathBR24( hFFT, buffer, TimeOut);
      break;
   case FFT_SinCosBRTable:
   default:
      ReorderToTime1xSinCosBRTable( hFFT, buffer, TimeOut);
   };
}

void ReorderToFreq1x(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut, int functionType)
{
   switch(functionType) {
   case FFT_SinCosTableVBR16:
      ReorderToFreq1xSinCosTableVBR16(hFFT, buffer, RealOut, ImagOut);
      break;
   case FFT_SinCosTableBR16:
      ReorderToFreq1xSinCosTableBR16(hFFT, buffer, RealOut, ImagOut);
      break;
   case FFT_FastMathBR16:
      ReorderToFreq1xFastMathBR16(hFFT, buffer, RealOut, ImagOut);
      break;
   case FFT_FastMathBR24:
      ReorderToFreq1xFastMathBR24(hFFT, buffer, RealOut, ImagOut);
      break;
   case FFT_SinCosBRTable:
   default:
      ReorderToFreq1xSinCosBRTable(hFFT, buffer, RealOut, ImagOut);
   };
}

void RealFFTf4x( fft_type *buffer, FFTParam *h, int functionType)
{
   switch(functionType) {
   case FFT_SinCosTableVBR16:
      RealFFTf4xSinCosTableVBR16( buffer, h);
      break;
   case FFT_SinCosTableBR16:
      RealFFTf4xSinCosTableBR16( buffer, h);
      break;
   case FFT_FastMathBR16:
      RealFFTf4xFastMathBR16( buffer, h);
      break;
   case FFT_FastMathBR24:
      RealFFTf4xFastMathBR24( buffer, h);
      break;
   case FFT_SinCosBRTable:
   default:
      RealFFTf4xSinCosBRTable( buffer, h);
   };
}

void InverseRealFFTf4x( fft_type *buffer, FFTParam *h, int functionType)
{
   switch(functionType) {
   case FFT_SinCosTableVBR16:
      InverseRealFFTf4xSinCosTableVBR16( buffer, h);
      break;
   case FFT_SinCosTableBR16:
      InverseRealFFTf4xSinCosTableBR16( buffer, h);
      break;
   case FFT_FastMathBR16:
      InverseRealFFTf4xFastMathBR16( buffer, h);
      break;
   case FFT_FastMathBR24:
      InverseRealFFTf4xFastMathBR24( buffer, h);
      break;
   case FFT_SinCosBRTable:
   default:
      InverseRealFFTf4xSinCosBRTable( buffer, h);
   };
}

void ReorderToTime4x(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut, int functionType)
{
   switch(functionType) {
   case FFT_SinCosTableVBR16:
      ReorderToTime4xSinCosTableVBR16( hFFT, buffer, TimeOut);
      break;
   case FFT_SinCosTableBR16:
      ReorderToTime4xSinCosTableBR16( hFFT, buffer, TimeOut);
      break;
   case FFT_FastMathBR16:
      ReorderToTime4xFastMathBR16( hFFT, buffer, TimeOut);
      break;
   case FFT_FastMathBR24:
      ReorderToTime4xFastMathBR24( hFFT, buffer, TimeOut);
      break;
   case FFT_SinCosBRTable:
   default:
      ReorderToTime4xSinCosBRTable( hFFT, buffer, TimeOut);
   };
}

void ReorderToFreq4x(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut, int functionType)
{
   switch(functionType) {
   case FFT_SinCosTableVBR16:
      ReorderToFreq4xSinCosTableVBR16(hFFT, buffer, RealOut, ImagOut);
      break;
   case FFT_SinCosTableBR16:
      ReorderToFreq4xSinCosTableBR16(hFFT, buffer, RealOut, ImagOut);
      break;
   case FFT_FastMathBR16:
      ReorderToFreq4xFastMathBR16(hFFT, buffer, RealOut, ImagOut);
      break;
   case FFT_FastMathBR24:
      ReorderToFreq4xFastMathBR24(hFFT, buffer, RealOut, ImagOut);
      break;
   case FFT_SinCosBRTable:
   default:
      ReorderToFreq4xSinCosBRTable(hFFT, buffer, RealOut, ImagOut);
   };
}

#define REAL_SINCOSBRTABLE
#ifdef REAL_SINCOSBRTABLE

/*
*  Forward FFT routine.  Must call GetFFT(fftlen) first!
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. Real_i = buffer[ h->BitReversed[i] ]
*                                  Imag_i = buffer[ h->BitReversed[i]+1 ] )
*        Input is in normal order.
*
* Output buffer[0] is the DC bin, and output buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void RealFFTf1xSinCosBRTable(fft_type *buffer, FFTParam *h)
{
   fft_type *A,*B;
   fft_type *sptr;
   fft_type *endptr1,*endptr2;
   int *br1,*br2;
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;

   auto ButterfliesPerGroup = h->Points / 2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = buffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      sptr = h->SinTable.get();

      while(A < endptr1)
      {
         sin = *sptr;
         cos = *(sptr + 1);
         endptr2 = B;
         while(A < endptr2)
         {
            v1 = *B * cos + *(B+1) * sin;
            v2 = *B * sin - *(B+1) * cos;
            *B = (*A + v1);
            *(A++) = *(B++) - 2 * v1;
            *B = (*A - v2);
            *(A++) = *(B++) + 2 * v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
         sptr += 2;
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */
   br1 = h->BitReversed.get() + 1;
   br2 = h->BitReversed.get() + h->Points - 1;

   while(br1 < br2)
   {
      sin=h->SinTable[*br1];
      cos=h->SinTable[*br1+1];
      A=buffer+*br1;
      B=buffer+*br2;
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus - cos*HIplus);
      v2 = (cos*HRminus + sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus + v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      br1++;
      br2--;
   }
   /* Handle the center bin (just need a conjugate) */
   A=buffer+*br1+1;
   *A=-*A;
   /* Handle DC bin separately - and ignore the Fs/2 bin
   buffer[0]+=buffer[1];
   buffer[1]=(fft_type)0;*/
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=buffer[0]-buffer[1];
   buffer[0]+=buffer[1];
   buffer[1]=v1;
}


/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf1xSinCosBRTable(fft_type *buffer, FFTParam *h)
{
   fft_type *A,*B;
   fft_type *sptr;
   fft_type *endptr1,*endptr2;
   int *br1;
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;

   auto ButterfliesPerGroup = h->Points / 2;

   /* Massage input to get the input for a real output sequence. */
   A = buffer + 2;
   B = buffer + h->Points * 2 - 2;
   br1 = h->BitReversed.get() + 1;
   while(A < B)
   {
      sin=h->SinTable[*br1];
      cos=h->SinTable[*br1+1];
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus + cos*HIplus);
      v2 = (cos*HRminus - sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus - v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      A+=2;
      B-=2;
      br1++;
   }
   /* Handle center bin (just need conjugate) */
   *(A+1)=-*(A+1);
   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1=0.5f*(buffer[0]+buffer[1]);
   v2=0.5f*(buffer[0]-buffer[1]);
   buffer[0]=v1;
   buffer[1]=v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = buffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      sptr = h->SinTable.get();

      while(A < endptr1)
      {
         sin = *(sptr++);
         cos = *(sptr++);
         endptr2 = B;
         while(A < endptr2)
         {
            v1 = *B * cos - *(B + 1) * sin;
            v2 = *B * sin + *(B + 1) * cos;
            *B = (*A + v1) * (fft_type)0.5;
            *(A++) = *(B++) - v1;
            *B = (*A + v2) * (fft_type)0.5;
            *(A++) = *(B++) - v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToFreq1xSinCosBRTable(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      RealOut[i]=buffer[hFFT->BitReversed[i]  ];
      ImagOut[i]=buffer[hFFT->BitReversed[i]+1];
   }
   RealOut[0] = buffer[0]; // DC component
   ImagOut[0] = 0;
   RealOut[hFFT->Points] = buffer[1]; // Fs/2 component
   ImagOut[hFFT->Points] = 0;
}

void ReorderToTime1xSinCosBRTable(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      TimeOut[i*2  ]=buffer[hFFT->BitReversed[i]  ];
      TimeOut[i*2+1]=buffer[hFFT->BitReversed[i]+1];
   }
}

// 4x processing simd
void RealFFTf4xSinCosBRTable(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A,*B;
   fft_type *sptr;
   __m128 *endptr1,*endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   __m128 HRplus,HRminus,HIplus,HIminus;
   __m128 v1,v2,sin,cos;
   auto ButterfliesPerGroup = h->Points / 2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = &localBuffer[h->Points * 2];

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = &localBuffer[ButterfliesPerGroup * 2];
      sptr = h->SinTable.get();
      while(A < endptr1)
      {
         sin = _mm_set1_ps(*(sptr++));
         cos = _mm_set1_ps(*(sptr++));
         endptr2 = B;
         while(A < endptr2)
         {
            v1 = _mm_add_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B+1), sin));
            v2 = _mm_sub_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B+1), cos));
            *B = _mm_add_ps( *A, v1);
            __m128 temp128 = _mm_set1_ps( 2.0); 
            *(A++) = _mm_sub_ps(*(B++), _mm_mul_ps(temp128, v1));
            *B = _mm_sub_ps(*A,v2);
            *(A++) = _mm_add_ps(*(B++), _mm_mul_ps(temp128, v2));
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index = 1; // h->BitReversed + 1;
   br2Index = h->Points - 1;   //h->BitReversed + h->Points - 1;

   while(br1Index<br2Index)
   {
      br1Value=h->BitReversed[br1Index];
      br2Value=h->BitReversed[br2Index];
      sin=_mm_set1_ps(h->SinTable[br1Value]);
      cos=_mm_set1_ps(h->SinTable[br1Value+1]);
      A=&localBuffer[br1Value];
      B=&localBuffer[br2Value];
      __m128 temp128 = _mm_set1_ps( 2.0);
      HRplus = _mm_add_ps(HRminus = _mm_sub_ps( *A, *B ), _mm_mul_ps(*B, temp128));
      HIplus = _mm_add_ps(HIminus = _mm_sub_ps(*(A+1), *(B+1) ), _mm_mul_ps(*(B+1), temp128));
      v1 = _mm_sub_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_add_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      temp128 = _mm_set1_ps( 0.5);
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), temp128);
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_add_ps(HIminus, v2), temp128);
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      br1Index++;
      br2Index--;
   }
   /* Handle the center bin (just need a conjugate) */
   A=&localBuffer[h->BitReversed[br1Index]+1];
   // negate sse style
   *A=_mm_xor_ps(*A, _mm_set1_ps(-0.f));
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=_mm_sub_ps(localBuffer[0], localBuffer[1]);
   localBuffer[0]=_mm_add_ps(localBuffer[0], localBuffer[1]);
   localBuffer[1]=v1;
}

/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf4xSinCosBRTable(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A,*B;
   fft_type *sptr;
   __m128 *endptr1,*endptr2;
   int br1Index, br1Value;
   __m128 HRplus,HRminus,HIplus,HIminus;
   __m128 v1,v2,sin,cos;

   auto ButterfliesPerGroup = h->Points / 2;

   /* Massage input to get the input for a real output sequence. */
   A = localBuffer + 2;
   B = localBuffer + h->Points * 2 - 2;
   br1Index = 1; //h->BitReversed + 1;
   while(A < B)
   {
      br1Value = h->BitReversed[br1Index];
      sin = _mm_set1_ps(h->SinTable[br1Value]);
      cos = _mm_set1_ps(h->SinTable[br1Value + 1]);
      HRminus = _mm_sub_ps(*A,  *B);
      HRplus = _mm_add_ps(HRminus, _mm_mul_ps(*B,  _mm_set1_ps(2.0)));
      HIminus = _mm_sub_ps( *(A+1), *(B+1));
      HIplus = _mm_add_ps(HIminus,  _mm_mul_ps(*(B+1), _mm_set1_ps(2.0)));
      v1 = _mm_add_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_sub_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), _mm_set1_ps(0.5));
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_sub_ps(HIminus, v2) , _mm_set1_ps(0.5));
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      A=&A[2];
      B=&B[-2];
      br1Index++;
   }
   /* Handle center bin (just need conjugate) */
   // negate sse style
   *(A+1)=_mm_xor_ps(*(A+1), _mm_set1_ps(-0.f));

   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1=_mm_mul_ps(_mm_set1_ps(0.5), _mm_add_ps(localBuffer[0], localBuffer[1]));
   v2=_mm_mul_ps(_mm_set1_ps(0.5), _mm_sub_ps(localBuffer[0], localBuffer[1]));
   localBuffer[0]=v1;
   localBuffer[1]=v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = localBuffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = localBuffer + ButterfliesPerGroup * 2;
      sptr = h->SinTable.get();
      while(A < endptr1)
      {
         sin = _mm_set1_ps(*(sptr++));
         cos = _mm_set1_ps(*(sptr++));
         endptr2 = B;
         while(A < endptr2)
         {
            v1 = _mm_sub_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B + 1), sin));
            v2 = _mm_add_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B + 1), cos));
            *B = _mm_mul_ps( _mm_add_ps(*A, v1), _mm_set1_ps(0.5));
            *(A++) = _mm_sub_ps(*(B++), v1);
            *B = _mm_mul_ps(_mm_add_ps(*A, v2), _mm_set1_ps(0.5));
            *(A++) = _mm_sub_ps(*(B++), v2);
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToFreq4xSinCosBRTable(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   __m128 *localBuffer=(__m128 *)buffer;
   __m128 *localRealOut=(__m128 *)RealOut;
   __m128 *localImagOut=(__m128 *)ImagOut;

   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      int brValue;
      brValue=hFFT->BitReversed[i];
      localRealOut[i]=localBuffer[brValue  ];
      localImagOut[i]=localBuffer[brValue+1];
   }
   localRealOut[0] = localBuffer[0]; // DC component
   localImagOut[0] = _mm_set1_ps(0.0);
   localRealOut[hFFT->Points] = localBuffer[1]; // Fs/2 component
   localImagOut[hFFT->Points] = _mm_set1_ps(0.0);
}

void ReorderToTime4xSinCosBRTable(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   __m128 *localBuffer=(__m128 *)buffer;
   __m128 *localTimeOut=(__m128 *)TimeOut;
   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      int brValue;
      brValue = hFFT->BitReversed[i];
      localTimeOut[i*2  ] = localBuffer[brValue  ];
      localTimeOut[i*2+1] = localBuffer[brValue+1];
   }
}

#endif

#define REAL_SINCOSTABLE_VBR16
#ifdef REAL_SINCOSTABLE_VBR16

/*
*  Forward FFT routine.  Must call GetFFT(fftlen) first!
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. Real_i = buffer[ h->BitReversed[i] ]
*                                  Imag_i = buffer[ h->BitReversed[i]+1 ] )
*        Input is in normal order.
*
* Output buffer[0] is the DC bin, and output buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void RealFFTf1xSinCosTableVBR16(fft_type *buffer, FFTParam *h)
{
   fft_type *A,*B;
   fft_type *endptr1,*endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;
   auto ButterfliesPerGroup = h->Points / 2;
   int pow2BitsMinus1 = h->pow2Bits - 1;
   int sinCosShift = (sSinCosTable.mSinCosTablePow - pow2BitsMinus1);

   endptr1 = buffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         int sinCosLookup = (*SmallVRB[pow2BitsMinus1])(iSinCosIndex)<<sinCosShift;
         sin = sSinCosTable.mSinCosTable[sinCosLookup].mSin;
         cos = sSinCosTable.mSinCosTable[sinCosLookup].mCos;
         iSinCosIndex++;
         endptr2 = B;
         while(A < endptr2)
         {
            v1 = *B*cos + *(B+1)*sin;
            v2 = *B*sin - *(B+1)*cos;
            *B = (*A+v1);
            *(A++) = *(B++) - 2 * v1;
            *B = (*A - v2);
            *(A++) = *(B++) + 2 * v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index = 1;
   br2Index = h->Points - 1;

   while(br1Index < br2Index)
   {
      br1Value=(*SmallVRB[h->pow2Bits])(br1Index);
      br2Value=(*SmallVRB[h->pow2Bits])(br2Index);
      int sinCosIndex=br1Index<<sinCosShift;
      sin=sSinCosTable.mSinCosTable[sinCosIndex].mSin;
      cos=sSinCosTable.mSinCosTable[sinCosIndex].mCos;
      A=&buffer[br1Value];
      B=&buffer[br2Value];
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus - cos*HIplus);
      v2 = (cos*HRminus + sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus + v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      br1Index++;
      br2Index--;
   }
   /* Handle the center bin (just need a conjugate) */
   A=&buffer[(*SmallVRB[h->pow2Bits])(br1Index)+1];

   /* Handle DC bin separately - and ignore the Fs/2 bin
   buffer[0]+=buffer[1];
   buffer[1]=(fft_type)0;*/
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=buffer[0]-buffer[1];
   buffer[0]+=buffer[1];
   buffer[1]=v1;
}

/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf1xSinCosTableVBR16(fft_type *buffer, FFTParam *h)
{
   fft_type *A,*B;
   fft_type *endptr1,*endptr2;
   int br1Index, br1Value;
   int *br1;
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;
   auto ButterfliesPerGroup = h->Points / 2;
   int pow2BitsMinus1 = h->pow2Bits - 1;
   int sinCosShift = (sSinCosTable.mSinCosTablePow - pow2BitsMinus1);

   /* Massage input to get the input for a real output sequence. */
   A = buffer + 2;
   B = buffer + h->Points * 2 - 2;
   br1 = h->BitReversed.get() + 1;
   br1Index = 1; //h->BitReversed + 1;
   while(A < B)
   {
      br1Value = (*SmallVRB[h->pow2Bits])(br1Index);
      int sinCosIndex = br1Index << sinCosShift;
      sin = sSinCosTable.mSinCosTable[sinCosIndex].mSin;
      cos = sSinCosTable.mSinCosTable[sinCosIndex].mCos;
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus + cos*HIplus);
      v2 = (cos*HRminus - sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus - v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      A=&A[2];
      B=&B[-2];
      br1Index++;
   }
   /* Handle center bin (just need conjugate) */
   *(A+1)=-*(A+1);
   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1=0.5f*(buffer[0]+buffer[1]);
   v2=0.5f*(buffer[0]-buffer[1]);
   buffer[0]=v1;
   buffer[1]=v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = buffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         int sinCosLookup = (*SmallVRB[pow2BitsMinus1])(iSinCosIndex) << sinCosShift;
         sin = sSinCosTable.mSinCosTable[sinCosLookup].mSin;
         cos = sSinCosTable.mSinCosTable[sinCosLookup].mCos;
         iSinCosIndex++;
         endptr2 = B;
         while(A < endptr2)
         {
            v1 = *B * cos - *(B + 1) * sin;
            v2 = *B * sin + *(B + 1) * cos;
            *B = (*A + v1) * (fft_type)0.5;
            *(A++) = *(B++) - v1;
            *B = (*A + v2) * (fft_type)0.5;
            *(A++) = *(B++) - v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToTime1xSinCosTableVBR16(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   // Copy the data into the real outputs
   for(size_t i = 0;i < hFFT->Points; i++) {
      int brValue;
      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      TimeOut[i*2  ] = buffer[brValue  ];
      TimeOut[i*2+1] = buffer[brValue+1];
   }
}

void ReorderToFreq1xSinCosTableVBR16(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      int brValue;
      brValue = (*SmallVRB[hFFT->pow2Bits])(i);
      RealOut[i] = buffer[brValue  ];
      ImagOut[i] = buffer[brValue+1];
   }
   RealOut[0] = buffer[0]; // DC component
   ImagOut[0] = 0;
   RealOut[hFFT->Points] = buffer[1]; // Fs/2 component
   ImagOut[hFFT->Points] = 0;
}

// 4x processing simd
void RealFFTf4xSinCosTableVBR16(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A,*B;
   __m128 *endptr1,*endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   __m128 HRplus,HRminus,HIplus,HIminus;
   __m128 v1,v2,sin,cos;
   auto ButterfliesPerGroup = h->Points / 2;
   int pow2BitsMinus1 = h->pow2Bits - 1;
   int sinCosShift = (sSinCosTable.mSinCosTablePow - pow2BitsMinus1);

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = &localBuffer[h->Points * 2];

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = &localBuffer[ButterfliesPerGroup * 2];
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         int sinCosLookup = (*SmallVRB[pow2BitsMinus1])(iSinCosIndex) << sinCosShift;
         sin = _mm_set1_ps(sSinCosTable.mSinCosTable[sinCosLookup].mSin);
         cos = _mm_set1_ps(sSinCosTable.mSinCosTable[sinCosLookup].mCos);
         iSinCosIndex++;
         endptr2 = B;
         while(A < endptr2)
         {
            v1 = _mm_add_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B+1), sin));
            v2 = _mm_sub_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B+1), cos));
            *B = _mm_add_ps( *A, v1);
            __m128 temp128 = _mm_set1_ps( 2.0); 
            *(A++) = _mm_sub_ps(*(B++), _mm_mul_ps(temp128, v1));
            *B = _mm_sub_ps(*A,v2);
            *(A++) = _mm_add_ps(*(B++), _mm_mul_ps(temp128, v2));
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index = 1; // h->BitReversed + 1;
   br2Index = h->Points - 1;   //h->BitReversed + h->Points - 1;

   while(br1Index < br2Index)
   {
      br1Value=(*SmallVRB[h->pow2Bits])(br1Index);
      br2Value=(*SmallVRB[h->pow2Bits])(br2Index);
      int sinCosIndex=br1Index<<sinCosShift;
      sin=_mm_set1_ps(sSinCosTable.mSinCosTable[sinCosIndex].mSin);
      cos=_mm_set1_ps(sSinCosTable.mSinCosTable[sinCosIndex].mCos);
      A=&localBuffer[br1Value];
      B=&localBuffer[br2Value];
      __m128 temp128 = _mm_set1_ps( 2.0);
      HRplus = _mm_add_ps(HRminus = _mm_sub_ps( *A, *B ), _mm_mul_ps(*B, temp128));
      HIplus = _mm_add_ps(HIminus = _mm_sub_ps(*(A+1), *(B+1) ), _mm_mul_ps(*(B+1), temp128));
      v1 = _mm_sub_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_add_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      temp128 = _mm_set1_ps( 0.5);
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), temp128);
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_add_ps(HIminus, v2), temp128);
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      br1Index++;
      br2Index--;
   }
   /* Handle the center bin (just need a conjugate) */
   A=&localBuffer[(*SmallVRB[h->pow2Bits])(br1Index)+1];
   // negate sse style
   *A=_mm_xor_ps(*A, _mm_set1_ps(-0.f));
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=_mm_sub_ps(localBuffer[0], localBuffer[1]);
   localBuffer[0]=_mm_add_ps(localBuffer[0], localBuffer[1]);
   localBuffer[1]=v1;
}

/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf4xSinCosTableVBR16(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A, *B;
   __m128 *endptr1, *endptr2;
   int br1Index, br1Value;
   __m128 HRplus, HRminus, HIplus, HIminus;
   __m128 v1, v2, sin, cos;
   auto ButterfliesPerGroup = h->Points / 2;
   int pow2BitsMinus1 = h->pow2Bits - 1;
   int sinCosShift = (sSinCosTable.mSinCosTablePow - pow2BitsMinus1);

   /* Massage input to get the input for a real output sequence. */
   A = localBuffer + 2;
   B = localBuffer + h->Points * 2 - 2;
   br1Index = 1; //h->BitReversed + 1;
   while(A < B)
   {
      br1Value = (*SmallVRB[h->pow2Bits])(br1Index);
      int sinCosIndex = br1Index << sinCosShift;
      sin = _mm_set1_ps(sSinCosTable.mSinCosTable[sinCosIndex].mSin);
      cos = _mm_set1_ps(sSinCosTable.mSinCosTable[sinCosIndex].mCos);
      HRminus = _mm_sub_ps(*A,  *B);
      HRplus = _mm_add_ps(HRminus, _mm_mul_ps(*B,  _mm_set1_ps(2.0)));
      HIminus = _mm_sub_ps( *(A+1), *(B+1));
      HIplus = _mm_add_ps(HIminus,  _mm_mul_ps(*(B+1), _mm_set1_ps(2.0)));
      v1 = _mm_add_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_sub_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), _mm_set1_ps(0.5));
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_sub_ps(HIminus, v2) , _mm_set1_ps(0.5));
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      A = &A[2];
      B = &B[-2];
      br1Index++;
   }
   /* Handle center bin (just need conjugate) */
   // negate sse style
   *(A+1) = _mm_xor_ps(*(A+1), _mm_set1_ps(-0.f));

   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1 = _mm_mul_ps(_mm_set1_ps(0.5), _mm_add_ps(localBuffer[0], localBuffer[1]));
   v2 = _mm_mul_ps(_mm_set1_ps(0.5), _mm_sub_ps(localBuffer[0], localBuffer[1]));
   localBuffer[0] = v1;
   localBuffer[1] = v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = localBuffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = localBuffer + ButterfliesPerGroup * 2;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         int sinCosLookup = (*SmallVRB[pow2BitsMinus1])(iSinCosIndex) << sinCosShift;
         sin = _mm_set1_ps(sSinCosTable.mSinCosTable[sinCosLookup].mSin);
         cos = _mm_set1_ps(sSinCosTable.mSinCosTable[sinCosLookup].mCos);
         iSinCosIndex++;
         endptr2 = B;
         while(A < endptr2)
         {
            v1 = _mm_sub_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B+1), sin));
            v2 = _mm_add_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B+1), cos));
            *B = _mm_mul_ps( _mm_add_ps(*A, v1), _mm_set1_ps(0.5));
            *(A++) = _mm_sub_ps(*(B++), v1);
            *B = _mm_mul_ps(_mm_add_ps(*A, v2), _mm_set1_ps(0.5));
            *(A++) = _mm_sub_ps(*(B++),v2);
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToTime4xSinCosTableVBR16(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   __m128 *localBuffer = (__m128 *)buffer;
   __m128 *localTimeOut = (__m128 *)TimeOut;
   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      int brValue;
      brValue = (*SmallVRB[hFFT->pow2Bits])(i);
      localTimeOut[i*2  ] = localBuffer[brValue  ];
      localTimeOut[i*2+1] = localBuffer[brValue+1];
   }
}

void ReorderToFreq4xSinCosTableVBR16(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   __m128 *localBuffer = (__m128 *)buffer;
   __m128 *localRealOut = (__m128 *)RealOut;
   __m128 *localImagOut = (__m128 *)ImagOut;

   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      int brValue;
      brValue = (*SmallVRB[hFFT->pow2Bits])(i);
      localRealOut[i] = localBuffer[brValue  ];
      localImagOut[i] = localBuffer[brValue+1];
   }
   localRealOut[0] = localBuffer[0]; // DC component
   localImagOut[0] = _mm_set1_ps(0.0);
   localRealOut[hFFT->Points] = localBuffer[1]; // Fs/2 component
   localImagOut[hFFT->Points] = _mm_set1_ps(0.0);
}
#endif

#define REAL_SINCOSTABLE_BR16
#ifdef REAL_SINCOSTABLE_BR16

/*
*  Forward FFT routine.  Must call GetFFT(fftlen) first!
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. Real_i = buffer[ h->BitReversed[i] ]
*                                  Imag_i = buffer[ h->BitReversed[i]+1 ] )
*        Input is in normal order.
*
* Output buffer[0] is the DC bin, and output buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void RealFFTf1xSinCosTableBR16(fft_type *buffer, FFTParam *h)
{
   fft_type *A,*B;
   fft_type *endptr1, *endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   fft_type HRplus, HRminus, HIplus, HIminus;
   fft_type v1, v2, sin, cos;
   auto ButterfliesPerGroup = h->Points / 2;
   int pow2BitsMinus1 = h->pow2Bits - 1;
   int bitReverseShiftM1 = 17 - h->pow2Bits;
   int bitReverseShift = bitReverseShiftM1 - 1;
   int sinCosShift = (sSinCosTable.mSinCosTablePow - pow2BitsMinus1);

   endptr1 = buffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
//         int sinCosLookup=(*SmallVRB[pow2BitsMinus1])(iSinCosIndex)<<sinCosShift;
         int sinCosLookup = ( ((sSmallRBTable[*((unsigned char *)&iSinCosIndex)]<<8) + (sSmallRBTable[*(((unsigned char *)&iSinCosIndex)+1)]) )>>bitReverseShiftM1)<<sinCosShift;
         sin = sSinCosTable.mSinCosTable[sinCosLookup].mSin;
         cos = sSinCosTable.mSinCosTable[sinCosLookup].mCos;
         iSinCosIndex++;
         endptr2 = B;
         while(A < endptr2)
         {
            v1=*B*cos + *(B+1)*sin;
            v2=*B*sin - *(B+1)*cos;
            *B=(*A+v1);
            *(A++)=*(B++)-2*v1;
            *B=(*A-v2);
            *(A++)=*(B++)+2*v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index = 1;
   br2Index = h->Points - 1;

   while(br1Index < br2Index)
   {
      br1Value=( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]) )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br1Index);
      br2Value=( ((sSmallRBTable[*((unsigned char *)&br2Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br2Index)+1)]) )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br2Index);
      int sinCosIndex = br1Index << sinCosShift;
      sin = sSinCosTable.mSinCosTable[sinCosIndex].mSin;
      cos = sSinCosTable.mSinCosTable[sinCosIndex].mCos;
      A = &buffer[br1Value];
      B = &buffer[br2Value];
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus - cos*HIplus);
      v2 = (cos*HRminus + sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus + v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      br1Index++;
      br2Index--;
   }
   /* Handle the center bin (just need a conjugate) */
//   A=&buffer[(*SmallVRB[h->pow2Bits])(br1Index)+1];
   A=&buffer[( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]) )>>bitReverseShift)+1];

   /* Handle DC bin separately - and ignore the Fs/2 bin
   buffer[0]+=buffer[1];
   buffer[1]=(fft_type)0;*/
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=buffer[0]-buffer[1];
   buffer[0]+=buffer[1];
   buffer[1]=v1;
}

/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf1xSinCosTableBR16(fft_type *buffer, FFTParam *h)
{
   fft_type *A,*B;
   fft_type *endptr1,*endptr2;
   int br1Index;
   fft_type HRplus, HRminus, HIplus, HIminus;
   fft_type v1, v2, sin, cos;
   auto ButterfliesPerGroup = h->Points / 2;
   int pow2BitsMinus1 = h->pow2Bits - 1;
   int sinCosShift = (sSinCosTable.mSinCosTablePow-pow2BitsMinus1);
   int bitReverseShiftM1 = 17 - h->pow2Bits;

   /* Massage input to get the input for a real output sequence. */
   A = buffer + 2;
   B = buffer + h->Points * 2 - 2;
   br1Index = 1; //h->BitReversed + 1;
   while(A < B)
   {
      int sinCosIndex = br1Index << sinCosShift;
      sin = sSinCosTable.mSinCosTable[sinCosIndex].mSin;
      cos = sSinCosTable.mSinCosTable[sinCosIndex].mCos;
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus + cos*HIplus);
      v2 = (cos*HRminus - sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus - v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      A=&A[2];
      B=&B[-2];
      br1Index++;
   }
   /* Handle center bin (just need conjugate) */
   *(A+1)=-*(A+1);
   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1=0.5f*(buffer[0]+buffer[1]);
   v2=0.5f*(buffer[0]-buffer[1]);
   buffer[0]=v1;
   buffer[1]=v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = buffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
//         int sinCosLookup=(*SmallVRB[pow2BitsMinus1])(iSinCosIndex)<<sinCosShift;
         int sinCosLookup=( ((sSmallRBTable[*((unsigned char *)&iSinCosIndex)]<<8) + (sSmallRBTable[*(((unsigned char *)&iSinCosIndex)+1)]) )>>bitReverseShiftM1)<<sinCosShift;
         sin=sSinCosTable.mSinCosTable[sinCosLookup].mSin;
         cos=sSinCosTable.mSinCosTable[sinCosLookup].mCos;
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1=*B*cos - *(B+1)*sin;
            v2=*B*sin + *(B+1)*cos;
            *B=(*A+v1)*(fft_type)0.5;
            *(A++)=*(B++)-v1;
            *B=(*A+v2)*(fft_type)0.5;
            *(A++)=*(B++)-v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToFreq1xSinCosTableBR16(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   int bitReverseShift=16-hFFT->pow2Bits;
   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      int brValue;
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      brValue = ( ((sSmallRBTable[*((unsigned char *)&i)]<<8) + (sSmallRBTable[*(((unsigned char *)&i)+1)]) )>>bitReverseShift);
      RealOut[i] = buffer[brValue  ];
      ImagOut[i] = buffer[brValue+1];
   }
   RealOut[0] = buffer[0]; // DC component
   ImagOut[0] = 0;
   RealOut[hFFT->Points] = buffer[1]; // Fs/2 component
   ImagOut[hFFT->Points] = 0;
}

void ReorderToTime1xSinCosTableBR16(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   int bitReverseShift=16-hFFT->pow2Bits;
   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      int brValue;
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<8) + (sSmallRBTable[*(((unsigned char *)&i)+1)]) )>>bitReverseShift);
      TimeOut[i*2  ] = buffer[brValue  ];
      TimeOut[i*2+1] = buffer[brValue+1];
   }
}

// 4x processing simd
void RealFFTf4xSinCosTableBR16(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A, *B;
   __m128 *endptr1, *endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   __m128 HRplus, HRminus, HIplus, HIminus;
   __m128 v1, v2, sin, cos;
   auto ButterfliesPerGroup = h->Points / 2;
   int pow2BitsMinus1 = h->pow2Bits - 1;
   int sinCosShift = (sSinCosTable.mSinCosTablePow-pow2BitsMinus1);
   int bitReverseShiftM1 = 17 - h->pow2Bits;
   int bitReverseShift = bitReverseShiftM1 - 1;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = &localBuffer[h->Points * 2];

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = &localBuffer[ButterfliesPerGroup * 2];
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
//         int sinCosLookup=(*SmallVRB[pow2BitsMinus1])(iSinCosIndex)<<sinCosShift;
         int sinCosLookup=( ((sSmallRBTable[*((unsigned char *)&iSinCosIndex)]<<8) + (sSmallRBTable[*(((unsigned char *)&iSinCosIndex)+1)]) )>>bitReverseShiftM1)<<sinCosShift;
         sin=_mm_set1_ps(sSinCosTable.mSinCosTable[sinCosLookup].mSin);
         cos=_mm_set1_ps(sSinCosTable.mSinCosTable[sinCosLookup].mCos);
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1 = _mm_add_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B+1), sin));
            v2 = _mm_sub_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B+1), cos));
            *B=_mm_add_ps( *A, v1);
            __m128 temp128 = _mm_set1_ps( 2.0); 
            *(A++)=_mm_sub_ps(*(B++), _mm_mul_ps(temp128, v1));
            *B=_mm_sub_ps(*A,v2);
            *(A++)=_mm_add_ps(*(B++), _mm_mul_ps(temp128, v2));
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index = 1; // h->BitReversed + 1;
   br2Index = h->Points - 1;   //h->BitReversed + h->Points - 1;

   while(br1Index < br2Index)
   {
      br1Value=( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]) )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br1Index);
      br2Value=( ((sSmallRBTable[*((unsigned char *)&br2Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br2Index)+1)]) )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br2Index);
      int sinCosIndex=br1Index<<sinCosShift;
      sin=_mm_set1_ps(sSinCosTable.mSinCosTable[sinCosIndex].mSin);
      cos=_mm_set1_ps(sSinCosTable.mSinCosTable[sinCosIndex].mCos);
      A=&localBuffer[br1Value];
      B=&localBuffer[br2Value];
      __m128 temp128 = _mm_set1_ps( 2.0);
      HRplus = _mm_add_ps(HRminus = _mm_sub_ps( *A, *B ), _mm_mul_ps(*B, temp128));
      HIplus = _mm_add_ps(HIminus = _mm_sub_ps(*(A+1), *(B+1) ), _mm_mul_ps(*(B+1), temp128));
      v1 = _mm_sub_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_add_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      temp128 = _mm_set1_ps( 0.5);
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), temp128);
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_add_ps(HIminus, v2), temp128);
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      br1Index++;
      br2Index--;
   }
   /* Handle the center bin (just need a conjugate) */
//   A=&localBuffer[(*SmallVRB[h->pow2Bits])(br1Index)+1];
   A=&localBuffer[( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]) )>>bitReverseShift)+1];
   // negate sse style
   *A=_mm_xor_ps(*A, _mm_set1_ps(-0.f));
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=_mm_sub_ps(localBuffer[0], localBuffer[1]);
   localBuffer[0]=_mm_add_ps(localBuffer[0], localBuffer[1]);
   localBuffer[1]=v1;
}

/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf4xSinCosTableBR16(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A, *B;
   __m128 *endptr1, *endptr2;
   int br1Index;
   __m128 HRplus, HRminus, HIplus, HIminus;
   __m128 v1, v2, sin, cos;
   auto ButterfliesPerGroup = h->Points / 2;
   int pow2BitsMinus1 = h->pow2Bits - 1;
   int sinCosShift = (sSinCosTable.mSinCosTablePow-pow2BitsMinus1);
   int bitReverseShiftM1 = 17 - h->pow2Bits;

   /* Massage input to get the input for a real output sequence. */
   A = localBuffer + 2;
   B = localBuffer + h->Points * 2 - 2;
   br1Index = 1; //h->BitReversed + 1;
   while(A < B)
   {
      int sinCosIndex = br1Index << sinCosShift;
      sin = _mm_set1_ps(sSinCosTable.mSinCosTable[sinCosIndex].mSin);
      cos = _mm_set1_ps(sSinCosTable.mSinCosTable[sinCosIndex].mCos);
      HRminus = _mm_sub_ps(*A,  *B);
      HRplus = _mm_add_ps(HRminus, _mm_mul_ps(*B,  _mm_set1_ps(2.0)));
      HIminus = _mm_sub_ps( *(A+1), *(B+1));
      HIplus = _mm_add_ps(HIminus,  _mm_mul_ps(*(B+1), _mm_set1_ps(2.0)));
      v1 = _mm_add_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_sub_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), _mm_set1_ps(0.5));
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_sub_ps(HIminus, v2) , _mm_set1_ps(0.5));
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      A=&A[2];
      B=&B[-2];
      br1Index++;
   }
   /* Handle center bin (just need conjugate) */
   // negate sse style
   *(A+1)=_mm_xor_ps(*(A+1), _mm_set1_ps(-0.f));

   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1=_mm_mul_ps(_mm_set1_ps(0.5), _mm_add_ps(localBuffer[0], localBuffer[1]));
   v2=_mm_mul_ps(_mm_set1_ps(0.5), _mm_sub_ps(localBuffer[0], localBuffer[1]));
   localBuffer[0] = v1;
   localBuffer[1] = v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = localBuffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = localBuffer + ButterfliesPerGroup * 2;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
//         int sinCosLookup=(*SmallVRB[pow2BitsMinus1])(iSinCosIndex)<<sinCosShift;
         int sinCosLookup=( ((sSmallRBTable[*((unsigned char *)&iSinCosIndex)]<<8) + (sSmallRBTable[*(((unsigned char *)&iSinCosIndex)+1)]) )>>bitReverseShiftM1)<<sinCosShift;
         sin=_mm_set1_ps(sSinCosTable.mSinCosTable[sinCosLookup].mSin);
         cos=_mm_set1_ps(sSinCosTable.mSinCosTable[sinCosLookup].mCos);
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1=_mm_sub_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B+1), sin));
            v2=_mm_add_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B+1), cos));
            *B=_mm_mul_ps( _mm_add_ps(*A, v1), _mm_set1_ps(0.5));
            *(A++)=_mm_sub_ps(*(B++), v1);
            *B=_mm_mul_ps(_mm_add_ps(*A, v2), _mm_set1_ps(0.5));
            *(A++)=_mm_sub_ps(*(B++),v2);
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToTime4xSinCosTableBR16(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   __m128 *localBuffer = (__m128 *)buffer;
   __m128 *localTimeOut = (__m128 *)TimeOut;
   int bitReverseShift = 16 - hFFT->pow2Bits;

   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      int brValue;
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<8) + (sSmallRBTable[*(((unsigned char *)&i)+1)]) )>>bitReverseShift);
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      localTimeOut[i*2  ] = localBuffer[brValue  ];
      localTimeOut[i*2+1] = localBuffer[brValue+1];
   }
}

void ReorderToFreq4xSinCosTableBR16(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   __m128 *localBuffer = (__m128 *)buffer;
   __m128 *localRealOut = (__m128 *)RealOut;
   __m128 *localImagOut = (__m128 *)ImagOut;
   int bitReverseShift = 16 - hFFT->pow2Bits;

   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      int brValue;
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<8) + (sSmallRBTable[*(((unsigned char *)&i)+1)]) )>>bitReverseShift);
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      localRealOut[i] = localBuffer[brValue  ];
      localImagOut[i] = localBuffer[brValue+1];
   }
   localRealOut[0] = localBuffer[0]; // DC component
   localImagOut[0] = _mm_set1_ps(0.0);
   localRealOut[hFFT->Points] = localBuffer[1]; // Fs/2 component
   localImagOut[hFFT->Points] = _mm_set1_ps(0.0);
}
#endif

#define FAST_MATH_BR24
#ifdef FAST_MATH_BR24

/*
*  Forward FFT routine.  Must call GetFFT(fftlen) first!
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. Real_i = buffer[ h->BitReversed[i] ]
*                                  Imag_i = buffer[ h->BitReversed[i]+1 ] )
*        Input is in normal order.
*
* Output buffer[0] is the DC bin, and output buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void RealFFTf1xFastMathBR24(fft_type *buffer, FFTParam *h)
{
   fft_type *A, *B;
   fft_type *endptr1, *endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   fft_type HRplus, HRminus, HIplus, HIminus;
   fft_type v1, v2, sin, cos;
   fft_type iToRad = 2 * M_PI/(2 * h->Points);
   int bitReverseShift = 24 - h->pow2Bits;
   int bitReverseShiftM1 = bitReverseShift + 1;
   auto ButterfliesPerGroup = h->Points / 2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = buffer + h->Points * 2;

   const v4sf zeroes = {0.0,0.0,0.0,0.0};
   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      int sinCosCalIndex = 0;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         v4sf sin4_2, cos4_2;
         if(!sinCosCalIndex)
         {
            //v4sf vx=zeroes; // <-- If we want to suppress the C4701 warning later.
            v4sf vx;
            for(int i=0;i<4;i++) {
               int brTemp=iSinCosIndex+i;
               vx.m128_f32[i]=( ((sSmallRBTable[*((unsigned char *)&brTemp)]<<16) + (sSmallRBTable[*(((unsigned char *)&brTemp)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&brTemp)+2)] )>>bitReverseShiftM1)*iToRad;
//               vx.m128_f32[i]=((fft_type )SmallRB(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
            }
            //"Warning C4701 potentially uninitialized local variable 'vx' " is OK.
            //vx is initialised component by component, and MSVC doesn't realize.
            sincos_ps(vx, &sin4_2, &cos4_2);
            sin=-sin4_2.m128_f32[0];
            cos=-cos4_2.m128_f32[0];
            sinCosCalIndex++;
         } else {
            sin=-sin4_2.m128_f32[sinCosCalIndex];
            cos=-cos4_2.m128_f32[sinCosCalIndex];
            if(sinCosCalIndex==3)
               sinCosCalIndex=0;
            else
               sinCosCalIndex++;
         }
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1=*B*cos + *(B+1)*sin;
            v2=*B*sin - *(B+1)*cos;
            *B=(*A+v1);
            *(A++)=*(B++)-2*v1;
            *B=(*A-v2);
            *(A++)=*(B++)+2*v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index = 1; // h->BitReversed + 1;
   br2Index = h->Points - 1;   //h->BitReversed + h->Points - 1;

   int sinCosCalIndex = 0;
   while(br1Index < br2Index)
   {
      v4sf sin4_2, cos4_2;
      br1Value=( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<16) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&br1Index)+2)]  )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br1Index);
      br2Value=( ((sSmallRBTable[*((unsigned char *)&br2Index)]<<16) + (sSmallRBTable[*(((unsigned char *)&br2Index)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&br2Index)+2)]  )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br2Index);
      if(!sinCosCalIndex)
      {
         v4sf vx;
         for(int i=0;i<4;i++)
            vx.m128_f32[i]=((float)(br1Index+i))*iToRad;
         sincos_ps(vx, &sin4_2, &cos4_2);
         sin=-sin4_2.m128_f32[0];
         cos=-cos4_2.m128_f32[0];
         sinCosCalIndex++;
      } else {
         sin=-sin4_2.m128_f32[sinCosCalIndex];
         cos=-cos4_2.m128_f32[sinCosCalIndex];
         if(sinCosCalIndex==3)
            sinCosCalIndex=0;
         else
            sinCosCalIndex++;
      }
      A=&buffer[br1Value];
      B=&buffer[br2Value];
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus - cos*HIplus);
      v2 = (cos*HRminus + sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus + v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      br1Index++;
      br2Index--;
   }
   /* Handle the center bin (just need a conjugate) */
//   A=&buffer[(*SmallVRB[h->pow2Bits])(br1Index)+1];
   A=&buffer[( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<16) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&br1Index)+2)] )>>bitReverseShift)+1];

   /* Handle DC bin separately - and ignore the Fs/2 bin
   buffer[0]+=buffer[1];
   buffer[1]=(fft_type)0;*/
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=buffer[0]-buffer[1];
   buffer[0]+=buffer[1];
   buffer[1]=v1;
}

/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf1xFastMathBR24(fft_type *buffer, FFTParam *h)
{
   fft_type *A,*B;
   fft_type *endptr1,*endptr2;
   int br1Index; 
   fft_type HRplus, HRminus, HIplus, HIminus;
   fft_type v1, v2, sin, cos;
   fft_type iToRad = 2 * M_PI / (2 * h->Points);
   int bitReverseShiftM1 = 25 - h->pow2Bits;

   auto ButterfliesPerGroup = h->Points / 2;

   /* Massage input to get the input for a real output sequence. */
   A = buffer + 2;
   B = buffer + h->Points * 2 - 2;
   br1Index = 1; //h->BitReversed + 1;
   int sinCosCalIndex = 0;
   while(A < B)
   {
      v4sf sin4_2, cos4_2;
      if(!sinCosCalIndex)
      {
         v4sf vx;
         for(int i=0;i<4;i++)
            vx.m128_f32[i]=((float)(br1Index+i))*iToRad;
         sincos_ps(vx, &sin4_2, &cos4_2);
         sin=-sin4_2.m128_f32[0];
         cos=-cos4_2.m128_f32[0];
         sinCosCalIndex++;
      } else {
         sin=-sin4_2.m128_f32[sinCosCalIndex];
         cos=-cos4_2.m128_f32[sinCosCalIndex];
         if(sinCosCalIndex==3)
            sinCosCalIndex=0;
         else
            sinCosCalIndex++;
      }
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus + cos*HIplus);
      v2 = (cos*HRminus - sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus - v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      A=&A[2];
      B=&B[-2];
      br1Index++;
   }
   /* Handle center bin (just need conjugate) */
   *(A+1)=-*(A+1);
   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1=0.5f*(buffer[0]+buffer[1]);
   v2=0.5f*(buffer[0]-buffer[1]);
   buffer[0]=v1;
   buffer[1]=v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = buffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      int sinCosCalIndex = 0;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         v4sf sin4_2, cos4_2;
         if(!sinCosCalIndex)
         {
            v4sf vx;
            for(int i=0;i<4;i++) {
               int brTemp=iSinCosIndex+i;
               vx.m128_f32[i]=( ((sSmallRBTable[*((unsigned char *)&brTemp)]<<16) + (sSmallRBTable[*(((unsigned char *)&brTemp)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&brTemp)+2)] )>>bitReverseShiftM1)*iToRad;
//               vx.m128_f32[i]=((fft_type )SmallRB(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
            }
            sincos_ps(vx, &sin4_2, &cos4_2);
            sin=-sin4_2.m128_f32[0];
            cos=-cos4_2.m128_f32[0];
            sinCosCalIndex++;
         } else {
            sin=-sin4_2.m128_f32[sinCosCalIndex];
            cos=-cos4_2.m128_f32[sinCosCalIndex];
            if(sinCosCalIndex==3)
               sinCosCalIndex=0;
            else
               sinCosCalIndex++;
         }
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1=*B*cos - *(B+1)*sin;
            v2=*B*sin + *(B+1)*cos;
            *B=(*A+v1)*(fft_type)0.5;
            *(A++)=*(B++)-v1;
            *B=(*A+v2)*(fft_type)0.5;
            *(A++)=*(B++)-v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToFreq1xFastMathBR24(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   int bitReverseShift = 24 - hFFT->pow2Bits;
   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      int brValue;
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<16) + (sSmallRBTable[*(((unsigned char *)&i)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&i)+2)] )>>bitReverseShift);

      RealOut[i] = buffer[brValue  ];
      ImagOut[i] = buffer[brValue+1];
   }
   RealOut[0] = buffer[0]; // DC component
   ImagOut[0] = 0;
   RealOut[hFFT->Points] = buffer[1]; // Fs/2 component
   ImagOut[hFFT->Points] = 0;
}

void ReorderToTime1xFastMathBR24(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   int bitReverseShift = 24 - hFFT->pow2Bits;
   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      int brValue;
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<16) + (sSmallRBTable[*(((unsigned char *)&i)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&i)+2)] )>>bitReverseShift);
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      TimeOut[i*2  ] = buffer[brValue  ];
      TimeOut[i*2+1] = buffer[brValue+1];
   }
}

void RealFFTf4xFastMathBR24(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A,*B;
   __m128 *endptr1,*endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   __m128 HRplus,HRminus,HIplus,HIminus;
   __m128 v1,v2,sin,cos;
   fft_type iToRad = 2 * M_PI/(2 * h->Points);
   auto ButterfliesPerGroup = h->Points / 2;
   int bitReverseShift = 24 - h->pow2Bits;
   int bitReverseShiftM1 = bitReverseShift + 1;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = &localBuffer[h->Points * 2];

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = &localBuffer[ButterfliesPerGroup * 2];
      int sinCosCalIndex = 0;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         v4sf sin4_2, cos4_2;
         if(!sinCosCalIndex)
         {
            v4sf vx;
            for(int i=0;i<4;i++) {
               int brTemp=iSinCosIndex+i;
               vx.m128_f32[i]=( ((sSmallRBTable[*((unsigned char *)&brTemp)]<<16) + (sSmallRBTable[*(((unsigned char *)&brTemp)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&brTemp)+2)] )>>bitReverseShiftM1)*iToRad;
//               vx.m128_f32[i]=((fft_type )SmallRB(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
            }
            sincos_ps(vx, &sin4_2, &cos4_2);
            sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
            sinCosCalIndex++;
         } else {
            sin=_mm_set1_ps(-sin4_2.m128_f32[sinCosCalIndex]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[sinCosCalIndex]);
            if(sinCosCalIndex==3)
               sinCosCalIndex=0;
            else
               sinCosCalIndex++;
         }
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1 = _mm_add_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B+1), sin));
            v2 = _mm_sub_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B+1), cos));
            *B=_mm_add_ps( *A, v1);
            __m128 temp128 = _mm_set1_ps( 2.0); 
            *(A++)=_mm_sub_ps(*(B++), _mm_mul_ps(temp128, v1));
            *B=_mm_sub_ps(*A,v2);
            *(A++)=_mm_add_ps(*(B++), _mm_mul_ps(temp128, v2));
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index = 1; // h->BitReversed+1;
   br2Index = h->Points - 1;   //h->BitReversed + h->Points - 1;

   int sinCosCalIndex = 0;
   while(br1Index < br2Index)
   {
      v4sf sin4_2, cos4_2;
      br1Value=( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<16) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&br1Index)+2)] )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br1Index);
      br2Value=( ((sSmallRBTable[*((unsigned char *)&br2Index)]<<16) + (sSmallRBTable[*(((unsigned char *)&br2Index)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&br2Index)+2)] )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br2Index);
      if(!sinCosCalIndex)
      {
         v4sf vx;
         for(int i=0;i<4;i++)
            vx.m128_f32[i]=((float)(br1Index+i))*iToRad;
         sincos_ps(vx, &sin4_2, &cos4_2);
         sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
         cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
         sinCosCalIndex++;
      } else {
         sin=_mm_set1_ps(-sin4_2.m128_f32[sinCosCalIndex]);
         cos=_mm_set1_ps(-cos4_2.m128_f32[sinCosCalIndex]);
         if(sinCosCalIndex==3)
            sinCosCalIndex=0;
         else
            sinCosCalIndex++;
      } 
      A=&localBuffer[br1Value];
      B=&localBuffer[br2Value];
      __m128 temp128 = _mm_set1_ps( 2.0);
      HRplus = _mm_add_ps(HRminus = _mm_sub_ps( *A, *B ), _mm_mul_ps(*B, temp128));
      HIplus = _mm_add_ps(HIminus = _mm_sub_ps(*(A+1), *(B+1) ), _mm_mul_ps(*(B+1), temp128));
      v1 = _mm_sub_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_add_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      temp128 = _mm_set1_ps( 0.5);
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), temp128);
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_add_ps(HIminus, v2), temp128);
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      br1Index++;
      br2Index--;
   }
   /* Handle the center bin (just need a conjugate) */
//   A=&localBuffer[(*SmallVRB[h->pow2Bits])(br1Index)+1];
   A=&localBuffer[( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<16) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&br1Index)+2)] )>>bitReverseShift)+1];
   
   // negate sse style
   *A=_mm_xor_ps(*A, _mm_set1_ps(-0.f));
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=_mm_sub_ps(localBuffer[0], localBuffer[1]);
   localBuffer[0]=_mm_add_ps(localBuffer[0], localBuffer[1]);
   localBuffer[1]=v1;
}

/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf4xFastMathBR24(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A,*B;
   __m128 *endptr1,*endptr2;
   int br1Index; 
   __m128 HRplus,HRminus,HIplus,HIminus;
   __m128 v1,v2,sin,cos;
   fft_type iToRad = 2 * M_PI/(2 * h->Points);
   int bitReverseShiftM1 = 25 - h->pow2Bits;
   auto ButterfliesPerGroup = h->Points / 2;

   /* Massage input to get the input for a real output sequence. */
   A = localBuffer + 2;
   B = localBuffer + h->Points * 2 - 2;
   br1Index = 1; //h->BitReversed + 1;
   int sinCosCalIndex = 0;
   while(A < B)
   {
      v4sf sin4_2, cos4_2;
      if(!sinCosCalIndex)
      {
         v4sf vx;
         for(int i=0;i<4;i++)
            vx.m128_f32[i]=((float)(br1Index+i))*iToRad;
         sincos_ps(vx, &sin4_2, &cos4_2);
         sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
         cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
         sinCosCalIndex++;
      } else {
         sin=_mm_set1_ps(-sin4_2.m128_f32[sinCosCalIndex]);
         cos=_mm_set1_ps(-cos4_2.m128_f32[sinCosCalIndex]);
         if(sinCosCalIndex==3)
            sinCosCalIndex=0;
         else
            sinCosCalIndex++;
      }
      HRminus = _mm_sub_ps(*A,  *B);
      HRplus = _mm_add_ps(HRminus, _mm_mul_ps(*B,  _mm_set1_ps(2.0)));
      HIminus = _mm_sub_ps( *(A+1), *(B+1));
      HIplus = _mm_add_ps(HIminus,  _mm_mul_ps(*(B+1), _mm_set1_ps(2.0)));
      v1 = _mm_add_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_sub_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), _mm_set1_ps(0.5));
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_sub_ps(HIminus, v2) , _mm_set1_ps(0.5));
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      A=&A[2];
      B=&B[-2];
      br1Index++;
   }
   /* Handle center bin (just need conjugate) */
   // negate sse style
   *(A+1)=_mm_xor_ps(*(A+1), _mm_set1_ps(-0.f));

   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1=_mm_mul_ps(_mm_set1_ps(0.5), _mm_add_ps(localBuffer[0], localBuffer[1]));
   v2=_mm_mul_ps(_mm_set1_ps(0.5), _mm_sub_ps(localBuffer[0], localBuffer[1]));
   localBuffer[0]=v1;
   localBuffer[1]=v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = localBuffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = localBuffer + ButterfliesPerGroup * 2;
      int sinCosCalIndex = 0;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         v4sf sin4_2, cos4_2;
         if(!sinCosCalIndex)
         {
            v4sf vx;
            for(int i=0;i<4;i++) {
               int brTemp=iSinCosIndex+i;
               vx.m128_f32[i]=( ((sSmallRBTable[*((unsigned char *)&brTemp)]<<16) + (sSmallRBTable[*(((unsigned char *)&brTemp)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&brTemp)+2)] )>>bitReverseShiftM1)*iToRad;
//               vx.m128_f32[i]=((fft_type )SmallRB(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
            }
            sincos_ps(vx, &sin4_2, &cos4_2);
            sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
            sinCosCalIndex++;
         } else {
            sin=_mm_set1_ps(-sin4_2.m128_f32[sinCosCalIndex]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[sinCosCalIndex]);
            if(sinCosCalIndex==3)
               sinCosCalIndex=0;
            else
               sinCosCalIndex++;
         }
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1=_mm_sub_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B+1), sin));
            v2=_mm_add_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B+1), cos));
            *B=_mm_mul_ps( _mm_add_ps(*A, v1), _mm_set1_ps(0.5));
            *(A++)=_mm_sub_ps(*(B++), v1);
            *B=_mm_mul_ps(_mm_add_ps(*A, v2), _mm_set1_ps(0.5));
            *(A++)=_mm_sub_ps(*(B++),v2);
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToFreq4xFastMathBR24(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   __m128 *localBuffer = (__m128 *)buffer;
   __m128 *localRealOut = (__m128 *)RealOut;
   __m128 *localImagOut = (__m128 *)ImagOut;
   int bitReverseShift = 24-hFFT->pow2Bits;


   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      int brValue;
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<16) + (sSmallRBTable[*(((unsigned char *)&i)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&i)+2)] )>>bitReverseShift);
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      localRealOut[i]=localBuffer[brValue  ];
      localImagOut[i]=localBuffer[brValue+1];
   }
   localRealOut[0] = localBuffer[0]; // DC component
   localImagOut[0] = _mm_set1_ps(0.0);
   localRealOut[hFFT->Points] = localBuffer[1]; // Fs/2 component
   localImagOut[hFFT->Points] = _mm_set1_ps(0.0);
}

void ReorderToTime4xFastMathBR24(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   __m128 *localBuffer = (__m128 *)buffer;
   __m128 *localTimeOut = (__m128 *)TimeOut;
   int bitReverseShift = 24-hFFT->pow2Bits;

   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      int brValue;
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<16) + (sSmallRBTable[*(((unsigned char *)&i)+1)]<<8) + sSmallRBTable[*(((unsigned char *)&i)+2)] )>>bitReverseShift);
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      localTimeOut[i*2  ] = localBuffer[brValue  ];
      localTimeOut[i*2+1] = localBuffer[brValue+1];
   }
}

#endif

#define FAST_MATH_BR16
#ifdef FAST_MATH_BR16

/*
*  Forward FFT routine.  Must call GetFFT(fftlen) first!
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. Real_i = buffer[ h->BitReversed[i] ]
*                                  Imag_i = buffer[ h->BitReversed[i]+1 ] )
*        Input is in normal order.
*
* Output buffer[0] is the DC bin, and output buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void RealFFTf1xFastMathBR16(fft_type *buffer, FFTParam *h)
{
   fft_type *A,*B;
   fft_type *endptr1,*endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;
   fft_type iToRad = 2 * M_PI / (2 * h->Points);
   int bitReverseShiftM1 = 17 - h->pow2Bits;
   int bitReverseShift = bitReverseShiftM1 - 1;
   auto ButterfliesPerGroup = h->Points / 2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = buffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      int sinCosCalIndex = 0;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         v4sf sin4_2, cos4_2;
         if(!sinCosCalIndex)
         {
            v4sf vx;
            for(int i=0;i<4;i++) {
               int brTemp=iSinCosIndex+i;
               vx.m128_f32[i]=( ((sSmallRBTable[*((unsigned char *)&brTemp)]<<8) + (sSmallRBTable[*(((unsigned char *)&brTemp)+1)]) )>>bitReverseShiftM1)*iToRad;
//               vx.m128_f32[i]=((fft_type )SmallRB(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
            }
            sincos_ps(vx, &sin4_2, &cos4_2);
            sin=-sin4_2.m128_f32[0];
            cos=-cos4_2.m128_f32[0];
            sinCosCalIndex++;
         } else {
            sin=-sin4_2.m128_f32[sinCosCalIndex];
            cos=-cos4_2.m128_f32[sinCosCalIndex];
            if(sinCosCalIndex==3)
               sinCosCalIndex=0;
            else
               sinCosCalIndex++;
         }
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1=*B*cos + *(B+1)*sin;
            v2=*B*sin - *(B+1)*cos;
            *B=(*A+v1);
            *(A++)=*(B++)-2*v1;
            *B=(*A-v2);
            *(A++)=*(B++)+2*v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index = 1; // h->BitReversed+1;
   br2Index = h->Points - 1;   //h->BitReversed + h->Points - 1;

   int sinCosCalIndex = 0;
   while(br1Index < br2Index)
   {
      v4sf sin4_2, cos4_2;
      br1Value=( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]) )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br1Index);
      br2Value=( ((sSmallRBTable[*((unsigned char *)&br2Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br2Index)+1)]) )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br2Index);
      if(!sinCosCalIndex)
      {
         v4sf vx;
         for(int i = 0; i < 4; i++)
            vx.m128_f32[i]=((float)(br1Index+i))*iToRad;
         sincos_ps(vx, &sin4_2, &cos4_2);
         sin = -sin4_2.m128_f32[0];
         cos=-cos4_2.m128_f32[0];
         sinCosCalIndex++;
      } else {
         sin=-sin4_2.m128_f32[sinCosCalIndex];
         cos=-cos4_2.m128_f32[sinCosCalIndex];
         if(sinCosCalIndex==3)
            sinCosCalIndex=0;
         else
            sinCosCalIndex++;
      }
      A=&buffer[br1Value];
      B=&buffer[br2Value];
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus - cos*HIplus);
      v2 = (cos*HRminus + sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus + v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      br1Index++;
      br2Index--;
   }
   /* Handle the center bin (just need a conjugate) */
//   A=&buffer[(*SmallVRB[h->pow2Bits])(br1Index)+1];
   A=&buffer[( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]) )>>bitReverseShift)+1];

   /* Handle DC bin separately - and ignore the Fs/2 bin
   buffer[0]+=buffer[1];
   buffer[1]=(fft_type)0;*/
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=buffer[0]-buffer[1];
   buffer[0]+=buffer[1];
   buffer[1]=v1;
}

/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf1xFastMathBR16(fft_type *buffer, FFTParam *h)
{
   fft_type *A,*B;
   fft_type *endptr1,*endptr2;
   int br1Index; 
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;
   fft_type iToRad=2 * M_PI / (2 * h->Points);
   int bitReverseShiftM1=17-h->pow2Bits;

   auto ButterfliesPerGroup = h->Points / 2;

   /* Massage input to get the input for a real output sequence. */
   A = buffer + 2;
   B = buffer + h->Points * 2 - 2;
   br1Index = 1; //h->BitReversed + 1;
   int sinCosCalIndex = 0;
   while(A < B)
   {
      v4sf sin4_2, cos4_2;
      if(!sinCosCalIndex)
      {
         v4sf vx;
         for(int i=0;i<4;i++)
            vx.m128_f32[i]=((float)(br1Index+i))*iToRad;
         sincos_ps(vx, &sin4_2, &cos4_2);
         sin=-sin4_2.m128_f32[0];
         cos=-cos4_2.m128_f32[0];
         sinCosCalIndex++;
      } else {
         sin=-sin4_2.m128_f32[sinCosCalIndex];
         cos=-cos4_2.m128_f32[sinCosCalIndex];
         if(sinCosCalIndex==3)
            sinCosCalIndex=0;
         else
            sinCosCalIndex++;
      }
      HRplus = (HRminus = *A     - *B    ) + (*B     * 2);
      HIplus = (HIminus = *(A+1) - *(B+1)) + (*(B+1) * 2);
      v1 = (sin*HRminus + cos*HIplus);
      v2 = (cos*HRminus - sin*HIplus);
      *A = (HRplus  + v1) * (fft_type)0.5;
      *B = *A - v1;
      *(A+1) = (HIminus - v2) * (fft_type)0.5;
      *(B+1) = *(A+1) - HIminus;

      A=&A[2];
      B=&B[-2];
      br1Index++;
   }
   /* Handle center bin (just need conjugate) */
   *(A+1)=-*(A+1);
   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1=0.5f*(buffer[0]+buffer[1]);
   v2=0.5f*(buffer[0]-buffer[1]);
   buffer[0]=v1;
   buffer[1]=v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = buffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = buffer;
      B = buffer + ButterfliesPerGroup * 2;
      int sinCosCalIndex = 0;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         v4sf sin4_2, cos4_2;
         if(!sinCosCalIndex)
         {
            v4sf vx;
            for(int i = 0; i < 4; i++) {
               int brTemp = iSinCosIndex + i;
               vx.m128_f32[i]=( ((sSmallRBTable[*((unsigned char *)&brTemp)]<<8) + (sSmallRBTable[*(((unsigned char *)&brTemp)+1)]) )>>bitReverseShiftM1)*iToRad;
//               vx.m128_f32[i]=((fft_type )SmallRB(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
            }
            sincos_ps(vx, &sin4_2, &cos4_2);
            sin=-sin4_2.m128_f32[0];
            cos=-cos4_2.m128_f32[0];
            sinCosCalIndex++;
         } else {
            sin=-sin4_2.m128_f32[sinCosCalIndex];
            cos=-cos4_2.m128_f32[sinCosCalIndex];
            if(sinCosCalIndex==3)
               sinCosCalIndex=0;
            else
               sinCosCalIndex++;
         }
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1=*B*cos - *(B+1)*sin;
            v2=*B*sin + *(B+1)*cos;
            *B=(*A+v1)*(fft_type)0.5;
            *(A++)=*(B++)-v1;
            *B=(*A+v2)*(fft_type)0.5;
            *(A++)=*(B++)-v2;
         }
         A = B;
         B += ButterfliesPerGroup * 2;
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToFreq1xFastMathBR16(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   int bitReverseShift = 16 - hFFT->pow2Bits;
   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      int brValue;
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<8) + (sSmallRBTable[*(((unsigned char *)&i)+1)]) )>>bitReverseShift);
      RealOut[i] = buffer[brValue  ];
      ImagOut[i] = buffer[brValue+1];
   }
   RealOut[0] = buffer[0]; // DC component
   ImagOut[0] = 0;
   RealOut[hFFT->Points] = buffer[1]; // Fs/2 component
   ImagOut[hFFT->Points] = 0;
}

void ReorderToTime1xFastMathBR16(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   int bitReverseShift=16-hFFT->pow2Bits;
   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      int brValue;
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<8) + (sSmallRBTable[*(((unsigned char *)&i)+1)]) )>>bitReverseShift);
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      TimeOut[i*2  ] = buffer[brValue  ];
      TimeOut[i*2+1] = buffer[brValue+1];
   }
}

void RealFFTf4xFastMathBR16(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer = (__m128 *)buffer;

   __m128 *A,*B;
   __m128 *endptr1, *endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   __m128 HRplus, HRminus, HIplus, HIminus;
   __m128 v1, v2, sin, cos;
   fft_type iToRad = 2 * M_PI/(2 * h->Points);
   auto ButterfliesPerGroup = h->Points / 2;
   int bitReverseShiftM1 = 17 - h->pow2Bits;
   int bitReverseShift = bitReverseShiftM1 - 1;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = &localBuffer[h->Points * 2];

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = &localBuffer[ButterfliesPerGroup * 2];
      int sinCosCalIndex = 0;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         v4sf sin4_2, cos4_2;
         if(!sinCosCalIndex)
         {
            v4sf vx;
            for(int i=0;i<4;i++) {
               int brTemp=iSinCosIndex+i;
               vx.m128_f32[i]=( ((sSmallRBTable[*((unsigned char *)&brTemp)]<<8) + (sSmallRBTable[*(((unsigned char *)&brTemp)+1)]) )>>bitReverseShiftM1)*iToRad;
//               vx.m128_f32[i]=((fft_type )SmallRB(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
            }
            sincos_ps(vx, &sin4_2, &cos4_2);
            sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
            sinCosCalIndex++;
         } else {
            sin=_mm_set1_ps(-sin4_2.m128_f32[sinCosCalIndex]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[sinCosCalIndex]);
            if(sinCosCalIndex==3)
               sinCosCalIndex=0;
            else
               sinCosCalIndex++;
         }
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1 = _mm_add_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B+1), sin));
            v2 = _mm_sub_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B+1), cos));
            *B=_mm_add_ps( *A, v1);
            __m128 temp128 = _mm_set1_ps( 2.0); 
            *(A++)=_mm_sub_ps(*(B++), _mm_mul_ps(temp128, v1));
            *B=_mm_sub_ps(*A,v2);
            *(A++)=_mm_add_ps(*(B++), _mm_mul_ps(temp128, v2));
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index = 1; // h->BitReversed + 1;
   br2Index = h->Points - 1;   //h->BitReversed + h->Points - 1;

   int sinCosCalIndex = 0;
   while(br1Index < br2Index)
   {
      v4sf sin4_2, cos4_2;
      br1Value=( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]) )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br1Index);
      br2Value=( ((sSmallRBTable[*((unsigned char *)&br2Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br2Index)+1)]) )>>bitReverseShift); // (*SmallVRB[h->pow2Bits])(br2Index);
      if(!sinCosCalIndex)
      {
         v4sf vx;
         for(int i = 0; i < 4; i++)
            vx.m128_f32[i] = ((float)(br1Index+i)) * iToRad;
         sincos_ps(vx, &sin4_2, &cos4_2);
         sin = _mm_set1_ps(-sin4_2.m128_f32[0]);
         cos = _mm_set1_ps(-cos4_2.m128_f32[0]);
         sinCosCalIndex++;
      } else {
         sin = _mm_set1_ps(-sin4_2.m128_f32[sinCosCalIndex]);
         cos = _mm_set1_ps(-cos4_2.m128_f32[sinCosCalIndex]);
         if(sinCosCalIndex == 3)
            sinCosCalIndex = 0;
         else
            sinCosCalIndex++;
      } 
      A = &localBuffer[br1Value];
      B = &localBuffer[br2Value];
      __m128 temp128 = _mm_set1_ps( 2.0);
      HRplus = _mm_add_ps(HRminus = _mm_sub_ps( *A, *B ), _mm_mul_ps(*B, temp128));
      HIplus = _mm_add_ps(HIminus = _mm_sub_ps(*(A+1), *(B+1) ), _mm_mul_ps(*(B+1), temp128));
      v1 = _mm_sub_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_add_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      temp128 = _mm_set1_ps( 0.5);
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), temp128);
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_add_ps(HIminus, v2), temp128);
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      br1Index++;
      br2Index--;
   }
   /* Handle the center bin (just need a conjugate) */
//   A=&localBuffer[(*SmallVRB[h->pow2Bits])(br1Index)+1];
   A=&localBuffer[( ((sSmallRBTable[*((unsigned char *)&br1Index)]<<8) + (sSmallRBTable[*(((unsigned char *)&br1Index)+1)]) )>>bitReverseShift)+1];
   
   // negate sse style
   *A=_mm_xor_ps(*A, _mm_set1_ps(-0.f));
   /* Handle DC and Fs/2 bins separately */
   /* Put the Fs/2 value into the imaginary part of the DC bin */
   v1=_mm_sub_ps(localBuffer[0], localBuffer[1]);
   localBuffer[0]=_mm_add_ps(localBuffer[0], localBuffer[1]);
   localBuffer[1]=v1;
}

/* Description: This routine performs an inverse FFT to real data.
*              This code is for floating point data.
*
*  Note: Output is BIT-REVERSED! so you must use the BitReversed to
*        get legible output, (i.e. wave[2*i]   = buffer[ BitReversed[i] ]
*                                  wave[2*i+1] = buffer[ BitReversed[i]+1 ] )
*        Input is in normal order, interleaved (real,imaginary) complex data
*        You must call GetFFT(fftlen) first to initialize some buffers!
*
* Input buffer[0] is the DC bin, and input buffer[1] is the Fs/2 bin
* - this can be done because both values will always be real only
* - this allows us to not have to allocate an extra complex value for the Fs/2 bin
*
*  Note: The scaling on this is done according to the standard FFT definition,
*        so a unit amplitude DC signal will output an amplitude of (N)
*        (Older revisions would progressively scale the input, so the output
*        values would be similar in amplitude to the input values, which is
*        good when using fixed point arithmetic)
*/
void InverseRealFFTf4xFastMathBR16(fft_type *buffer, FFTParam *h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A, *B;
   __m128 *endptr1, *endptr2;
   int br1Index; 
   __m128 HRplus, HRminus, HIplus, HIminus;
   __m128 v1, v2, sin, cos;
   fft_type iToRad = 2 * M_PI/(2 * h->Points);
   int bitReverseShiftM1 = 17 - h->pow2Bits;
   auto ButterfliesPerGroup = h->Points / 2;

   /* Massage input to get the input for a real output sequence. */
   A = localBuffer + 2;
   B = localBuffer + h->Points * 2 - 2;
   br1Index=1; //h->BitReversed+1;
   int sinCosCalIndex=0;
   while(A<B)
   {
      v4sf sin4_2, cos4_2;
      if(!sinCosCalIndex)
      {
         v4sf vx;
         for(int i=0;i<4;i++)
            vx.m128_f32[i]=((float)(br1Index+i))*iToRad;
         sincos_ps(vx, &sin4_2, &cos4_2);
         sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
         cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
         sinCosCalIndex++;
      } else {
         sin=_mm_set1_ps(-sin4_2.m128_f32[sinCosCalIndex]);
         cos=_mm_set1_ps(-cos4_2.m128_f32[sinCosCalIndex]);
         if(sinCosCalIndex==3)
            sinCosCalIndex=0;
         else
            sinCosCalIndex++;
      }
      HRminus = _mm_sub_ps(*A,  *B);
      HRplus = _mm_add_ps(HRminus, _mm_mul_ps(*B,  _mm_set1_ps(2.0)));
      HIminus = _mm_sub_ps( *(A+1), *(B+1));
      HIplus = _mm_add_ps(HIminus,  _mm_mul_ps(*(B+1), _mm_set1_ps(2.0)));
      v1 = _mm_add_ps(_mm_mul_ps(sin, HRminus), _mm_mul_ps(cos, HIplus));
      v2 = _mm_sub_ps(_mm_mul_ps(cos, HRminus), _mm_mul_ps(sin, HIplus));
      *A = _mm_mul_ps(_mm_add_ps(HRplus, v1), _mm_set1_ps(0.5));
      *B = _mm_sub_ps(*A, v1);
      *(A+1) = _mm_mul_ps(_mm_sub_ps(HIminus, v2) , _mm_set1_ps(0.5));
      *(B+1) = _mm_sub_ps(*(A+1), HIminus);

      A=&A[2];
      B=&B[-2];
      br1Index++;
   }
   /* Handle center bin (just need conjugate) */
   // negate sse style
   *(A+1)=_mm_xor_ps(*(A+1), _mm_set1_ps(-0.f));

   /* Handle DC bin separately - this ignores any Fs/2 component
   buffer[1]=buffer[0]=buffer[0]/2;*/
   /* Handle DC and Fs/2 bins specially */
   /* The DC bin is passed in as the real part of the DC complex value */
   /* The Fs/2 bin is passed in as the imaginary part of the DC complex value */
   /* (v1+v2) = buffer[0] == the DC component */
   /* (v1-v2) = buffer[1] == the Fs/2 component */
   v1=_mm_mul_ps(_mm_set1_ps(0.5), _mm_add_ps(localBuffer[0], localBuffer[1]));
   v2=_mm_mul_ps(_mm_set1_ps(0.5), _mm_sub_ps(localBuffer[0], localBuffer[1]));
   localBuffer[0]=v1;
   localBuffer[1]=v2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1 = localBuffer + h->Points * 2;

   while(ButterfliesPerGroup > 0)
   {
      A = localBuffer;
      B = localBuffer + ButterfliesPerGroup * 2;
      int sinCosCalIndex = 0;
      int iSinCosIndex = 0;
      while(A < endptr1)
      {
         v4sf sin4_2, cos4_2;
         if(!sinCosCalIndex)
         {
            v4sf vx;
            for(int i=0;i<4;i++) {
               int brTemp=iSinCosIndex+i;
               vx.m128_f32[i]=( ((sSmallRBTable[*((unsigned char *)&brTemp)]<<8) + (sSmallRBTable[*(((unsigned char *)&brTemp)+1)]) )>>bitReverseShiftM1)*iToRad;
//               vx.m128_f32[i]=((fft_type )SmallRB(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
            }
            sincos_ps(vx, &sin4_2, &cos4_2);
            sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
            sinCosCalIndex++;
         } else {
            sin=_mm_set1_ps(-sin4_2.m128_f32[sinCosCalIndex]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[sinCosCalIndex]);
            if(sinCosCalIndex==3)
               sinCosCalIndex=0;
            else
               sinCosCalIndex++;
         }
         iSinCosIndex++;
         endptr2=B;
         while(A<endptr2)
         {
            v1=_mm_sub_ps( _mm_mul_ps(*B, cos), _mm_mul_ps(*(B+1), sin));
            v2=_mm_add_ps( _mm_mul_ps(*B, sin), _mm_mul_ps(*(B+1), cos));
            *B=_mm_mul_ps( _mm_add_ps(*A, v1), _mm_set1_ps(0.5));
            *(A++)=_mm_sub_ps(*(B++), v1);
            *B=_mm_mul_ps(_mm_add_ps(*A, v2), _mm_set1_ps(0.5));
            *(A++)=_mm_sub_ps(*(B++),v2);
         }
         A = B;
         B = &B[ButterfliesPerGroup * 2];
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToFreq4xFastMathBR16(FFTParam *hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   __m128 *localBuffer=(__m128 *)buffer;
   __m128 *localRealOut=(__m128 *)RealOut;
   __m128 *localImagOut=(__m128 *)ImagOut;
   int bitReverseShift=16-hFFT->pow2Bits;


   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      int brValue;
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<8) + (sSmallRBTable[*(((unsigned char *)&i)+1)]) )>>bitReverseShift);
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      localRealOut[i]=localBuffer[brValue  ];
      localImagOut[i]=localBuffer[brValue+1];
   }
   localRealOut[0] = localBuffer[0]; // DC component
   localImagOut[0] = _mm_set1_ps(0.0);
   localRealOut[hFFT->Points] = localBuffer[1]; // Fs/2 component
   localImagOut[hFFT->Points] = _mm_set1_ps(0.0);
}

void ReorderToTime4xFastMathBR16(FFTParam *hFFT, fft_type *buffer, fft_type *TimeOut)
{
   __m128 *localBuffer=(__m128 *)buffer;
   __m128 *localTimeOut=(__m128 *)TimeOut;
   int bitReverseShift=16-hFFT->pow2Bits;

   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      int brValue;
      brValue=( ((sSmallRBTable[*((unsigned char *)&i)]<<8) + (sSmallRBTable[*(((unsigned char *)&i)+1)]) )>>bitReverseShift);
//      brValue=(*SmallVRB[hFFT->pow2Bits])(i);
      localTimeOut[i*2  ] = localBuffer[brValue  ];
      localTimeOut[i*2+1] = localBuffer[brValue+1];
   }
}

#endif
#endif
