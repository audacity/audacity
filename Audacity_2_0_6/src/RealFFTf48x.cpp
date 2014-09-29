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
#include "Experimental.h"
#ifdef EXPERIMENTAL_EQ_SSE_THREADED


#ifndef USE_SSE2
#define	USE_SSE2
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
#include <xmmintrin.h>


#ifndef M_PI
#define	M_PI		3.14159265358979323846  /* pi */
#endif

unsigned char smallReverseBitsTable[256];

int tableMask=0;
bool useBitReverseTable=false;
bool useSinCosTable=false;

void TableUsage(int iMask)
{
   tableMask=iMask;
   useBitReverseTable=((iMask & 1)!=0);
   useSinCosTable=((iMask&2)!=0);
}

// note !!! number of bits must be between 9-16
int SmallReverseBits(int bits, int numberBits)
{
   return (smallReverseBitsTable[*((unsigned char *)&bits)]<<(numberBits-8))+(smallReverseBitsTable[*(((unsigned char *)&bits)+1)]>>(16-numberBits));
}


/*
*  Initialize the Sine table and Twiddle pointers (bit-reversed pointers)
*  for the FFT routine.
*/
HFFT InitializeFFT1x(int WXUNUSED( fftlen ) )
{
   int i;
   //int temp;
   //int mask;
   //HFFT h;


   // this needs to move out but ehh... Andrew Hallendorff
   for(i=0;i<256;i++) {
      smallReverseBitsTable[i]=0;
      for(int maskLow=1, maskHigh=128;maskLow<256;maskLow<<=1,maskHigh>>=1)
         if(i&maskLow)
            smallReverseBitsTable[i]|=maskHigh;
   }

   return NULL;
}

/*
*  Free up the memory allotted for Sin table and Twiddle Pointers
*/
void EndFFT1x(HFFT h)
{
   if(h->Points>0) {
      free(h->BitReversed);
      free(h->SinTable);
   }
   h->Points=0;
   free(h);
}

#define MAX_HFFT 10
static HFFT hFFTArray[MAX_HFFT] = { NULL };
static int nFFTLockCount[MAX_HFFT] = { 0 };

/* Get a handle to the FFT tables of the desired length */
/* This version keeps common tables rather than allocating a new table every time */
HFFT GetFFT1x(int fftlen)
{
   int h,n = fftlen/2;
   for(h=0; (h<MAX_HFFT) && (hFFTArray[h] != NULL) && (n != hFFTArray[h]->Points); h++);
   if(h<MAX_HFFT) {
      if(hFFTArray[h] == NULL) {
         hFFTArray[h] = InitializeFFT(fftlen);
         nFFTLockCount[h] = 0;
      }
      nFFTLockCount[h]++;
      return hFFTArray[h];
   } else {
      // All buffers used, so fall back to allocating a new set of tables
      return InitializeFFT(fftlen);;
   }
}

/* Release a previously requested handle to the FFT tables */
void ReleaseFFT1x(HFFT hFFT)
{
   int h;
   for(h=0; (h<MAX_HFFT) && (hFFTArray[h] != hFFT); h++);
   if(h<MAX_HFFT) {
      nFFTLockCount[h]--;
   } else {
      EndFFT(hFFT);
   }
}

/* Deallocate any unused FFT tables */
void CleanupFFT1x()
{
   int h;
   for(h=0; (h<MAX_HFFT); h++) {
      if((nFFTLockCount[h] <= 0) && (hFFTArray[h] != NULL)) {
         EndFFT(hFFTArray[h]);
         hFFTArray[h] = NULL;
      }
   }
}

/*
*  Forward FFT routine.  Must call InitializeFFT(fftlen) first!
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
void RealFFTf1x(fft_type *buffer,HFFT h)
{
   fft_type *A,*B;
   fft_type *sptr;
   fft_type *endptr1,*endptr2;
   int *br1,*br2;
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;

   int ButterfliesPerGroup=h->Points/2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1=buffer+h->Points*2;

   while(ButterfliesPerGroup>0)
   {
      A=buffer;
      B=buffer+ButterfliesPerGroup*2;
      sptr=h->SinTable;

      while(A<endptr1)
      {
         sin=*sptr;
         cos=*(sptr+1);
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
         A=B;
         B+=ButterfliesPerGroup*2;
         sptr+=2;
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */
   br1=h->BitReversed+1;
   br2=h->BitReversed+h->Points-1;

   while(br1<br2)
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
*        You must call InitializeFFT(fftlen) first to initialize some buffers!
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
void InverseRealFFTf1x(fft_type *buffer,HFFT h)
{
   fft_type *A,*B;
   fft_type *sptr;
   fft_type *endptr1,*endptr2;
   int *br1;
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;

   int ButterfliesPerGroup=h->Points/2;

   /* Massage input to get the input for a real output sequence. */
   A=buffer+2;
   B=buffer+h->Points*2-2;
   br1=h->BitReversed+1;
   while(A<B)
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

   endptr1=buffer+h->Points*2;

   while(ButterfliesPerGroup>0)
   {
      A=buffer;
      B=buffer+ButterfliesPerGroup*2;
      sptr=h->SinTable;

      while(A<endptr1)
      {
         sin=*(sptr++);
         cos=*(sptr++);
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
         A=B;
         B+=ButterfliesPerGroup*2;
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToFreq1x(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   // Copy the data into the real and imaginary outputs
   for(int i=1;i<hFFT->Points;i++) {
      RealOut[i]=buffer[hFFT->BitReversed[i]  ];
      ImagOut[i]=buffer[hFFT->BitReversed[i]+1];
   }
   RealOut[0] = buffer[0]; // DC component
   ImagOut[0] = 0;
   RealOut[hFFT->Points] = buffer[1]; // Fs/2 component
   ImagOut[hFFT->Points] = 0;
}

void ReorderToTime1x(HFFT hFFT, fft_type *buffer, fft_type *TimeOut)
{
   // Copy the data into the real outputs
   for(int i=0;i<hFFT->Points;i++) {
      TimeOut[i*2  ]=buffer[hFFT->BitReversed[i]  ];
      TimeOut[i*2+1]=buffer[hFFT->BitReversed[i]+1];
   }
}


// 4x processing simd

void RealFFTf4x(fft_type *buffer,HFFT h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A,*B;
   fft_type *sptr;
   __m128 *endptr1,*endptr2;
   int br1Index, br2Index;
   int br1Value, br2Value;
   __m128 HRplus,HRminus,HIplus,HIminus;
   __m128 v1,v2,sin,cos;
   fft_type iToRad=2*M_PI/(2*h->Points);

   int ButterfliesPerGroup=h->Points/2;

   /*
   *  Butterfly:
   *     Ain-----Aout
   *         \ /
   *         / \
   *     Bin-----Bout
   */

   endptr1=&localBuffer[h->Points*2];

   while(ButterfliesPerGroup>0)
   {
      A=localBuffer;
      B=&localBuffer[ButterfliesPerGroup*2];
      sptr=h->SinTable;
      int iSinCosIndex=0;
      int iSinCosCalIndex=0;
      while(A<endptr1)
      {
         v4sfu sin4_2, cos4_2;
         if(useSinCosTable) {
            sin=_mm_set1_ps(*(sptr++));
            cos=_mm_set1_ps(*(sptr++));
         } else {
            if(!iSinCosCalIndex)
            {
               v4sfu vx;
               for(int i=0;i<4;i++)
                  vx.m128_f32[i]=((fft_type )SmallReverseBits(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
               sincos_ps(&vx, &sin4_2, &cos4_2);
               sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
               cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
               iSinCosCalIndex++;
            } else {
               sin=_mm_set1_ps(-sin4_2.m128_f32[iSinCosCalIndex]);
               cos=_mm_set1_ps(-cos4_2.m128_f32[iSinCosCalIndex]);
               if(iSinCosCalIndex==3)
                  iSinCosCalIndex=0;
               else
                  iSinCosCalIndex++;
            }
            iSinCosIndex++;
         }
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
         A=B;
         B=&B[ButterfliesPerGroup*2];
      }
      ButterfliesPerGroup >>= 1;
   }
   /* Massage output to get the output for a real input sequence. */

   br1Index=1; // h->BitReversed+1;
   br2Index=h->Points-1;   //h->BitReversed+h->Points-1;

   int iSinCosCalIndex=0;
   while(br1Index<br2Index)
   {
      v4sfu sin4_2, cos4_2;
      if(useBitReverseTable) {
         br1Value=h->BitReversed[br1Index];
         br2Value=h->BitReversed[br2Index];
      } else {
         br1Value=SmallReverseBits(br1Index,h->pow2Bits);
         br2Value=SmallReverseBits(br2Index,h->pow2Bits);
      }
      if(useSinCosTable) {
         sin=_mm_set1_ps(h->SinTable[br1Value]);
         cos=_mm_set1_ps(h->SinTable[br1Value+1]);
      } else {
         if(!iSinCosCalIndex)
         {
            v4sfu vx;
            for(int i=0;i<4;i++)
               vx.m128_f32[i]=((float)(br1Index+i))*iToRad;
            sincos_ps(&vx, &sin4_2, &cos4_2);
            sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
            iSinCosCalIndex++;
         } else {
            sin=_mm_set1_ps(-sin4_2.m128_f32[iSinCosCalIndex]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[iSinCosCalIndex]);
            if(iSinCosCalIndex==3)
               iSinCosCalIndex=0;
            else
               iSinCosCalIndex++;
         }
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
   if(useBitReverseTable)
      A=&localBuffer[h->BitReversed[br1Index]+1];
   else
      A=&localBuffer[SmallReverseBits(br1Index,h->pow2Bits)+1];
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
*        You must call InitializeFFT(fftlen) first to initialize some buffers!
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
void InverseRealFFTf4x(fft_type *buffer,HFFT h)
{

   __m128 *localBuffer=(__m128 *)buffer;

   __m128 *A,*B;
   fft_type *sptr;
   __m128 *endptr1,*endptr2;
   int br1Index, br1Value;
   __m128 HRplus,HRminus,HIplus,HIminus;
   __m128 v1,v2,sin,cos;
   fft_type iToRad=2*M_PI/(2*h->Points);


   int ButterfliesPerGroup=h->Points/2;

   /* Massage input to get the input for a real output sequence. */
   A=localBuffer+2;
   B=localBuffer+h->Points*2-2;
   br1Index=1; //h->BitReversed+1;
   int iSinCosCalIndex=0;
   while(A<B)
   {
      v4sfu sin4_2, cos4_2;
      if(useBitReverseTable) {
         br1Value=h->BitReversed[br1Index];
      } else {
         br1Value=SmallReverseBits(br1Index,h->pow2Bits);
      }
      if(useSinCosTable) {
         sin=_mm_set1_ps(h->SinTable[br1Value]);
         cos=_mm_set1_ps(h->SinTable[br1Value+1]);
      } else {
         if(!iSinCosCalIndex)
         {
            v4sfu vx;
            for(int i=0;i<4;i++)
               vx.m128_f32[i]=((float)(br1Index+i))*iToRad;
            sincos_ps(&vx, &sin4_2, &cos4_2);
            sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
            iSinCosCalIndex++;
         } else {
            sin=_mm_set1_ps(-sin4_2.m128_f32[iSinCosCalIndex]);
            cos=_mm_set1_ps(-cos4_2.m128_f32[iSinCosCalIndex]);
            if(iSinCosCalIndex==3)
               iSinCosCalIndex=0;
            else
               iSinCosCalIndex++;
         }
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

   endptr1=localBuffer+h->Points*2;

   while(ButterfliesPerGroup>0)
   {
      A=localBuffer;
      B=localBuffer+ButterfliesPerGroup*2;
      sptr=h->SinTable;
      int iSinCosIndex=0;
      int iSinCosCalIndex=0;
      while(A<endptr1)
      {
         v4sfu sin4_2, cos4_2;
         if(useSinCosTable) {
            sin=_mm_set1_ps(*(sptr++));
            cos=_mm_set1_ps(*(sptr++));
         } else {
            if(!iSinCosCalIndex)
            {
               v4sfu vx;
               for(int i=0;i<4;i++)
                  vx.m128_f32[i]=((fft_type )SmallReverseBits(iSinCosIndex+i,h->pow2Bits-1))*iToRad;
               sincos_ps(&vx, &sin4_2, &cos4_2);
               sin=_mm_set1_ps(-sin4_2.m128_f32[0]);
               cos=_mm_set1_ps(-cos4_2.m128_f32[0]);
               iSinCosCalIndex++;
            } else {
               sin=_mm_set1_ps(-sin4_2.m128_f32[iSinCosCalIndex]);
               cos=_mm_set1_ps(-cos4_2.m128_f32[iSinCosCalIndex]);
               if(iSinCosCalIndex==3)
                  iSinCosCalIndex=0;
               else
                  iSinCosCalIndex++;
            }
            iSinCosIndex++;
         }
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
         A=B;
         B=&B[ButterfliesPerGroup*2];
      }
      ButterfliesPerGroup >>= 1;
   }
}

void ReorderToFreq4x(HFFT hFFT, fft_type *buffer, fft_type *RealOut, fft_type *ImagOut)
{
   __m128 *localBuffer=(__m128 *)buffer;
   __m128 *localRealOut=(__m128 *)RealOut;
   __m128 *localImagOut=(__m128 *)ImagOut;

   // Copy the data into the real and imaginary outputs
   for(int i=1;i<hFFT->Points;i++) {
      int brValue;
      if(useBitReverseTable)
         brValue=hFFT->BitReversed[i];
      else
         brValue=SmallReverseBits(i,hFFT->pow2Bits);
      localRealOut[i]=localBuffer[brValue  ];
      localImagOut[i]=localBuffer[brValue+1];
   }
   localRealOut[0] = localBuffer[0]; // DC component
   localImagOut[0] = _mm_set1_ps(0.0);
   localRealOut[hFFT->Points] = localBuffer[1]; // Fs/2 component
   localImagOut[hFFT->Points] = _mm_set1_ps(0.0);
}

void ReorderToTime4x(HFFT hFFT, fft_type *buffer, fft_type *TimeOut)
{
   __m128 *localBuffer=(__m128 *)buffer;
   __m128 *localTimeOut=(__m128 *)TimeOut;
   // Copy the data into the real outputs
   for(int i=0;i<hFFT->Points;i++) {
      int brValue;
      if(useBitReverseTable)
         brValue=hFFT->BitReversed[i];
      else
         brValue=SmallReverseBits(i,hFFT->pow2Bits);
      localTimeOut[i*2  ]=localBuffer[brValue  ];
      localTimeOut[i*2+1]=localBuffer[brValue+1];
   }
}

#endif