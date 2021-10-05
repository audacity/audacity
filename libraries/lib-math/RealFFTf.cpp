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

#include "RealFFTf.h"

#include <vector>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include <wx/thread.h>

#ifndef M_PI
#define	M_PI		3.14159265358979323846  /* pi */
#endif

/*
*  Initialize the Sine table and Twiddle pointers (bit-reversed pointers)
*  for the FFT routine.
*/
HFFT InitializeFFT(size_t fftlen)
{
   int temp;
   HFFT h{ safenew FFTParam };

   /*
   *  FFT size is only half the number of data points
   *  The full FFT output can be reconstructed from this FFT's output.
   *  (This optimization can be made since the data is real.)
   */
   h->Points = fftlen / 2;

   h->SinTable.reinit(2*h->Points);

   h->BitReversed.reinit(h->Points);

   for(size_t i = 0; i < h->Points; i++)
   {
      temp = 0;
      for(size_t mask = h->Points / 2; mask > 0; mask >>= 1)
         temp = (temp >> 1) + (i & mask ? h->Points : 0);

      h->BitReversed[i] = temp;
   }

   for(size_t i = 0; i < h->Points; i++)
   {
      h->SinTable[h->BitReversed[i]  ]=(fft_type)-sin(2*M_PI*i/(2*h->Points));
      h->SinTable[h->BitReversed[i]+1]=(fft_type)-cos(2*M_PI*i/(2*h->Points));
   }

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
   // NEW SSE FFT routines work on live data
   for(size_t i = 0; i < 32; i++)
      if((1 << i) & fftlen)
         h->pow2Bits = i;
#endif

   return h;
}

enum : size_t { MAX_HFFT = 10 };

// Maintain a pool:
static std::vector< std::unique_ptr<FFTParam> > hFFTArray(MAX_HFFT);
wxCriticalSection getFFTMutex;

/* Get a handle to the FFT tables of the desired length */
/* This version keeps common tables rather than allocating a NEW table every time */
HFFT GetFFT(size_t fftlen)
{
   // To do:  smarter policy about when to retain in the pool and when to
   // allocate a unique instance.

   wxCriticalSectionLocker locker{ getFFTMutex };
   
   size_t h = 0;
   auto n = fftlen/2;
   auto size = hFFTArray.size();
   for(;
       (h < size) && hFFTArray[h] && (n != hFFTArray[h]->Points);
       h++)
      ;
   if(h < size) {
      if(hFFTArray[h] == NULL) {
         hFFTArray[h].reset( InitializeFFT(fftlen).release() );
      }
      return HFFT{ hFFTArray[h].get() };
   } else {
      // All buffers used, so fall back to allocating a NEW set of tables
      return InitializeFFT(fftlen);
   }
}

/* Release a previously requested handle to the FFT tables */
void FFTDeleter::operator() (FFTParam *hFFT) const
{
   wxCriticalSectionLocker locker{ getFFTMutex };

   auto it = hFFTArray.begin(), end = hFFTArray.end();
   while (it != end && it->get() != hFFT)
      ++it;
   if ( it != end )
      ;
   else
      delete hFFT;
}

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
void RealFFTf(fft_type *buffer, const FFTParam *h)
{
   fft_type *A,*B;
   const fft_type *sptr;
   const fft_type *endptr1,*endptr2;
   const int *br1,*br2;
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;

   auto ButterfliesPerGroup = h->Points/2;

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
         cos = *(sptr+1);
         endptr2 = B;
         while(A < endptr2)
         {
            v1 = *B * cos + *(B + 1) * sin;
            v2 = *B * sin - *(B + 1) * cos;
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
void InverseRealFFTf(fft_type *buffer, const FFTParam *h)
{
   fft_type *A,*B;
   const fft_type *sptr;
   const fft_type *endptr1,*endptr2;
   const int *br1;
   fft_type HRplus,HRminus,HIplus,HIminus;
   fft_type v1,v2,sin,cos;

   auto ButterfliesPerGroup = h->Points / 2;

   /* Massage input to get the input for a real output sequence. */
   A = buffer + 2;
   B = buffer + h->Points * 2 - 2;
   br1 = h->BitReversed.get() + 1;
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

void ReorderToFreq(const FFTParam *hFFT, const fft_type *buffer,
		   fft_type *RealOut, fft_type *ImagOut)
{
   // Copy the data into the real and imaginary outputs
   for(size_t i = 1; i < hFFT->Points; i++) {
      RealOut[i] = buffer[hFFT->BitReversed[i]  ];
      ImagOut[i] = buffer[hFFT->BitReversed[i]+1];
   }
   RealOut[0] = buffer[0]; // DC component
   ImagOut[0] = 0;
   RealOut[hFFT->Points] = buffer[1]; // Fs/2 component
   ImagOut[hFFT->Points] = 0;
}

void ReorderToTime(const FFTParam *hFFT, const fft_type *buffer, fft_type *TimeOut)
{
   // Copy the data into the real outputs
   for(size_t i = 0; i < hFFT->Points; i++) {
      TimeOut[i*2  ]=buffer[hFFT->BitReversed[i]  ];
      TimeOut[i*2+1]=buffer[hFFT->BitReversed[i]+1];
   }
}
