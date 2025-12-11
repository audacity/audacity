/**
Time Frequency Calculator Library
Copyright (C) 2025  Klaus Gram-Hansen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "StftCalculator.h"
#include <assert.h>
#include <iostream>
#include <numeric>
#include <cstring>

using namespace std;

StftCalculator::StftCalculator(unsigned int windowLength,
                  unsigned int transformLength,
                  const TF_DATA_TYPE * pWindow) : nWindowLength(windowLength), nTransformLength(transformLength), vWindow(pWindow, pWindow + windowLength), nSamples(0)
{
   assert(windowLength && (ceil(log2(windowLength)) == floor(log2(windowLength))));
   assert(transformLength && (ceil(log2(transformLength)) == floor(log2(transformLength))));
   hFFT = GetFFT(transformLength);
   
   // Assure out window has mean value "1" not to add any amplification
   TF_DATA_TYPE sum = accumulate(vWindow.begin(), vWindow.end(), decltype(vWindow)::value_type(0));
   for (auto &it : vWindow)
   {
      it *= 2.0/(sum);  // "2" because we attribute all energy to one-sided sectrum
   }
}

StftCalculator::~StftCalculator()
{
}
   
unsigned int  StftCalculator::doTransform(const TF_DATA_TYPE* pSamples, unsigned int _nSamples, unsigned int nValidSamplesBefore, unsigned int nValidSamplesAfter)
{
   unsigned int nPre;
   unsigned int nPost;
   
   assert(_nSamples);
   nSamples = _nSamples;
   // Transform is simple: We just take in samples and prepare our buffer. We choose to do calculations on-the-fly when extracting results
   prepare(nSamples, 0, nPre, nPost);
   
   // Allocate
   size_t len = nPre + nSamples + nPost;
   vTransformBuffer.resize(len);
   TF_DATA_TYPE * ptBuf = &vTransformBuffer[0];
   int ii = nPre - nValidSamplesBefore;

   // Zeropad
   if (ii > 0)
   {
      memset(ptBuf, 0 ,sizeof(TF_DATA_TYPE) * ii);
      ptBuf += ii;
      len -= ii;
   }
   ii = nPost - nValidSamplesAfter;
   if (ii > 0)
   {
      memset(ptBuf + len - ii, 0 ,sizeof(TF_DATA_TYPE) * ii);
      len -= ii;
   }
   // Take in samples
   memcpy(ptBuf, pSamples - nPre + (ptBuf - &vTransformBuffer[0]), sizeof(TF_DATA_TYPE) * len);
   
   return 0;
}
   
void  StftCalculator::prepare(unsigned int nSamples, unsigned int resolution, unsigned int & nPre, unsigned int & nPost)
{
   // Disregard most of the parameters. Simply fill in the returns
   // Assume "zeropoint" of window is at sample N/2
   nPre = nWindowLength >> 1;
   nPost = nPre -1;
}
   
int StftCalculator::extractFrequencySlices(const std::vector<double> & timestamps,
                                      const std::vector<double> & frequencies,
                                      TF_DATA_TYPE *  out,
                                      int    nOut,
                                      bool transpose
                                      ) const
{
   vector<size_t> vFrequencyIndexTable(nTransformLength / 2);
   
   // Inspect the frequencies table
   for (int i = 0; (i < nTransformLength / 2); i++)
   {
      vFrequencyIndexTable[i] = (size_t)(nTransformLength * frequencies[i] + 0.5);
      assert(vFrequencyIndexTable[i] < nTransformLength / 2);
   }
   
   // OK ready to roll. Iterate all timestamps
   vector<TF_DATA_TYPE> workBuf(nTransformLength);
   size_t dout = transpose ? 1 : timestamps.size();
   size_t delta = 0;
   for (auto itertime = timestamps.begin(); itertime != timestamps.end(); itertime++)
   {
      if (!transpose)
      {
         delta = (itertime - timestamps.begin());
      }
      assert(*itertime <= nSamples - 1 && *itertime >= 0);
      const TF_DATA_TYPE * ptSrc = &vTransformBuffer[(size_t)(*itertime + 0.5)];
      if (nTransformLength > nWindowLength)
      {
         memset(&workBuf[nWindowLength], 0, (nTransformLength - nWindowLength) * sizeof(TF_DATA_TYPE));
      }
      memcpy(&workBuf[0], ptSrc, nWindowLength * sizeof(TF_DATA_TYPE));
      
      // Do the FFT.  Note that useBuffer is multiplied by the window,
      // and the window is initialized with leading and trailing zeroes
      // when there is padding.  Therefore we did not need to reinitialize
      // the part of useBuffer in the padding zones.
      size_t i;
      assert(nWindowLength <= 2 * hFFT->Points);
      for(i = 0; i < nWindowLength; i++)
         workBuf[i] *= vWindow[i];
      for( ; i < (hFFT->Points * 2); i++)
         workBuf[i] = 0; // zero pad as needed
      RealFFTf(&workBuf[0], hFFT.get());
      // Handle the (real-only) DC

      for(i = 0; i < hFFT->Points; i++) {
         const size_t inx = vFrequencyIndexTable[i];
         assert (delta < nOut);
         if (inx == 0)
         {
            out[delta] = 0.5 * workBuf[0] * workBuf[0]; // Factor 0.5 bcs DC is not two-sided
         }
         else
         {
            const int index = hFFT->BitReversed[inx];
            const float re = workBuf[index], im = workBuf[index + 1];
            out[delta] = re * re + im * im;
         }
         delta += dout;
      }
   }
   return (int)(delta - dout + 1);
}
   
   



