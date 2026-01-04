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
#include <iostream>
#include <iomanip>
#include <chrono>
#include <random>
#include <assert.h>

#include "../src/StftCalculator.h"
#include "../src/WaveletCalculator.h"
#include "../src/DyadicFilter.h"
#include "../src/ConfinedGaussianWaveletVoice.h"
using namespace std;
using namespace chrono;
int main() {
    std::cout << "Hello World!" << std::endl;

#define LEN 100
#define FLEN 256
#define DELTA 12
#define OUT_LEN (LEN*FLEN)
   float spike[LEN];
   float out[OUT_LEN];
   float out2[OUT_LEN];
   memset(spike, 0, sizeof(float) * (LEN));
   memset(out, 0, sizeof(float) * (OUT_LEN));
   for (int i = 0; i < 0*LEN; i++)
   {
      switch (i % 4)
      {
         case 0:
            spike[i] = 0;
            break;
         case 1:
            spike[i] = 1;
            break;
         case 2:
            spike[i] = 0;
            break;
         case 3:
            spike[i] = -1;
            break;
      }
   }
   spike[LEN/2] = 10.0;

   /**==================**/
   DyadicFilter * pFilter;
#define OCTAVES 4
   unsigned int padding = DyadicFilter::getExtraSamples(OCTAVES - 1);
   cout << "Extra samples for filter with ##OCTAVES octaves : " << padding << endl;
   pair<TF_DATA_TYPE *, unsigned int> rval;
   pFilter = new DyadicFilter(OCTAVES);
   pFilter->doAllocation(LEN, padding + 33, padding);
   pFilter->filterSamples(spike, LEN, 0, 0);
   for (int octave = 0; octave < OCTAVES; octave ++)
   {
      rval = pFilter->getSamples(octave,-16);
      cout << "Octave " << octave << ". Samples including tail " << rval.second << endl << "-------------------------------------------------" << endl;
      for (int i = 0; i < rval.second; i++)
      {
         cout << rval.first[i] << ", ";
      }
      cout << endl << endl;
   }
   
   delete pFilter;
   pFilter = new DyadicFilter(10);
   pFilter->doAllocation(LEN, 20000, 21000);
   pFilter->filterSamples(spike, LEN, 0, 0);
   ConfinedGaussianWaveletVoice wavelet(0.001, 0.0005, 0.002, 8.7, 0, pFilter);
   wavelet.allocateResult(LEN,0);
   wavelet.transform();
   wavelet.dump();
   vector<double> timestamps(LEN);
   std::iota(timestamps.begin(), timestamps.end(), 0);
   wavelet.executeSequence(FLEN, LEN, out, timestamps.cbegin(), timestamps.cend(), true);
   wavelet.executeSequence(FLEN, LEN, out, timestamps.cbegin(), timestamps.cend(), false);
   ConfinedGaussianWaveletVoice wavelet2(0.5, 0.45, 0.55, 8.7, 0, pFilter);
   wavelet2.allocateResult(LEN,0);
   wavelet2.transform();
   wavelet2.dump();
   delete pFilter;
   pFilter = NULL;
   
   std::random_device dev;
   std::mt19937 rng(dev());
   std::uniform_int_distribution<std::mt19937::result_type> dist99(1,99); // distribution in range [1, 6]
   std::uniform_int_distribution<std::mt19937::result_type> dist999(1,999); // distribution in range [1, 6]

   float window[64];
   for (int i = 0; i < 64;i++)
   {
      window[i] = 1;
   }
   ITimeFrequencyCalculator * stft = new StftCalculator(4, 2 * FLEN, window);
   stft->doTransform(spike, LEN, 0, 0);
   stft->extractFrequencySlices(0, 1.0, LEN, 0.1, 0.3/FLEN, FLEN, out, OUT_LEN, false);
   delete stft;
   cout << std::fixed << std::setprecision(2);
   std::cout << std::endl;
   double total1 = 0;
   for (int finx = FLEN; finx--; )
   {
      for (int tinx = LEN/2 - DELTA; tinx < LEN/2 + DELTA; tinx++)
      {
         std::cout << out[finx*LEN + tinx] << ", ";
         total1 += out[finx*LEN + tinx];
      }
      std::cout << std::endl;
   }


   float overlap = dist99(rng);
   int octaves = 10;
   double Q = 8.651358596;
   double fmax = dist99(rng)/99.0 * 0.25 + 0.25;
#define TIMING(x) high_resolution_clock::time_point x = high_resolution_clock::now()
   
   TIMING(t1);
   ITimeFrequencyCalculator * wc = new WaveletCalculator(octaves, 0.4, Q, overlap);

   TIMING(t2);
   unsigned int nb = wc->doTransform(spike, LEN, 0, 0);

   TIMING(t3);
   wc->extractFrequencySlices(0, 1.0, LEN, 0.0, 0.5/FLEN, FLEN, out, OUT_LEN, false);

   TIMING(t4);

   cout << "Transform of " << LEN << " samples with overlap " << overlap << "and fmax " << fmax << " gave " << nb << " points in t/f plane" << ". Factor " << (double)nb / LEN << endl;

   int ttime = 0;
   auto tm_duration = duration_cast<microseconds>(t2 - t1).count();
   ttime += tm_duration;
   cout << "setup time us " << ttime << endl;

   tm_duration = duration_cast<microseconds>(t3 - t2).count();
   ttime += tm_duration;
   cout << "setup+transform time us " << ttime << ", " << tm_duration << endl;

   tm_duration = duration_cast<microseconds>(t4 - t3).count();
   ttime += tm_duration;
   cout << "setup+transform+extract time us " << ttime << ", " << tm_duration << endl;

   cout << std::fixed << std::setprecision(2);
   std::cout << std::endl;
   double total2 = 0;
   for (int finx = FLEN; finx--; )
   {
      for (int tinx = LEN/2 - DELTA; tinx < LEN/2 + DELTA; tinx++)
      {
         std::cout << out[finx*LEN + tinx] << ", ";
         total2 += out[finx*LEN + tinx];
      }
      std::cout << std::endl;
   }
   
   cout << "Totals " << total1 << " / " << total2 << endl;
   
   int N = wc->prepareSequences(spike, LEN, 0, 0, timestamps, FLEN, false, out2);
   for (int n = 0; n < N; n++)
   {
      wc->executeSequence(n);
   }
   
   // Write out comparison map
   cout << std::fixed << std::setprecision(10);
   for (int finx = FLEN; finx--; )
   {
      for (int tinx = LEN/2 + DELTA - 1; tinx >= LEN/2 - DELTA; tinx--)
      {
//         cout << tinx << ", " << finx << "(" << finx*LEN + tinx << "): " << out[finx*LEN + tinx] << ", " << out2[finx*LEN + tinx] << endl;
      }
   }

   // Proove the same result as direct calculation
   for (int finx = FLEN; finx--; )
   {
      for (int tinx = LEN/2 - DELTA; tinx < LEN/2 + DELTA; tinx++)
      {
         assert(out[finx*LEN + tinx] == out2[finx*LEN + tinx]);
      }
   }

   
   // Reuse calculator for a shorter sequence. This is OK, although not efficient
   nb = wc->doTransform(spike, LEN/2, 0, 0);
   cout << "Transform of " << LEN/2 << " samples with overlap " << overlap << " gave " << nb << " points in t/f plane" << endl;
   
   // Reset calculator and do the same again
   unsigned int pre, post;
   wc->prepare(LEN, 0, pre, post);
   cout << "Pre " << pre << ", post " << post << endl;
   unsigned int len = LEN/2 + dist999(rng);
   wc->prepare(len, 0, pre, post);
   cout << "Transform of " << len << " samples with overlap " << overlap << " gave ";
   nb = wc->doTransform(spike, len, 0, 0);
   cout << nb << " points in t/f plane" << endl;

   delete wc;
return 0;
}
