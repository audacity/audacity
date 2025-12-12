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

//
//  WaveletVoice.h
//  test
//
//  Created by Klaus Gram-Hansen on 17/11/2025.
//

#include <assert.h>
#include <cmath>
#include <iostream>
#include "ConfinedGaussianWaveletVoice.h"
using namespace std;

ConfinedGaussianWaveletVoice::ConfinedGaussianWaveletVoice(double fCenter,
                double Q,
                const float overlapPercentage,
                const DyadicFilter * dFilter) : WaveletBaseClass(overlapPercentage, dFilter, fCenter), q(Q)
{
   // When constructing, firstly let us know about the upper frequency limit by the wavelet to be defined
   assert(fCenter > 0 && fCenter <= 0.5);
   assert(dFilter);
   float deltaF = fCenter / Q / 2.0;
   float fmax = bw70Ratio * deltaF + fCenter;
   
   // From fmax, locate the octave we will be using
   octave = dFilter->findOctave(fmax);
   
   // recalculate fCenter relative to actual octave
   fCenter *= (1 << octave);
   
   // Calculate Len of wavelet, halfLen = (1 / (2 * pi * sigma * fCenter / Q)) / 2
   float fltWaveletLength = Q / (2.0 * pi * sigma * fCenter) ;
   waveletHalfLength = (unsigned int) (fltWaveletLength / 2);
   waveletLength = 2 * waveletHalfLength + 1;
   waveletRe = new TF_DATA_TYPE[waveletHalfLength + 1];
   waveletIm = new TF_DATA_TYPE[waveletHalfLength + 1];
   
   float sum;
   waveletRe[0] = sum = approximateConfinedGaussian(waveletHalfLength);
   waveletIm[0] = 0;
   
   for (int i = 1; i <= waveletHalfLength; i++)
   {
      //     wavelet = np.append(wavelet, env * math.cos(2 * math.pi * fc * xs[inx]))
      float argument = 2 * pi * fCenter * i;
      float envelope = approximateConfinedGaussian(waveletHalfLength + i);
      sum += 2* envelope;
      waveletRe[i] = envelope * cos(argument);
      waveletIm[i] = envelope * sin(argument);
   }
   float factor = 2 / sum; // Factor "2" bcs of one-sided spectrum
   
   for (int i = 0; i <= waveletHalfLength; i++)
   {
      waveletRe[i] *= factor;
      waveletIm[i] *= factor;
   }
   
   // Calculate duration (measured in samples at current rate
   // Approximate temporal resolution is dT =  (2 * halfLen + 1) * sigma
   duration = 2.0 * waveletLength *sigma;
   transformStep = (unsigned int)(duration * (100-overlapPercentage) / 100.0 + 0.5);
   if (transformStep < 1)
   {
      transformStep = 1;
   }
}


void ConfinedGaussianWaveletVoice::dump()
{
   cout << "Approximate Confined Gaussian Wavelet in octave " << octave << ", windowL " << waveletLength <<", duration " << duration << ", step " << transformStep << endl;
   WaveletVoiceUnbuffered::dump();
}

ConfinedGaussianWaveletVoice::~ConfinedGaussianWaveletVoice()
{
   
}

float ConfinedGaussianWaveletVoice::gaussian(float x)
{
   float argument = (x - waveletHalfLength) / 2 / waveletLength / sigma;
   return exp(- argument * argument);
}

float ConfinedGaussianWaveletVoice::approximateConfinedGaussian(float x)
{
   return gaussian(x) - gaussian(-0.5) * (gaussian(x + waveletLength) + gaussian(x - waveletLength)) / (gaussian(-0.5 + waveletLength) + gaussian(-0.5 - waveletLength));
}

void ConfinedGaussianWaveletVoice::getFrequency(double & freq, double & bw) const
{
   freq = frequency;
   bw = frequency / q;
}
