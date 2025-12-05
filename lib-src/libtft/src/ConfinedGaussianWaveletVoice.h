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

#ifndef CGWaveletVoice_h
#define CGWaveletVoice_h

#include "WaveletVoice.h"

using WaveletBaseClass = WaveletVoice; // Two possibilities here: WaveletVoice is faster, WaveletVoiceUnbuffered uses less memory. Optimal choice depends on usage scenario
class ConfinedGaussianWaveletVoice : public WaveletBaseClass
{
public:
   /**
    Construct wavelet based on modulated half-length approximate confined gaussian window, ref https://en.wikipedia.org/wiki/Window_function#Confined_Gaussian_window
     This window is a good-enough estimate for confined gaussian window which has optimum time-frequency location for a fixed-length window

    Construct half-length wavelet (complex conjugate symmetrical) at relative frequency
         Resulting wavelet is buffered in internal buffers having dimension halfLen+1
         Approximate temporal resolution is dT =  (2 * halfLen) * sigma
         Approximate frequency resolution is dF = 1/4/pi/dT
         Approximate 70dB frequency resolution depends on choice of sigma. For sigma = 0.1, we get approximately 6.5 * dF (found via inspection)
    
         Relative bandwidth of generated wavelet is 2*dF / relF which we solve for halfLen vs Q factor
         Q^-1 = 2*dF/relF = 1/2/pi/dT/relF  = 1 / (2 * pi * (2 * halfLen ) * sigma * relF)
    
          2 * halflen  = 1 / (2 * pi * sigma * relF * Q^-1)
            halfLen = (1 / (2 * pi * sigma * relF * Q^-1) ) / 2
    
         Example
            Q^-1 = 0.1156 (1/6 octave filter, 1/Q = sqrt(2^1/6) - sqrt(2^-1/6)
            sigma = 0.1
            relF = 0.25
      
          ==> halfLen = 27.54
    
         Function will assure that upper -70dB limit of generated wavelet is below limiting frequency range.
          Wavelet upper frequency is estimated like upperF = relF + 6.5*dF = relF + 6.5 / 4 / pi / dT  = relF + 0.52 / (2 * halfLen) / sigma
         For above example we get
          upperF = 0.25 + 0.52 /0.1 /55 = 0.34
    
    */
   ConfinedGaussianWaveletVoice(double fCenter,
                double Q,
                const float overlapPercentage,
                const DyadicFilter * dFilter);
   virtual void dump() override;
   virtual void getFrequency(double & freq, double & bw) const override;
   virtual ~ConfinedGaussianWaveletVoice();
protected:
   static constexpr float sigma = 0.1;
   static constexpr float bw70Ratio = 6.5;
   static constexpr float pi = 3.14159265359;
   unsigned int waveletLength;
   double q;
   
   float gaussian(float x);
   float approximateConfinedGaussian(float x);
};

#endif // !CGWaveletVoice_h
