/**********************************************************************

  Audacity: A Digital Audio Editor

  SpecPowerMeter.cpp

  Philipp Sibler

******************************************************************//**

\class SpecPowerMeter
\brief SpecPowerMeter is a simple spectral power level meter.

SpecPowerMeter operates in the Fourier domain and allows power level
measurements in subbands or in the entire signal band.  

*//*******************************************************************/

#include "SpecPowerMeter.h"

#include <cmath>
#include <cstdlib>
#include <wx/defs.h>

#include "../FFT.h"

SpecPowerMeter::SpecPowerMeter(size_t sigLen)
  : mSigLen(sigLen)
{

   // Init buffers
   mSigI = new float[sigLen];
   mSigFR = new float[sigLen];
   mSigFI = new float[sigLen];
   for (int n = 0; n < sigLen; n++)
   {
      mSigI[n] = 0.0f;
   }
}

SpecPowerMeter::~SpecPowerMeter()
{
   delete[] mSigI;
   delete[] mSigFR;
   delete[] mSigFI;
}

float SpecPowerMeter::CalcPower(float* sig, float fc, float bw)
{
   float pwr;
   int loBin, hiBin;
   
   // Given the bandwidth bw, get the boundary bin numbers
   loBin = Freq2Bin(fc - (bw / 2.0f));
   hiBin = Freq2Bin(fc + (bw / 2.0f));
   if (loBin == hiBin)
   {
      hiBin = loBin + 1;
   }
   
   // Calc the FFT
   FFT(mSigLen, 0, sig, mSigI, mSigFR, mSigFI);
   
   // Calc the in-band power
   pwr = CalcBinPower(mSigFR, mSigFI, loBin, hiBin);
   
   return pwr;     
}

float SpecPowerMeter::CalcBinPower(float* sig_f_r, float* sig_f_i, int loBin, int hiBin)
{
   float pwr = 0.0f;
   
   for (int n = loBin; n < hiBin; n++)
   {
      pwr += ((sig_f_r[n]*sig_f_r[n])+(sig_f_i[n]*sig_f_i[n]));
   }
   
   return pwr;
}

int SpecPowerMeter::Freq2Bin(float fc)
{
   int bin;
   
   // There is no round() in (older) MSVSs ...
   bin = floor((double)fc * mSigLen);
   bin %= (int)mSigLen;
   
   return bin;   
}

