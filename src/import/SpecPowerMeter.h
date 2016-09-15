/**********************************************************************

  Audacity: A Digital Audio Editor

  SpecPowerMeter.h

  Philipp Sibler

**********************************************************************/

#ifndef __AUDACITY_SPECPOWERMETER_H_
#define __AUDACITY_SPECPOWERMETER_H_

#include <cstddef>

class SpecPowerMeter
{
   const size_t mSigLen;
   
   float* mSigI;
   float* mSigFR;
   float* mSigFI;

   float CalcBinPower(float* sig_f_r, float* sig_f_i, int loBin, int hiBin);
   int Freq2Bin(float fc);
public:
   SpecPowerMeter(size_t sigLen);
   ~SpecPowerMeter();
   
   float CalcPower(float* sig, float fc, float bw);
};

#endif

