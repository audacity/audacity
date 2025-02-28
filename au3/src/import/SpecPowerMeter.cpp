/**********************************************************************

  Audacity: A Digital Audio Editor

  SpecPowerCalculation.cpp

  Philipp Sibler

******************************************************************//**

\class SpecPowerCalculation
\brief SpecPowerCalculation is a simple spectral power level meter.

SpecPowerCalculation operates in the Fourier domain and allows power level
measurements in subbands or in the entire signal band.

*//*******************************************************************/

#include "SpecPowerMeter.h"

#include <cmath>
#include <cstdlib>
#include <wx/defs.h>

#include "FFT.h"

SpecPowerCalculation::SpecPowerCalculation(size_t sigLen)
    : mSigLen(sigLen)
    , mSigI{sigLen, true}
    , mSigFR{sigLen}
    , mSigFI{sigLen}
{
}

SpecPowerCalculation::~SpecPowerCalculation()
{
}

float SpecPowerCalculation::CalcPower(float* sig, float fc, float bw)
{
    float pwr;
    int loBin, hiBin;

    // Given the bandwidth bw, get the boundary bin numbers
    loBin = Freq2Bin(fc - (bw / 2.0f));
    hiBin = Freq2Bin(fc + (bw / 2.0f));
    if (loBin == hiBin) {
        hiBin = loBin + 1;
    }

    // Calc the FFT
    FFT(mSigLen, 0, sig, mSigI.get(), mSigFR.get(), mSigFI.get());

    // Calc the in-band power
    pwr = CalcBinPower(mSigFR.get(), mSigFI.get(), loBin, hiBin);

    return pwr;
}

float SpecPowerCalculation::CalcBinPower(float* sig_f_r, float* sig_f_i, int loBin, int hiBin)
{
    float pwr = 0.0f;

    for (int n = loBin; n < hiBin; n++) {
        pwr += ((sig_f_r[n] * sig_f_r[n]) + (sig_f_i[n] * sig_f_i[n]));
    }

    return pwr;
}

int SpecPowerCalculation::Freq2Bin(float fc)
{
    int bin;

    // There is no round() in (older) MSVSs ...
    bin = floor((double)fc * mSigLen);
    bin %= (int)mSigLen;

    return bin;
}
