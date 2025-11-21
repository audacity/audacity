/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationFilter.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

**********************************************************************/
#include "EqualizationFilter.h"
#include "Envelope.h"
#include "FFT.h"

EqualizationFilter::EqualizationFilter(const EffectSettingsManager& manager)
    : EqualizationParameters{manager}
    , mLogEnvelope{false,
                   dBMin.min, dBMax.max, // MB: this is the highest possible range
                   0.0}
    , mLinEnvelope{false,
                   dBMin.min, dBMax.max, // MB: this is the highest possible range
                   0.0}
{
    mLogEnvelope.SetTrackLen(1.0);
    mLinEnvelope.SetTrackLen(1.0);
}

#include "RealFFTf.h"
bool EqualizationFilter::CalcFilter()
{
    // Inverse-transform the given curve from frequency domain to time;
    // Apply a taper to define a finite impulse response;
    // Transform that back to frequency domain to get the modified curve.

    double loLog = log10(mLoFreq);
    double hiLog = log10(mHiFreq);
    double denom = hiLog - loLog;

    double delta = mHiFreq / ((double)(mWindowSize / 2.));
    double val0;
    double val1;

    if (IsLinear()) {
        val0 = mLinEnvelope.GetValue(0.0); //no scaling required - saved as dB
        val1 = mLinEnvelope.GetValue(1.0);
    } else {
        val0 = mLogEnvelope.GetValue(0.0); //no scaling required - saved as dB
        val1 = mLogEnvelope.GetValue(1.0);
    }
    mFilterFuncR[0] = val0;
    double freq = delta;

    for (size_t i = 1; i <= mWindowSize / 2; i++) {
        double when;
        if (IsLinear()) {
            when = freq / mHiFreq;
        } else {
            when = (log10(freq) - loLog) / denom;
        }
        if (when < 0.) {
            mFilterFuncR[i] = val0;
        } else if (when > 1.0) {
            mFilterFuncR[i] = val1;
        } else {
            if (IsLinear()) {
                mFilterFuncR[i] = mLinEnvelope.GetValue(when);
            } else {
                mFilterFuncR[i] = mLogEnvelope.GetValue(when);
            }
        }
        freq += delta;
    }
    mFilterFuncR[mWindowSize / 2] = val1;

    mFilterFuncR[0] = DB_TO_LINEAR(mFilterFuncR[0]);

    {
        size_t i = 1;
        for (; i < mWindowSize / 2; i++) {
            mFilterFuncR[i] = DB_TO_LINEAR(mFilterFuncR[i]);
            mFilterFuncR[mWindowSize - i] = mFilterFuncR[i]; //Fill entire array
        }
        mFilterFuncR[i] = DB_TO_LINEAR(mFilterFuncR[i]); //do last one
    }

    //transfer to time domain to do the padding and windowing
    Floats outr{ mWindowSize };
    Floats outi{ mWindowSize };
    InverseRealFFT(mWindowSize, mFilterFuncR.get(), NULL, outr.get()); // To time domain

    {
        size_t i = 0;
        for (; i <= (mM - 1) / 2; i++) { //Windowing - could give a choice, fixed for now - MJS
                                         //      double mult=0.54-0.46*cos(2*M_PI*(i+(mM-1)/2.0)/(mM-1));   //Hamming
                                         //Blackman
            double mult
                =0.42
                  - 0.5 * cos(2 * M_PI * (i + (mM - 1) / 2.0) / (mM - 1))
                  + .08 * cos(4 * M_PI * (i + (mM - 1) / 2.0) / (mM - 1));
            outr[i] *= mult;
            if (i != 0) {
                outr[mWindowSize - i] *= mult;
            }
        }
        for (; i <= mWindowSize / 2; i++) { //Padding
            outr[i] = 0;
            outr[mWindowSize - i] = 0;
        }
    }
    Floats tempr{ mM };
    {
        size_t i = 0;
        for (; i < (mM - 1) / 2; i++) { //shift so that padding on right
            tempr[(mM - 1) / 2 + i] = outr[i];
            tempr[i] = outr[mWindowSize - (mM - 1) / 2 + i];
        }
        tempr[(mM - 1) / 2 + i] = outr[i];
    }

    for (size_t i = 0; i < mM; i++) { //and copy useful values back
        outr[i] = tempr[i];
    }
    for (size_t i = mM; i < mWindowSize; i++) { //rest is padding
        outr[i]=0.;
    }

    //Back to the frequency domain so we can use it
    RealFFT(mWindowSize, outr.get(), mFilterFuncR.get(), mFilterFuncI.get());

    return TRUE;
}

void EqualizationFilter::Filter(size_t len, float* buffer) const
{
    // Transform a window of the time-domain signal to frequency;
    // Multiply by corresponding coefficients;
    // Inverse transform back to time domain:  that's fast convolution.

    float re, im;
    // Apply FFT
    RealFFTf(buffer, hFFT.get());
    //FFT(len, false, inr, NULL, outr, outi);

    // Apply filter
    // DC component is purely real
    mFFTBuffer[0] = buffer[0] * mFilterFuncR[0];
    for (size_t i = 1; i < (len / 2); i++) {
        re=buffer[hFFT->BitReversed[i]  ];
        im=buffer[hFFT->BitReversed[i] + 1];
        mFFTBuffer[2 * i  ] = re * mFilterFuncR[i] - im * mFilterFuncI[i];
        mFFTBuffer[2 * i + 1] = re * mFilterFuncI[i] + im * mFilterFuncR[i];
    }
    // Fs/2 component is purely real
    mFFTBuffer[1] = buffer[1] * mFilterFuncR[len / 2];

    // Inverse FFT and normalization
    InverseRealFFTf(mFFTBuffer.get(), hFFT.get());
    ReorderToTime(hFFT.get(), mFFTBuffer.get(), buffer);
}
