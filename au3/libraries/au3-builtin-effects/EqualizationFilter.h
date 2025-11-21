/**********************************************************************

  Audacity: A Digital Audio Editor

  EqualizationFilter.h

  Mitch Golden
  Vaughan Johnson (Preview)

  Paul Licameli split from Equalization.h

***********************************************************************/

#ifndef __AUDACITY_EQUALIZATION_FILTER__
#define __AUDACITY_EQUALIZATION_FILTER__

#include "EqualizationParameters.h" // base class
#include "Envelope.h" // member
#include "RealFFTf.h" // member
using Floats = ArrayOf<float>;

//! Extend EqualizationParameters with frequency domain coefficients computed
//! from a curve or from frequency band slider positions
struct BUILTIN_EFFECTS_API EqualizationFilter : EqualizationParameters
{
    // Low frequency of the FFT.  20Hz is the
    // low range of human hearing
    static constexpr int loFreqI = 20;

    // Number of samples in an FFT window
    // MJS - work out the optimum for this at run time?
    // Have a dialog box for it?
    static constexpr size_t windowSize = 16384u;

    explicit EqualizationFilter(const EffectSettingsManager& manager);

    //! Adjust given coefficients so there is a finite impulse response in time
    //! domain
    bool CalcFilter();

    //! Transform a given buffer of time domain signal, which should be zero
    //! padded left and right for the tails
    void Filter(size_t len, float* buffer) const;

    const Envelope& ChooseEnvelope() const
    { return mLin ? mLinEnvelope : mLogEnvelope; }
    Envelope& ChooseEnvelope()
    { return mLin ? mLinEnvelope : mLogEnvelope; }

    // If sliders show, always use the log envelope
    const Envelope& ChooseEnvelopeToPaint() const
    { return IsLinear() ? mLinEnvelope : mLogEnvelope; }

    Envelope mLinEnvelope, mLogEnvelope;
    HFFT hFFT{ GetFFT(windowSize) };
    Floats mFFTBuffer{ windowSize };
    Floats mFilterFuncR{ windowSize }, mFilterFuncI{ windowSize };
    double mLoFreq{ loFreqI };
    double mHiFreq{ mLoFreq };
    size_t mWindowSize{ windowSize };
};
#endif
