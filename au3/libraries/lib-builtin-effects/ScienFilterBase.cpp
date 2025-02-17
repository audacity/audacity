/**********************************************************************

  Audacity: A Digital Audio Editor

  ScienFilterBase.cpp

  Norm C
  Mitch Golden
  Vaughan Johnson (Preview)

*******************************************************************//**

\class ScienFilterBase
\brief An Effect that applies 'classical' IIR filters.

  Performs IIR filtering that emulates analog filters, specifically
  Butterworth, Chebyshev Type I and Type II. Highpass and lowpass filters
  are supported, as are filter orders from 1 to 10.

  The filter is applied using biquads

*//*******************************************************************/
#include "ScienFilterBase.h"
#include "BasicUI.h"
#include "Prefs.h"
#include "Project.h"
#include "WaveTrack.h"
#include <cmath>

#if !defined(M_PI)
#   define PI = 3.1415926535897932384626433832795
#else
#   define PI M_PI
#endif
#define square(a) ((a) * (a))

const EnumValueSymbol ScienFilterBase::kTypeStrings[nTypes] = {
    /*i18n-hint: Butterworth is the name of the person after whom the filter type
       is named.*/
    { XO("Butterworth") },
    /*i18n-hint: Chebyshev is the name of the person after whom the filter type
       is named.*/
    { XO("Chebyshev Type I") },
    /*i18n-hint: Chebyshev is the name of the person after whom the filter type
       is named.*/
    { XO("Chebyshev Type II") }
};

const EnumValueSymbol ScienFilterBase::kSubTypeStrings[nSubTypes] = {
    // These are acceptable dual purpose internal/visible names
    { XO("Lowpass") },
    { XO("Highpass") }
};

const EffectParameterMethods& ScienFilterBase::Parameters() const
{
    static CapturedParameters<
        ScienFilterBase, Type, Subtype, Order, Cutoff, Passband, Stopband>
    parameters {
        [](ScienFilterBase&, EffectSettings&, ScienFilterBase& e,
           bool updating) {
            if (updating) {
                e.mOrderIndex = e.mOrder - 1;
                e.CalcFilter();
            }
            return true;
        },
    };
    return parameters;
}

//----------------------------------------------------------------------------
// ScienFilterBase
//----------------------------------------------------------------------------

const ComponentInterfaceSymbol ScienFilterBase::Symbol { XO(
                                                             "Classic Filters") };

ScienFilterBase::ScienFilterBase()
{
    Parameters().Reset(*this);
    SetLinearEffectFlag(true);

    mOrderIndex = mOrder - 1;

    mdBMin = -30.0;
    mdBMax = 30.0;

    mLoFreq = 20; // Lowest frequency to display in response graph
    mNyquist
        =44100.0
          / 2.0; // only used during initialization, updated when effect is used
}

ScienFilterBase::~ScienFilterBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol ScienFilterBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString ScienFilterBase::GetDescription() const
{
    /* i18n-hint: "infinite impulse response" */
    return XO("Performs IIR filtering that emulates analog filters");
}

ManualPageID ScienFilterBase::ManualPage() const
{
    return L"Classic_Filters";
}

// EffectDefinitionInterface implementation

EffectType ScienFilterBase::GetType() const
{
    return EffectTypeProcess;
}

unsigned ScienFilterBase::GetAudioInCount() const
{
    return 1;
}

unsigned ScienFilterBase::GetAudioOutCount() const
{
    return 1;
}

bool ScienFilterBase::ProcessInitialize(
    EffectSettings&, double, ChannelNames chanMap)
{
    for (int iPair = 0; iPair < (mOrder + 1) / 2; iPair++) {
        mpBiquad[iPair].Reset();
    }
    return true;
}

size_t ScienFilterBase::ProcessBlock(
    EffectSettings&, const float* const* inBlock, float* const* outBlock,
    size_t blockLen)
{
    const float* ibuf = inBlock[0];
    for (int iPair = 0; iPair < (mOrder + 1) / 2; iPair++) {
        mpBiquad[iPair].Process(ibuf, outBlock[0], blockLen);
        ibuf = outBlock[0];
    }

    return blockLen;
}

// Effect implementation

bool ScienFilterBase::Init()
{
    int selcount = 0;
    double rate = 0.0;

    auto trackRange = inputTracks()->Selected<const WaveTrack>();

    {
        auto t = *trackRange.begin();
        mNyquist = (t ? t->GetRate() : mProjectRate) / 2.0;
    }

    for (auto t : trackRange) {
        if (selcount == 0) {
            rate = t->GetRate();
        } else {
            if (t->GetRate() != rate) {
                BasicUI::ShowMessageBox(XO(
                                            "To apply a filter, all selected tracks must have the same sample rate."));
                return false;
            }
        }
        ++selcount;
    }

    return true;
}

void ScienFilterBase::CalcFilter()
{
    switch (mFilterType) {
    case kButterworth:
        mpBiquad = Biquad::CalcButterworthFilter(
            mOrder, mNyquist, mCutoff, mFilterSubtype);
        break;
    case kChebyshevTypeI:
        mpBiquad = Biquad::CalcChebyshevType1Filter(
            mOrder, mNyquist, mCutoff, mRipple, mFilterSubtype);
        break;
    case kChebyshevTypeII:
        mpBiquad = Biquad::CalcChebyshevType2Filter(
            mOrder, mNyquist, mCutoff, mStopbandRipple, mFilterSubtype);
        break;
    }
}

float ScienFilterBase::FilterMagnAtFreq(float Freq)
{
    float Magn;
    if (Freq >= mNyquist) {
        Freq = mNyquist - 1; // prevent tan(PI/2)
    }
    float FreqWarped = tan(PI * Freq / (2 * mNyquist));
    if (mCutoff >= mNyquist) {
        mCutoff = mNyquist - 1;
    }
    float CutoffWarped = tan(PI * mCutoff / (2 * mNyquist));
    float fOverflowThresh
        =pow(10.0, 12.0 / (2 * mOrder)); // once we exceed 10^12 there's not much
                                         // to be gained and overflow could happen

    switch (mFilterType) {
    case kButterworth: // Butterworth
    default:
        switch (mFilterSubtype) {
        case kLowPass: // lowpass
        default:
            if (FreqWarped / CutoffWarped > fOverflowThresh) { // prevent pow()
                                                               // overflow
                Magn = 0;
            } else {
                Magn = sqrt(1 / (1 + pow(FreqWarped / CutoffWarped, 2 * mOrder)));
            }
            break;
        case kHighPass: // highpass
            if (FreqWarped / CutoffWarped > fOverflowThresh) {
                Magn = 1;
            } else {
                Magn = sqrt(
                    pow(FreqWarped / CutoffWarped, 2 * mOrder)
                    / (1 + pow(FreqWarped / CutoffWarped, 2 * mOrder)));
            }
            break;
        }
        break;

    case kChebyshevTypeI: // Chebyshev Type 1
        double eps;
        eps = sqrt(pow(10.0, std::max<double>(0.001, mRipple) / 10.0) - 1);
        double chebyPolyVal;
        switch (mFilterSubtype) {
        case 0: // lowpass
        default:
            chebyPolyVal = Biquad::ChebyPoly(mOrder, FreqWarped / CutoffWarped);
            Magn = sqrt(1 / (1 + square(eps) * square(chebyPolyVal)));
            break;
        case 1:
            chebyPolyVal = Biquad::ChebyPoly(mOrder, CutoffWarped / FreqWarped);
            Magn = sqrt(1 / (1 + square(eps) * square(chebyPolyVal)));
            break;
        }
        break;

    case kChebyshevTypeII: // Chebyshev Type 2
        eps = 1 / sqrt(pow(10.0, std::max<double>(0.001, mStopbandRipple) / 10.0) - 1);
        switch (mFilterSubtype) {
        case kLowPass: // lowpass
        default:
            chebyPolyVal = Biquad::ChebyPoly(mOrder, CutoffWarped / FreqWarped);
            Magn = sqrt(1 / (1 + 1 / (square(eps) * square(chebyPolyVal))));
            break;
        case kHighPass:
            chebyPolyVal = Biquad::ChebyPoly(mOrder, FreqWarped / CutoffWarped);
            Magn = sqrt(1 / (1 + 1 / (square(eps) * square(chebyPolyVal))));
            break;
        }
        break;
    }

    return Magn;
}
