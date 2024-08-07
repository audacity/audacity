/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectAmplify
\brief An Effect that makes a sound louder or softer.

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

*//*******************************************************************/

#include "amplify.h"

#include <math.h>

#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/TimeStretching.h"
#include "libraries/lib-wave-track/WaveChannelUtilities.h"
#include "libraries/lib-effects/EffectOutputTracks.h"

using namespace au::effects;

enum
{
    ID_Amp = 10000,
    ID_Peak,
    ID_Clip
};

// const EffectParameterMethods& EffectAmplify::Parameters() const
// {
//     static CapturedParameters<EffectAmplify,
//                               // Interactive case
//                               Ratio, Clipping
//                               > parameters;

//     static CapturedParameters<EffectAmplify,
//                               Ratio
//                               > batchParameters{
//         // If invoking Amplify from a macro, mCanClip is not a parameter
//         // but is always true
//         [](EffectAmplify&, EffectSettings&, EffectAmplify& e, bool) {
//             e.mCanClip = true;
//             return true;
//         },
//     };

//     // Parameters differ depending on batch mode.  Option to disable clipping
//     // is interactive only.
//     if (IsBatchProcessing()) {
//         return batchParameters;
//     } else {
//         return parameters;
//     }
// }

//
// EffectAmplify
//

// EffectAmplify::Instance::~Instance()
// {
//     // In case the dialog is cancelled before effect processing
//     static_cast<EffectAmplify&>(GetEffect()).DestroyOutputTracks();
// }

EffectAmplify::EffectAmplify()
{
    setId("amplify"); // todo: duplicate id

    EffectParameters params = {
        m_ratio,
        m_amp,
        m_allowClipping
    };

    effectsParametersService()->regEffectParameters(muse::String(id()), params);
    effectsProcessing()->regEffect(muse::String(id()), this);

    emit ampChanged();
    emit ampSliderValueChanged();
    emit newPeakAmpChanged();
    emit allowClipingChanged();
}

EffectAmplify::~EffectAmplify()
{
    effectsProcessing()->unregEffect(muse::String(id()));
}

void EffectAmplify::onAmpTextChanged()
{
    // if (!mAmpT->GetValidator()->TransferFromWindow())
    // {
    //     EffectEditor::EnableApply(mUIParent, false);
    //     return;
    // }

    double scale = m_amp.scale.toDouble();
    double newRatio = dBToLinear(std::clamp<double>(m_amp.value.toDouble() * scale,
                                                    m_amp.minValue.toDouble() * scale,
                                                    m_amp.maxValue.toDouble() * scale) / scale);
    m_ratio.value = muse::Val(newRatio);

    setAmpSliderValue((int)(linearToDB(newRatio) * scale + 0.5));

    setNewPeakAmp(linearToDB(newRatio * m_peakAmp));
    // mNewPeakT->GetValidator()->TransferToWindow();

    CheckClip();
}

void EffectAmplify::onNewPeakTextChanged()
{
    // if (!mNewPeakT->GetValidator()->TransferFromWindow())
    // {
    //     EffectEditor::EnableApply(mUIParent, false);
    //     return;
    // }

    if (m_newPeakAmp == 0.0) {
        m_ratio.value = muse::Val(m_ratioMaxForClip);
    } else {
        m_ratio.value = muse::Val(dBToLinear(m_newPeakAmp) / m_peakAmp);
    }

    double ampInit = linearToDB(m_ratio.value.toDouble());
    double newAmp = std::clamp<double>(ampInit, m_amp.minValue.toDouble(), m_amp.maxValue.toDouble());
    setAmp(newAmp);

    if (newAmp != ampInit) {
        m_ratio.value = muse::Val(dBToLinear(newAmp));
    }

    setAmpSliderValue((int)(newAmp * m_amp.scale.toDouble() + 0.5f));

    CheckClip();
}

void EffectAmplify::onAmpSliderValueChanged(int newValue)
{
    double scale = m_amp.scale.toDouble();
    double min = m_amp.minValue.toDouble();
    double max = m_amp.maxValue.toDouble();

    double dB = newValue / scale;
    double newRatio = dBToLinear(std::clamp<double>(dB, min, max));

    double dB2 = (newValue - 1) / scale;
    double ratio2 = dBToLinear(std::clamp<double>(dB2, min, max));

    if (!m_allowClipping.value.toBool() && newRatio * m_peakAmp > 1.0 && ratio2 * m_peakAmp < 1.0) {
        newRatio = 1.0 / m_peakAmp;
    }

    m_ratio.value = muse::Val(newRatio);
    setAmp(linearToDB(newRatio));

    setNewPeakAmp(linearToDB(newRatio * m_peakAmp));

    CheckClip();
}

void EffectAmplify::CheckClip()
{
    // EffectEditor::EnableApply(mUIParent,
    // mClip->GetValue() || (mPeak > 0.0 && mRatio <= mRatioClip));
}

// // ComponentInterface implementation

// ComponentInterfaceSymbol EffectAmplify::GetSymbol() const
// {
//     return Symbol;
// }

// TranslatableString EffectAmplify::GetDescription() const
// {
//     // Note: This is useful only after ratio has been set.
//     return XO("Increases or decreases the volume of the audio you have selected");
// }

// ManualPageID EffectAmplify::ManualPage() const
// {
//     return L"Amplify";
// }

// EffectDefinitionInterface implementation

EffectType EffectAmplify::GetType() const
{
    return EffectTypeProcess;
}

unsigned EffectAmplify::GetAudioInCount() const
{
    return 1;
}

unsigned EffectAmplify::GetAudioOutCount() const
{
    return 1;
}

size_t EffectAmplify::ProcessBlock(EffectSettings&,
                                   const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
    for (decltype(blockLen) i = 0; i < blockLen; i++) {
        outBlock[0][i] = inBlock[0][i] * m_ratio.value.toDouble();
    }

    return blockLen;
}

OptionalMessage
EffectAmplify::LoadFactoryDefaults(EffectSettings& settings) const
{
    // To do: externalize state so const_cast isn't needed
    return const_cast<EffectAmplify&>(*this).DoLoadFactoryDefaults(settings);
}

OptionalMessage EffectAmplify::DoLoadFactoryDefaults(EffectSettings& settings)
{
    Init();

    m_ratioMaxForClip = 0.0;
    if (m_peakAmp > 0.0) {
        m_ratio.value = muse::Val(1.0 / m_peakAmp);
        m_ratioMaxForClip = m_ratio.value.toDouble();
    } else {
        m_ratio.value = muse::Val(1.0);
    }
    setAllowCliping(false);

    ClampRatio();
    return { nullptr };
}

// // Effect implementation

bool EffectAmplify::Init()
{
    auto range = inputTracks()->Selected<const WaveTrack>();
    bool hasPitchOrSpeed = std::any_of(begin(range), end(range), [this](auto* pTrack) {
        return TimeStretching::HasPitchOrSpeed(*pTrack, mT0, mT1);
    });
    if (hasPitchOrSpeed) {
        range = MakeOutputTracks()->Get().Selected<const WaveTrack>();
    }
    m_peakAmp = 0.0;
    for (auto t : range) {
        for (const auto pChannel : t->Channels()) {
            auto pair
                =WaveChannelUtilities::GetMinMax(*pChannel, mT0, mT1); // may throw
            const float min = pair.first, max = pair.second;
            const float newpeak = std::max(fabs(min), fabs(max));
            m_peakAmp = std::max<double>(m_peakAmp, newpeak);
        }
    }
    return true;
}

// std::any EffectAmplify::BeginPreview(const EffectSettings& settings)
// {
//     return { std::pair{
//                  CopyableValueRestorer(mRatio), CopyableValueRestorer(mPeak)
//              } };
// }

// std::unique_ptr<EffectEditor> EffectAmplify::PopulateOrExchange(
//     ShuttleGui& S, EffectInstance&, EffectSettingsAccess&,
//     const EffectOutputs*)
// {
//     mUIParent = S.GetParent();

//     enum {
//         precision = 3
//     };                    // allow (a generous) 3 decimal  places for Amplification (dB)

//     bool batch = IsBatchProcessing();
//     if (batch) {
//         mCanClip = true;
//         mPeak = 1.0;
//     } else {
//         if (mPeak > 0.0) {
//             mRatio = 1.0 / mPeak;
//             mRatioClip = mRatio;
//         } else {
//             mRatio = 1.0;
//         }
//     }

//     // At this point mNewPeak is still uninitialized; this will initialize it
//     ClampRatio();

//     S.AddSpace(0, 5);

//     S.StartVerticalLay(0);
//     {
//         // Amplitude
//         S.StartMultiColumn(2, wxCENTER);
//         {
//             mAmpT = S.Id(ID_Amp)
//                     .Validator<FloatingPointValidator<double> >(
//                 precision, &mAmp, NumValidatorStyle::ONE_TRAILING_ZERO, Amp.min, Amp.max)
//                     .AddTextBox(XXO("&Amplification (dB):"), L"", 12);
//         }
//         S.EndMultiColumn();

//         // Amplitude
//         S.StartHorizontalLay(wxEXPAND);
//         {
//             mAmpS = S.Id(ID_Amp)
//                     .Style(wxSL_HORIZONTAL)
//                     .Name(XO("Amplification dB"))
//                     .AddSlider({}, 0, Amp.max * Amp.scale, Amp.min * Amp.scale);
//         }
//         S.EndHorizontalLay();

//         // Peak
//         S.StartMultiColumn(2, wxCENTER);
//         {
//             mNewPeakT = S.Id(ID_Peak)
//                         .Validator<FloatingPointValidator<double> >(
//                 // One extra decimal place so that rounding is visible to user
//                 // (see: bug 958)
//                 precision + 1,
//                 &mNewPeak, NumValidatorStyle::ONE_TRAILING_ZERO,
//                 // min and max need same precision as what we're validating (bug 963)
//                 RoundValue(precision + 1, Amp.min + LINEAR_TO_DB(mPeak)),
//                 RoundValue(precision + 1, Amp.max + LINEAR_TO_DB(mPeak)))
//                         .AddTextBox(XXO("&New Peak Amplitude (dB):"), L"", 12);
//         }
//         S.EndMultiColumn();

//         // Clipping
//         S.StartHorizontalLay(wxCENTER);
//         {
//             mClip = S.Id(ID_Clip).Disable(batch)
//                     .AddCheckBox(XXO("Allo&w clipping"), false);
//         }
//         S.EndHorizontalLay();
//     }
//     S.EndVerticalLay();

//     return nullptr;
// }

void EffectAmplify::ClampRatio()
{
    // limit range of gain
    double dBInit = linearToDB(m_ratio.value.toDouble());
    double dB = std::clamp<double>(dBInit, m_amp.minValue.toDouble(), m_amp.maxValue.toDouble());
    if (dB != dBInit) {
        m_ratio.value = muse::Val(dBToLinear(dB));
    }

    setAmp(linearToDB(m_amp.maxValue.toDouble()));
    setNewPeakAmp(linearToDB(m_amp.maxValue.toDouble() * m_newPeakAmp));
}

// bool EffectAmplify::TransferDataToWindow(const EffectSettings&)
// {
//     mAmpT->GetValidator()->TransferToWindow();

//     mAmpS->SetValue((int)(mAmp * Amp.scale + 0.5f));

//     mNewPeakT->GetValidator()->TransferToWindow();

//     mClip->SetValue(mCanClip);

//     CheckClip();

//     return true;
// }

// bool EffectAmplify::TransferDataFromWindow(EffectSettings&)
// {
//     if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
//         return false;
//     }

//     mRatio = DB_TO_LINEAR(std::clamp<double>(mAmp * Amp.scale, Amp.min * Amp.scale, Amp.max * Amp.scale) / Amp.scale);

//     mCanClip = mClip->GetValue();

//     if (!mCanClip && mRatio * mPeak > 1.0) {
//         mRatio = 1.0 / mPeak;
//     }

//     ClampRatio();

//     return true;
// }

std::shared_ptr<EffectInstance> EffectAmplify::MakeInstance() const
{
    // Cheat with const_cast to return an object that calls through to
    // non-const methods of a stateful effect.
    return std::make_shared<Instance>(const_cast<EffectAmplify&>(*this));
}

// // EffectAmplify implementation

// void EffectAmplify::CheckClip()
// {
//     EffectEditor::EnableApply(mUIParent,
//                               mClip->GetValue() || (mPeak > 0.0 && mRatio <= mRatioClip));
// }

double EffectAmplify::amp() const
{
    return m_amp.value.toDouble();
}

void EffectAmplify::setAmp(double amp)
{
    if (qFuzzyCompare(m_amp.value.toDouble(), amp)) {
        return;
    }

    m_amp.value = muse::Val(amp);
    emit ampChanged();
}

double EffectAmplify::ampMin() const
{
    return m_amp.minValue.toDouble();
}

double EffectAmplify::ampMax() const
{
    return m_amp.maxValue.toDouble();
}

double EffectAmplify::newPeakAmp() const
{
    return m_newPeakAmp;
}

void EffectAmplify::setNewPeakAmp(double amp)
{
    if (qFuzzyCompare(m_newPeakAmp, amp)) {
        return;
    }

    m_newPeakAmp = amp;
    emit newPeakAmpChanged();
}

double EffectAmplify::newPeakAmpMin() const
{
    return ampMin() + linearToDB(m_peakAmp);
}

double EffectAmplify::newPeakAmpMax() const
{
    return ampMax() + linearToDB(m_peakAmp);
}

bool EffectAmplify::allowCliping() const
{
    return m_allowClipping.value.toBool();
}

void EffectAmplify::setAllowCliping(bool allow)
{
    if (allowCliping() == allow) {
        return;
    }

    m_allowClipping.value = muse::Val(allow);

    CheckClip();

    emit allowClipingChanged();
}

int EffectAmplify::ampSliderValue() const
{
    return m_ampSliderValue;
}

void EffectAmplify::setAmpSliderValue(int value)
{
    if (m_ampSliderValue == value) {
        return;
    }

    m_ampSliderValue = value;
    emit ampSliderValueChanged();
}

double EffectAmplify::ampSliderValueMin() const
{
    return ampMin() * m_amp.scale.toDouble();
}

double EffectAmplify::ampSliderValueMax() const
{
    return ampMax() * m_amp.scale.toDouble();
}
