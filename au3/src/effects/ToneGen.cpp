/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.cpp

  Steve Jolly
  James Crook (Adapted for 'Chirps')

  This class implements a tone generator effect.

*******************************************************************//**

\class EffectToneGen
\brief An Effect that can generate a sine, square or sawtooth wave.
An extended mode of EffectToneGen supports 'chirps' where the
frequency changes smoothly during the tone.

*//*******************************************************************/
#include "ToneGen.h"
#include "EffectEditor.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/choice.h>
#include <wx/valgen.h>

#include "Project.h"
#include "ProjectRate.h"
#include "ShuttleGui.h"
#include "../widgets/valnum.h"
#include "../widgets/NumericTextCtrl.h"

const ComponentInterfaceSymbol EffectChirp::Symbol
{ XO("Chirp") };

namespace {
BuiltinEffectsModule::Registration< EffectChirp > reg;
}

const ComponentInterfaceSymbol EffectTone::Symbol
{ XO("Tone") };

namespace {
BuiltinEffectsModule::Registration< EffectTone > reg2;
}

BEGIN_EVENT_TABLE(EffectToneGen, wxEvtHandler)
EVT_TEXT(wxID_ANY, EffectToneGen::OnControlUpdate)
END_EVENT_TABLE();

// ComponentInterface implementation

ComponentInterfaceSymbol EffectToneGen::GetSymbol() const
{
    return mChirp ? EffectChirp::Symbol : EffectTone::Symbol;
}

TranslatableString EffectToneGen::GetDescription() const
{
    return mChirp
           ? XO("Generates an ascending or descending tone of one of four types")
           : XO("Generates a constant frequency tone of one of four types");
}

ManualPageID EffectToneGen::ManualPage() const
{
    return mChirp ? L"Chirp" : L"Tone";
}

// Effect implementation

std::unique_ptr<EffectEditor> EffectToneGen::PopulateOrExchange(
    ShuttleGui& S, EffectInstance&, EffectSettingsAccess& access,
    const EffectOutputs*)
{
    mUIParent = S.GetParent();
    wxTextCtrl* t;

    S.StartMultiColumn(2, wxCENTER);
    {
        S.Validator<wxGenericValidator>(&mWaveform)
        .AddChoice(XXO("&Waveform:"),
                   Msgids(kWaveStrings, nWaveforms));

        if (mChirp) {
            S.AddFixedText({});
            S.StartHorizontalLay(wxEXPAND);
            {
                S.StartHorizontalLay(wxLEFT, 50);
                {
                    S.AddTitle(XO("Start"));
                }
                S.EndHorizontalLay();

                S.StartHorizontalLay(wxLEFT, 50);
                {
                    S.AddTitle(XO("End"));
                }
                S.EndHorizontalLay();
            }
            S.EndHorizontalLay();

            S.AddPrompt(XXO("&Frequency (Hz):"));
            S.StartHorizontalLay(wxEXPAND);
            {
                S.StartHorizontalLay(wxLEFT, 50);
                {
                    t = S.Name(XO("Frequency Hertz Start"))
                        .Validator<FloatingPointValidator<double> >(
                        6, &mFrequency0,
                        NumValidatorStyle::NO_TRAILING_ZEROES,
                        StartFreq.min,
                        mProjectRate / 2.0)
                        .AddTextBox({}, L"", 12);
                }
                S.EndHorizontalLay();

                S.StartHorizontalLay(wxLEFT, 50);
                {
                    t = S.Name(XO("Frequency Hertz End"))
                        .Validator<FloatingPointValidator<double> >(
                        6, &mFrequency1,
                        NumValidatorStyle::NO_TRAILING_ZEROES,
                        EndFreq.min,
                        mProjectRate / 2.0)
                        .AddTextBox({}, L"", 12);
                }
                S.EndHorizontalLay();
            }
            S.EndHorizontalLay();

            S.AddPrompt(XXO("&Amplitude (0-1):"));
            S.StartHorizontalLay(wxEXPAND);
            {
                S.StartHorizontalLay(wxLEFT, 50);
                {
                    t = S.Name(XO("Amplitude Start"))
                        .Validator<FloatingPointValidator<double> >(
                        6, &mAmplitude0, NumValidatorStyle::NO_TRAILING_ZEROES,
                        StartAmp.min, StartAmp.max)
                        .AddTextBox({}, L"", 12);
                }
                S.EndHorizontalLay();

                S.StartHorizontalLay(wxLEFT, 50);
                {
                    t = S.Name(XO("Amplitude End"))
                        .Validator<FloatingPointValidator<double> >(
                        6, &mAmplitude1, NumValidatorStyle::NO_TRAILING_ZEROES,
                        EndAmp.min, EndAmp.max)
                        .AddTextBox({}, L"", 12);
                }
                S.EndHorizontalLay();
            }
            S.EndHorizontalLay();

            S.Validator<wxGenericValidator>(&mInterpolation)
            .AddChoice(XXO("I&nterpolation:"),
                       Msgids(kInterStrings, nInterpolations));
        } else {
            t = S.Validator<FloatingPointValidator<double> >(
                6, &mFrequency0, NumValidatorStyle::NO_TRAILING_ZEROES,
                Frequency.min,
                mProjectRate / 2.0)
                .AddTextBox(XXO("&Frequency (Hz):"), L"", 12);

            t = S.Validator<FloatingPointValidator<double> >(
                6, &mAmplitude0, NumValidatorStyle::NO_TRAILING_ZEROES,
                Amplitude.min, Amplitude.max)
                .AddTextBox(XXO("&Amplitude (0-1):"), L"", 12);
        }

        S.AddPrompt(XXO("&Duration:"));
        auto& extra = access.Get().extra;
        mToneDurationT = safenew
                             NumericTextCtrl(FormatterContext::SampleRateContext(mProjectRate),
                                             S.GetParent(), wxID_ANY,
                                             NumericConverterType_TIME(),
                                             extra.GetDurationFormat(),
                                             extra.GetDuration(),
                                             NumericTextCtrl::Options{}
                                             .AutoPos(true));
        S.Name(XO("Duration"))
        .Position(wxALIGN_LEFT | wxALL)
        .AddWindow(mToneDurationT);
    }
    S.EndMultiColumn();

    return nullptr;
}

bool EffectToneGen::TransferDataToWindow(const EffectSettings& settings)
{
    if (!mUIParent->TransferDataToWindow()) {
        return false;
    }

    mToneDurationT->SetValue(settings.extra.GetDuration());
    return true;
}

bool EffectToneGen::TransferDataFromWindow(EffectSettings& settings)
{
    if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow()) {
        return false;
    }

    if (!mChirp) {
        mFrequency1 = mFrequency0;
        mAmplitude1 = mAmplitude0;
    }

    settings.extra.SetDuration(mToneDurationT->GetValue());

    return true;
}

// ToneGenBase implementation

void EffectToneGen::OnControlUpdate(wxCommandEvent& WXUNUSED(evt))
{
    if (!EffectEditor::EnableApply(
            mUIParent, mUIParent->TransferDataFromWindow())) {
        return;
    }
}
