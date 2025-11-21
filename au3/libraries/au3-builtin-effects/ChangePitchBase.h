/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2 or later.  See License.txt.

  ChangePitchBase.h
  Vaughan Johnson, Dominic Mazzoni, Steve Daulton

******************************************************************//**

\file ChangePitchBase.h
\brief Change Pitch effect provides raising or lowering
the pitch without changing the tempo.

*//*******************************************************************/

#if USE_SOUNDTOUCH

#pragma once

#include "SettingsVisitor.h"
#include "SoundTouchBase.h"

class BUILTIN_EFFECTS_API ChangePitchBase : public SoundTouchBase
{
public:
    static inline ChangePitchBase*
    FetchParameters(ChangePitchBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    ChangePitchBase();
    virtual ~ChangePitchBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    OptionalMessage LoadFactoryDefaults(EffectSettings& settings) const override;
    OptionalMessage DoLoadFactoryDefaults(EffectSettings& settings);

    bool Process(EffectInstance& instance, EffectSettings& settings) override;
    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;

protected:
    // ChangePitchBase implementation

    // Deduce m_FromFrequency from the samples at the beginning of
    // the selection. Then set some other params accordingly.
    void DeduceFrequencies();

    // calculations
    void Calc_ToPitch(); // Update m_nToPitch from NEW m_dSemitonesChange.
    void Calc_ToOctave();
    void Calc_SemitonesChange_fromPitches();
    void Calc_SemitonesChange_fromOctaveChange();
    void Calc_SemitonesChange_fromPercentChange();
    void Calc_ToFrequency();  // Update m_ToFrequency from m_FromFrequency &
                              // m_dPercentChange.
    void Calc_PercentChange(); // Update m_dPercentChange based on NEW
                               // m_dSemitonesChange.

    bool mUseSBSMS;
    // effect parameters
    int m_nFromPitch; // per PitchIndex()
    int m_nFromOctave; // per PitchOctave()
    int m_nToPitch;   // per PitchIndex()
    int m_nToOctave;  // per PitchOctave()

    double m_FromFrequency; // starting frequency of selection
    double m_ToFrequency;  // target frequency of selection

    double m_dSemitonesChange; // how many semitones to change pitch
    double m_dStartFrequency; // starting frequency of first 0.2s of selection
    double
        m_dPercentChange; // percent change to apply to pitch
                          // Slider is (-100, 200], but textCtrls can set higher.

    bool m_bLoopDetect; // Used to avoid loops in initialization and in event
                        // handling.

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Percentage {
        &ChangePitchBase::m_dPercentChange, L"Percentage", 0.0, -99.0, 3000.0, 1
    };
    static constexpr EffectParameter UseSBSMS {
        &ChangePitchBase::mUseSBSMS, L"SBSMS", false, false, true, 1
    };
};

#endif // USE_SOUNDTOUCH
