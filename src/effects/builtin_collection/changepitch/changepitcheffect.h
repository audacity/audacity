/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3-builtin-effects/ChangePitchBase.h"

namespace au::effects {
class ChangePitchEffect final : public ChangePitchBase
{
public:
    static inline ChangePitchEffect*
    FetchParameters(ChangePitchEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    ChangePitchEffect();
    ~ChangePitchEffect() override;

    // ComponentInterface implementation
    ComponentInterfaceSymbol GetSymbol() const override;

    // Expose protected members from base class as public
    using ChangePitchBase::mUseSBSMS;
    using ChangePitchBase::m_nFromPitch;
    using ChangePitchBase::m_nFromOctave;
    using ChangePitchBase::m_nToPitch;
    using ChangePitchBase::m_nToOctave;
    using ChangePitchBase::m_FromFrequency;
    using ChangePitchBase::m_ToFrequency;
    using ChangePitchBase::m_dSemitonesChange;
    using ChangePitchBase::m_dStartFrequency;
    using ChangePitchBase::m_dPercentChange;
    using ChangePitchBase::m_bLoopDetect;

    // Expose protected methods from base class as public
    using ChangePitchBase::DeduceFrequencies;
    using ChangePitchBase::Calc_ToPitch;
    using ChangePitchBase::Calc_ToOctave;
    using ChangePitchBase::Calc_SemitonesChange_fromPitches;
    using ChangePitchBase::Calc_SemitonesChange_fromOctaveChange;
    using ChangePitchBase::Calc_SemitonesChange_fromPercentChange;
    using ChangePitchBase::Calc_ToFrequency;
    using ChangePitchBase::Calc_PercentChange;

    // Expose protected static parameter definitions as public
    using ChangePitchBase::Percentage;
    using ChangePitchBase::UseSBSMS;
};
}
