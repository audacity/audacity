/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/TruncSilenceBase.h"

namespace au::effects {
class TruncateSilenceEffect final : public TruncSilenceBase
{
public:
    static inline TruncateSilenceEffect*
    FetchParameters(TruncateSilenceEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    TruncateSilenceEffect();
    ~TruncateSilenceEffect() override;

    // ComponentInterface implementation
    ComponentInterfaceSymbol GetSymbol() const override;

    // Expose protected members from base class as public
    using TruncSilenceBase::mThresholdDB;
    using TruncSilenceBase::mActionIndex;
    using TruncSilenceBase::mInitialAllowedSilence;
    using TruncSilenceBase::mTruncLongestAllowedSilence;
    using TruncSilenceBase::mSilenceCompressPercent;
    using TruncSilenceBase::mbIndependent;

    // Expose protected static parameter definitions as public
    using TruncSilenceBase::Threshold;
    using TruncSilenceBase::ActIndex;
    using TruncSilenceBase::Minimum;
    using TruncSilenceBase::Truncate;
    using TruncSilenceBase::Compress;
    using TruncSilenceBase::Independent;
};
}
