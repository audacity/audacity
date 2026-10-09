/*
 * Audacity: A Digital Audio Editor
 */
/**********************************************************************

  Audacity: A Digital Audio Editor

  autoduckeffect.h

  Markus Meyer

**********************************************************************/
#pragma once

#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-effects/StatefulEffect.h"
#include "au3-track/Track.h"

#include <cfloat>
#include <optional>
#include <string>
#include <vector>

class WaveChannel;
class WaveTrack;

namespace au::effects {
class AutoDuckEffect : public StatefulEffect
{
public:
    static inline AutoDuckEffect* FetchParameters(AutoDuckEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    AutoDuckEffect();
    virtual ~AutoDuckEffect();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    ::EffectType GetType() const override;
    ::EffectGroup GetGroup() const override { return EffectGroup::VolumeAndCompression; }

    // Effect implementation

    bool Init() override;
    bool Process(::EffectInstance& instance, EffectSettings& settings) override;

    // AutoDuckEffect implementation

    struct ControlTrackCandidate {
        ::TrackId id;
        std::string name;
    };

    //! Wave tracks that are not selected, in project order. Updated by Init().
    const std::vector<ControlTrackCandidate>& ControlTrackCandidates() const;
    std::optional<::TrackId> ControlTrackId() const;
    void SetControlTrackId(::TrackId id);

    double mDuckAmountDb = DuckAmountDb.def;
    double mInnerFadeDownLen = InnerFadeDownLen.def;
    double mInnerFadeUpLen = InnerFadeUpLen.def;
    double mOuterFadeDownLen = OuterFadeDownLen.def;
    double mOuterFadeUpLen = OuterFadeUpLen.def;
    double mThresholdDb = ThresholdDb.def;
    double mMaximumPause = MaximumPause.def;

private:
    bool ApplyDuckFade(int trackNum, WaveChannel& track, double t0, double t1);
    const WaveTrack* FindControlTrack() const;

    std::vector<ControlTrackCandidate> mControlTrackCandidates;
    std::optional<::TrackId> mControlTrackId;

protected:
    const EffectParameterMethods& Parameters() const override;

public:
    static constexpr EffectParameter DuckAmountDb {
        &AutoDuckEffect::mDuckAmountDb, L"DuckAmountDb", -12.0, -24.0, 0.0, 1
    };
    static constexpr EffectParameter InnerFadeDownLen {
        &AutoDuckEffect::mInnerFadeDownLen, L"InnerFadeDownLen", 0.0, 0.0, 3.0, 1
    };
    static constexpr EffectParameter InnerFadeUpLen {
        &AutoDuckEffect::mInnerFadeUpLen, L"InnerFadeUpLen", 0.0, 0.0, 3.0, 1
    };
    static constexpr EffectParameter OuterFadeDownLen {
        &AutoDuckEffect::mOuterFadeDownLen, L"OuterFadeDownLen", 0.5, 0.0, 3.0, 1
    };
    static constexpr EffectParameter OuterFadeUpLen {
        &AutoDuckEffect::mOuterFadeUpLen, L"OuterFadeUpLen", 0.5, 0.0, 3.0, 1
    };
    static constexpr EffectParameter ThresholdDb {
        &AutoDuckEffect::mThresholdDb, L"ThresholdDb", -30.0, -100.0, 0.0, 1
    };
    static constexpr EffectParameter MaximumPause {
        &AutoDuckEffect::mMaximumPause, L"MaximumPause", 1.0, 0.0, DBL_MAX, 1
    };
};
}
