/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-builtin-effects/RepeatBase.h"

namespace au::effects {
class RepeatEffect final : public RepeatBase
{
public:
    static inline RepeatEffect*
    FetchParameters(RepeatEffect& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    RepeatEffect();
    ~RepeatEffect() override;

    // ComponentInterface implementation
    ComponentInterfaceSymbol GetSymbol() const override;

    // EffectDefinitionInterface implementation
    EffectGroup GetGroup() const override
    {
        return EffectGroup::Special;
    }

    // Effect implementation
    //! Like Au3, where clips can't be selected: repeat the whole span of the selected clips
    bool processClipSelectionAsTimeRange() const override { return true; }
    bool Process(::EffectInstance& instance, EffectSettings& settings) override;

    //! Au4 cap: every repeat becomes a clip, and the timeline can't handle many thousands of clips on a track
    static constexpr int MaxRepeatCount = 100;

    // Expose protected members from base class as public
    using RepeatBase::repeatCount;
    using RepeatBase::Count;
};
}
