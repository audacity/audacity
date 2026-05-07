/*
 * Audacity: A Digital Audio Editor
 */
#include "slidingstretchsettingmodel.h"

#include "global/log.h"
#include "global/translation.h"

namespace au::effects {
SlidingStretchSettingModel::SlidingStretchSettingModel(QObject* parent, int instanceId)
    : EffectSettingModelImpl<SlidingStretchEffect>(parent, instanceId, {
        { "initialTempo", { muse::qtrc("effects", "Initial tempo change"), QString { "%" } } },
        { "finalTempo", { muse::qtrc("effects", "Final tempo change"), QString { "%" } } },
        { "initialPitchSemitones", { muse::qtrc("effects", "Initial pitch shift"), muse::qtrc("effects", "semitones") } },
        { "finalPitchSemitones", { muse::qtrc("effects", "Final pitch shift"), muse::qtrc("effects", "semitones") } },
        { "initialPitchPct", { muse::qtrc("effects", "Initial pitch shift"), QString { "%" } } },
        { "finalPitchPct", { muse::qtrc("effects", "Final pitch shift"), QString { "%" } } },
    }, [this](const SlidingStretchEffect& effect) {
    if (m_paramId == "initialTempo") {
        return effect.RatePercentStart;
    } else if (m_paramId == "finalTempo") {
        return effect.RatePercentEnd;
    } else if (m_paramId == "initialPitchSemitones") {
        return effect.HalfStepsStart;
    } else if (m_paramId == "finalPitchSemitones") {
        return effect.HalfStepsEnd;
    } else if (m_paramId == "initialPitchPct") {
        return effect.PitchPercentStart;
    } else if (m_paramId == "finalPitchPct") {
        return effect.PitchPercentEnd;
    } else {
        LOGE() << "Unknown sliding stretch parameter id:" << m_paramId;
        assert(false);
        return effect.RatePercentStart;
    }
})
{
}
}
