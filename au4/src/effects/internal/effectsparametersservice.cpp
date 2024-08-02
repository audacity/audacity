/*
* Audacity: A Digital Audio Editor
*/
#include "effectsparametersservice.h"

#include "global/types/id.h"

#include "containers.h"
#include "log.h"

using namespace au::effects;

void EffectsParametersService::regEffectParameters(const muse::String& effectId, const EffectParameters& parameters)
{
    if (muse::contains(m_effectsParametersMap, effectId)) {
        LOGE() << "effect " << effectId << " is already registered. Will be overridden.";
        return;
    }

    ParametersData data;
    data.channel = new muse::async::Channel<EffectParameter>();
    data.parameters = parameters;

    m_effectsParametersMap[effectId] = data;
}

EffectParameters& EffectsParametersService::effectParameters(const muse::String& effectId) const
{
    if (!muse::contains(m_effectsParametersMap, effectId)) {
        LOGW() << "not found effect " << effectId;
        static EffectParameters stub;
        return stub;
    }

    return m_effectsParametersMap[effectId].parameters;
}

muse::async::Channel<EffectParameter> EffectsParametersService::effectParameterChanged(const muse::String& effectId) const
{
    if (!muse::contains(m_effectsParametersMap, effectId)) {
        LOGW() << "not found effect " << effectId;
        static muse::async::Channel<EffectParameter> stub;
        return stub;
    }

    return *m_effectsParametersMap[effectId].channel;
}
