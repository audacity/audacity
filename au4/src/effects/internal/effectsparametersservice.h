/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "effects/ieffectsparametersservice.h"

namespace au::effects {
class EffectsParametersService : public IEffectsParametersService
{
public:
    void regEffectParameters(const muse::String& effectId, const EffectParameters& parameters) override;
    EffectParameters &effectParameters(const muse::String& effectId) const override;
    muse::async::Channel<EffectParameter> effectParameterChanged(const muse::String& effectId) const override;

private:
    struct ParametersData {
        EffectParameters parameters;
        muse::async::Channel<EffectParameter>* channel = nullptr;
    };

    mutable std::map<muse::String, ParametersData> m_effectsParametersMap;
};
}
