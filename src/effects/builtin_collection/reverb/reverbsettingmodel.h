/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/effectsettingmodelimpl.h"
#include "reverbeffect.h"

namespace au::effects {
class ReverbSettingModel : public EffectSettingModelImpl<ReverbEffect>
{
public:
    ReverbSettingModel(QObject* parent, int instanceId);
};

class ReverbSettingModelFactory : public BuiltinEffectSettingModelFactory<ReverbSettingModel>
{
};
}
