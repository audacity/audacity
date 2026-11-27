/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/effectsettingmodelimpl.h"
#include "wahwaheffect.h"

namespace au::effects {
class WahWahSettingModel : public EffectSettingModelImpl<WahWahEffect>
{
public:
    WahWahSettingModel(QObject* parent, int instanceId);
};

class WahWahSettingModelFactory : public BuiltinEffectSettingModelFactory<WahWahSettingModel>
{
};
}
