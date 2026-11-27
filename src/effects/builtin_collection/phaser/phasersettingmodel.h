/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/effectsettingmodelimpl.h"
#include "phasereffect.h"

namespace au::effects {
class PhaserSettingModel : public EffectSettingModelImpl<PhaserEffect>
{
public:
    PhaserSettingModel(QObject* parent, int instanceId);
};

class PhaserSettingModelFactory : public BuiltinEffectSettingModelFactory<PhaserSettingModel>
{
};
}
