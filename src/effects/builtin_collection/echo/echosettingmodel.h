/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/effectsettingmodelimpl.h"
#include "echoeffect.h"

namespace au::effects {
class EchoSettingModel : public EffectSettingModelImpl<EchoEffect>
{
public:
    EchoSettingModel(QObject* parent, int instanceId);
};

class EchoSettingModelFactory : public BuiltinEffectSettingModelFactory<EchoSettingModel>
{
};
}
