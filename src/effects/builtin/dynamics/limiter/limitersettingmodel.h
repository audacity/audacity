/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/builtin/common/effectsettingmodelimpl.h"
#include "limitereffect.h"

namespace au::effects {
class LimiterSettingModel : public EffectSettingModelImpl<LimiterEffect>
{
public:
    LimiterSettingModel(QObject* parent = nullptr);
};
}
