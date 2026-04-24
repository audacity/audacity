/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/effectsettingmodelimpl.h"
#include "au3-builtin-effects/TimeScaleBase.h"

namespace au::effects {
class SlidingStretchSettingModel : public EffectSettingModelImpl<TimeScaleBase>
{
public:
    SlidingStretchSettingModel(QObject* parent, int instanceId);
};

class SlidingStretchSettingModelFactory : public BuiltinEffectSettingModelFactory<SlidingStretchSettingModel>
{
};
}
