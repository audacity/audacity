/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/builtin/common/effectsettingmodelimpl.h"
#include "compressoreffect.h"

namespace au::effects {
class CompressorSettingModel : public EffectSettingModelImpl<CompressorEffect>
{
public:
    CompressorSettingModel(QObject* parent = nullptr);
};
}
