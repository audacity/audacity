/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/effectsettingmodelimpl.h"
#include "limitereffect.h"

#include <cassert>
#include <functional>

namespace au::effects {
class LimiterViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    LimiterViewModel(QObject* parent = nullptr);

private:
    void doReload() override;
};

class LimiterSettingModel : public EffectSettingModelImpl<LimiterEffect>
{
public:
    LimiterSettingModel(QObject* parent = nullptr);
};
}
