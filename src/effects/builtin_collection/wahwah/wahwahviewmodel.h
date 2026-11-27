#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

namespace au::effects {
//! The settings are edited through WahWahSettingModel
class WahWahViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    WahWahViewModel(QObject* parent, int instanceId);

private:
    void doReload() override;
};

class WahWahViewModelFactory : public EffectViewModelFactory<WahWahViewModel>
{
};
}
