#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

namespace au::effects {
//! The settings are edited through ReverbSettingModel
class ReverbViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    ReverbViewModel(QObject* parent, int instanceId);

private:
    void doReload() override;
};

class ReverbViewModelFactory : public EffectViewModelFactory<ReverbViewModel>
{
};
}
