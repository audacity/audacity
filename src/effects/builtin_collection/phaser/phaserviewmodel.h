#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

namespace au::effects {
//! The settings are edited through PhaserSettingModel
class PhaserViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    PhaserViewModel(QObject* parent, int instanceId);

private:
    void doReload() override;
};

class PhaserViewModelFactory : public EffectViewModelFactory<PhaserViewModel>
{
};
}
