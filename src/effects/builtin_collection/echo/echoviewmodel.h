#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

namespace au::effects {
//! The settings are edited through EchoSettingModel
class EchoViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    EchoViewModel(QObject* parent, int instanceId);

private:
    void doReload() override;
};

class EchoViewModelFactory : public EffectViewModelFactory<EchoViewModel>
{
};
}
