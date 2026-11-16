/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

namespace au::effects {
class LimiterViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    LimiterViewModel(QObject* parent, int instanceId);

private:
    void doReload() override;
};

class LimiterViewModelFactory : public EffectViewModelFactory<LimiterViewModel>
{
};
}
