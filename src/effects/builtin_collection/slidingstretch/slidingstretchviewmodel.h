/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/builtin/view/builtineffectmodel.h"

namespace au::effects {
class SlidingStretchViewModel : public BuiltinEffectModel
{
    Q_OBJECT

public:
    SlidingStretchViewModel(QObject* parent, int instanceId);

private:
    void doReload() override;
};

class SlidingStretchViewModelFactory : public EffectViewModelFactory<SlidingStretchViewModel>
{
};
}
