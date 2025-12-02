/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "../common/generatoreffectmodel.h"

namespace au::effects {
class SilenceViewModel : public GeneratorEffectModel
{
    Q_OBJECT

public:
    SilenceViewModel(QObject* parent, int instanceId)
        : GeneratorEffectModel(parent, instanceId) {}
    ~SilenceViewModel() override = default;

private:
    void doEmitSignals() override {}
    bool usesPresets() const override { return false; }
};

class SilenceViewModelFactory : public EffectViewModelFactory<SilenceViewModel>
{
};
}
