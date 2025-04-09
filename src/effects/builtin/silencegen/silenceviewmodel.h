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
    SilenceViewModel() = default;
    ~SilenceViewModel() override = default;

private:
    void doEmitSignals() override {}
};
}
