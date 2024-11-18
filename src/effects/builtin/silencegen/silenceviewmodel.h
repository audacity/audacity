/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/generatoreffectmodel.h"
#include "../common/params.h"

namespace au::effects {
class SilenceEffect;
class SilenceViewModel : public GeneratorEffectModel
{
    Q_OBJECT

private:
    SilenceEffect* effect() const;
};
}
