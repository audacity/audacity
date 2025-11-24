#pragma once

#include "../common/generatoreffect.h"
#include "au3-builtin-effects/SilenceBase.h"

namespace au::effects {
class SilenceGenerator : public SilenceBase, public GeneratorEffect
{
public:
    SilenceGenerator();
};
}
