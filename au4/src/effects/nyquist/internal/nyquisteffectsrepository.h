/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../inyquisteffectsrepository.h"

namespace au::effects {
class NyquistEffectsRepository : public INyquistEffectsRepository
{
public:
    NyquistEffectsRepository() = default;

    EffectMetaList effectMetaList() const override;
};
}
