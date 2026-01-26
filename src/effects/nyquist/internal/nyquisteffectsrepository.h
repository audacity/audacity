/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../inyquisteffectsrepository.h"
#include "au3-nyquist-effects/LoadNyquist.h"

namespace au::effects {
class NyquistEffectsRepository : public INyquistEffectsRepository
{
public:
    NyquistEffectsRepository() = default;

    EffectMetaList effectMetaList() const override;

private:
    // This member forces the linker to include LoadNyquist.cpp,
    // which contains DECLARE_BUILTIN_PROVIDER for the Nyquist module
    ::NyquistEffectsModule m_module;
};
}
