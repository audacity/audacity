/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsmodule.h"

#include "internal/nyquisteffectsrepository.h"

using namespace au::effects;

std::string NyquistEffectsModule::moduleName() const
{
    return "effects_nyquist";
}

void NyquistEffectsModule::registerExports()
{
    m_nyquistEffectsRepository = std::make_shared<NyquistEffectsRepository>();

    ioc()->registerExport<INyquistEffectsRepository>(moduleName(), m_nyquistEffectsRepository);
}

void NyquistEffectsModule::resolveImports()
{
}
