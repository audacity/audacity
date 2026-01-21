/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsmodule.h"

#include "internal/nyquisteffectsrepository.h"

std::string au::effects::NyquistEffectsModule::moduleName() const
{
    return "effects_nyquist";
}

void au::effects::NyquistEffectsModule::registerExports()
{
    m_nyquistEffectsRepository = std::make_shared<NyquistEffectsRepository>();

    ioc()->registerExport<INyquistEffectsRepository>(moduleName(), m_nyquistEffectsRepository);
}

void au::effects::NyquistEffectsModule::resolveImports()
{
}
