#include "vitamodule.h"

#include "modularity/ioc.h"

#include "internal/vitaservice.h"

using namespace app::vita;

std::string VitaModule::moduleName() const
{
    return "vita";
}

void VitaModule::registerExports()
{
    modularity::ioc()->registerExport<IVitaService>(moduleName(), new VitaService());
}
