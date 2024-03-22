#include "alphamodule.h"

#include "modularity/ioc.h"

#include "internal/alphaservice.h"

using namespace app::alpha;

std::string AlphaModule::moduleName() const
{
    return "alpha";
}

void AlphaModule::registerExports()
{
    modularity::ioc()->registerExport<IAlphaService>(moduleName(), new AlphaService());
}
