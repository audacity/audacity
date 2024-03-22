#ifndef EXAMPLE_IVITASERVICE_H
#define EXAMPLE_IVITASERVICE_H

#include <string>

#include "modularity/imoduleinterface.h"

namespace app::vita {
class IVitaService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(app::vita::IVitaService)

public:

    virtual ~IVitaService() = default;

    virtual std::string doSomeThing() const = 0;
    virtual std::string doSomeThingWithAlpha() const = 0;
};
}

#endif // EXAMPLE_IVITASERVICE_H
