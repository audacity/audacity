#ifndef EXAMPLE_IALPHASERVICE_H
#define EXAMPLE_IALPHASERVICE_H

#include <string>

#include "modularity/imoduleinterface.h"

namespace app::alpha {
class IAlphaService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(app::alpha::IAlphaService)

public:

    virtual ~IAlphaService() = default;

    virtual std::string info() const = 0;
};
}

#endif // EXAMPLE_IALPHASERVICE_H
