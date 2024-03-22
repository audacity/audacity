#ifndef EXAMPLE_VITASERVICE_H
#define EXAMPLE_VITASERVICE_H

#include "../ivitaservice.h"

#include "modularity/ioc.h"
#include "alpha/ialphaservice.h"

namespace app::vita {
class VitaService : public IVitaService
{
    modularity::Inject<alpha::IAlphaService> alphaService;

public:
    VitaService() = default;

    std::string doSomeThing() const override;
    std::string doSomeThingWithAlpha() const override;
};
}

#endif // EXAMPLE_VITASERVICE_H
