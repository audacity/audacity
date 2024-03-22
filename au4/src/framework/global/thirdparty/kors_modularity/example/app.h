#ifndef EXAMPLE_APP_H
#define EXAMPLE_APP_H

#include <vector>

#include "modularity/imodulesetup.h"

#include "modularity/ioc.h"
#include "vita/ivitaservice.h"

namespace app {
class App
{
    INJECT(vita::IVitaService, vitaService)

public:
    App() = default;
    ~App();

    void addModule(modularity::IModuleSetup* m);

    int run(int argc, char** argv);

private:

    std::vector<modularity::IModuleSetup*> m_modules;
};
}

#endif // EXAMPLE_APP_H
