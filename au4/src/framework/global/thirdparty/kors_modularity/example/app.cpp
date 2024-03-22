#include <iostream>

#include "app.h"

using namespace app;

App::~App()
{
    for (modularity::IModuleSetup* m : m_modules) {
        delete m;
    }
}

void App::addModule(modularity::IModuleSetup* m)
{
    m_modules.push_back(m);
}

int App::run(int argc, char** argv)
{
    //! NOTE Modules setup
    //! First call registerExports for all modules
    for (modularity::IModuleSetup* m : m_modules) {
        m->registerExports();
    }

    for (modularity::IModuleSetup* m : m_modules) {
        m->resolveImports();
        m->registerResources();
        m->registerUiTypes();
        m->onPreInit();
    }

    for (modularity::IModuleSetup* m : m_modules) {
        m->onInit();
    }

    for (modularity::IModuleSetup* m : m_modules) {
        m->onAllInited();
    }

    //! NOTE Run app (main event loop...)
    {
        std::string data = vitaService()->doSomeThing();
        std::cout << "vita data: " << data << std::endl;

        std::string data2 = vitaService()->doSomeThingWithAlpha();
        std::cout << "vita data (with alpha): " << data2 << std::endl;
    }

    //! NOTE Deinit
    for (modularity::IModuleSetup* m : m_modules) {
        m->onDeinit();
    }

    for (modularity::IModuleSetup* m : m_modules) {
        m->onDestroy();
    }

    return 0;
}
