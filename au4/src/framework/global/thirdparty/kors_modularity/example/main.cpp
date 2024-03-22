#include <iostream>
#include <vector>

#include "app.h"
#include "alpha/alphamodule.h"
#include "vita/vitamodule.h"

int main(int argc, char* argv[])
{
    std::cout << "Hello World, I am Modularity" << std::endl;

    app::App app;

    //! NOTE Declaration of modules
    app.addModule(new app::alpha::AlphaModule());
    app.addModule(new app::vita::VitaModule());

    //! NOTE Run app
    return app.run(argc, argv);
}
