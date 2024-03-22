#ifndef EXAMPLE_IMODULESETUP_H
#define EXAMPLE_IMODULESETUP_H

#include "../../modularity/imodulesetup.h"

namespace app::modularity {
//! NOTE This is an example of an interface,
//! we can use it or just make our own to suit our needs.
//! This has nothing to do with IoC.
using IModuleSetup = kors::modularity::IModuleSetup;
}

#endif // EXAMPLE_IMODULESETUP_H
