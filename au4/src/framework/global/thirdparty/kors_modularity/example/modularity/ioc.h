#ifndef EXAMPLE_IOC_H
#define EXAMPLE_IOC_H

#include "../../modularity/ioc.h" // kors

namespace app::modularity {
using ModulesIoC = kors::modularity::ModulesIoC;

inline ModulesIoC* ioc()
{
    return kors::modularity::ModulesIoC::instance();
}

template<class I>
using Inject = kors::modularity::Inject<I>;


}

#endif // EXAMPLE_IOC_H
