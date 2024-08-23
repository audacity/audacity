/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::effects {
class TempConceptExecutor
{
    muse::Inject<context::IGlobalContext> globalContext;

public:
    TempConceptExecutor() = default;

    void execute(const std::string& effectId);
};
}
