/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "context/iglobalcontext.h"
#include "macromanager/imacrorunner.h"
#include "macromanager/irunonfilesrequests.h"

namespace au::macromanager {
class RunOnFilesStarter : public muse::Contextable, public muse::async::Asyncable
{
    muse::GlobalInject<IRunOnFilesRequests> requests;

    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<IMacroRunner> runner{ this };

public:
    explicit RunOnFilesStarter(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

private:
    void startIfRequested();
};
}
