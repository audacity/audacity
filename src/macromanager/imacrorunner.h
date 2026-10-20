/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/notification.h"
#include "framework/global/async/promise.h"
#include "framework/global/io/path.h"
#include "framework/global/types/ret.h"

#include "macromanagertypes.h"

namespace au::macromanager {
class IMacroRunner : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IMacroRunner)

public:
    virtual ~IMacroRunner() = default;

    virtual muse::async::Promise<muse::Ret> run(const Macro& macro) = 0;
    virtual muse::async::Promise<muse::Ret> runOnFiles(const Macro& macro, const muse::io::paths_t& files) = 0;
    virtual void abort() = 0;

    virtual bool isRunning() const = 0;
    virtual muse::async::Notification isRunningChanged() const = 0;

    virtual size_t currentFileIndex() const = 0;
    virtual muse::async::Notification currentFileChanged() const = 0;
};
}
