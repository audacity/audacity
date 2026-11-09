/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/io/path.h"

#include "macromanagertypes.h"

namespace au::macromanager {
struct RunOnFilesRequest {
    Macro macro;
    muse::io::paths_t files;
};

class IRunOnFilesRequests : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IRunOnFilesRequests)

public:
    virtual ~IRunOnFilesRequests() = default;

    virtual void push(const muse::modularity::ContextPtr& ctx, const RunOnFilesRequest& request) = 0;
    virtual std::optional<RunOnFilesRequest> take(const muse::modularity::ContextPtr& ctx) = 0;
};
}
