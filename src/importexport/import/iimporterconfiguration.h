/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"
#include "modularity/imoduleinterface.h"

#include "types/importtypes.h"

#include <string>
#include <vector>

namespace au::importexport {
class IImporterConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IImporterConfiguration)

public:
    virtual ~IImporterConfiguration() = default;

    virtual std::vector<std::string> tempoDetectionWorkspaces() const = 0;
    virtual void setTempoDetectionWorkspaces(const std::vector<std::string>& workspaces) = 0;
    virtual muse::async::Notification tempoDetectionWorkspacesChanged() const = 0;

    virtual LoopAction emptyProjectLoopAction() const = 0;
    virtual void setEmptyProjectLoopAction(LoopAction action) = 0;

    virtual LoopAction subsequentImportLoopAction() const = 0;
    virtual void setSubsequentImportLoopAction(LoopAction action) = 0;
};
}
