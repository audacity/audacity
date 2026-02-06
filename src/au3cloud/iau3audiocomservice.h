/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/promise.h"

#include "cloudtypes.h"

namespace au::au3cloud {
class IAu3AudioComService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAu3AudioComService)

public:
    virtual ~IAu3AudioComService() = default;

    virtual muse::async::Promise<ProjectList> downloadProjectList(size_t projectsPerBatch, size_t batchNumber) = 0;
    virtual muse::async::Promise<AudioList> downloadAudioList(size_t audiosPerBatch, size_t batchNumber) = 0;
};
}
