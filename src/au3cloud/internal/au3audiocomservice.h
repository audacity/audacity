/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/async/promise.h"

#include "au3cloud/cloudtypes.h"
#include "au3cloud/iau3audiocomservice.h"

namespace au::au3cloud {
class Au3AudioComService : public IAu3AudioComService, public muse::async::Asyncable
{
public:
    muse::async::Promise<ProjectList> downloadProjectList(size_t projectsPerBatch, size_t batchNumber) override;
    muse::async::Promise<AudioList> downloadAudioList(size_t audiosPerBatch, size_t batchNumber) override;
};
}
