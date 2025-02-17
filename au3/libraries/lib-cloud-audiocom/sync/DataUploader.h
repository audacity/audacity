/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DataUploader.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <mutex>
#include <vector>

#include "CloudSyncDTO.h"
#include "NetworkUtils.h"

#include "concurrency/CancellationContext.h"

namespace audacity::cloud::audiocom {
class ServiceConfig;
}

namespace audacity::cloud::audiocom::sync {
using concurrency::CancellationContextPtr;

class DataUploader final
{
    DataUploader() = default;
    ~DataUploader();

    DataUploader(const DataUploader&)            = delete;
    DataUploader(DataUploader&&)                 = delete;
    DataUploader& operator=(const DataUploader&) = delete;
    DataUploader& operator=(DataUploader&&)      = delete;

public:
    static DataUploader& Get();

    void Upload(
        CancellationContextPtr cancellationContex, const ServiceConfig& config, const UploadUrls& target, std::vector<uint8_t> data,
        std::function<void(ResponseResult)> callback, std::function<void(double)> progressCallback = {});

    void Upload(
        CancellationContextPtr cancellationContex, const ServiceConfig& config, const UploadUrls& target, std::string filePath,
        std::function<void(ResponseResult)> callback, std::function<void(double)> progressCallback = {});

private:
    struct UploadOperation;

    void RemoveResponse(UploadOperation& response);

    std::mutex mResponseMutex;

    using ResponsesList = std::vector<std::shared_ptr<UploadOperation> >;
    ResponsesList mResponses;
};
} // namespace audacity::cloud::audiocom::sync
