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

#include "CloudSyncUtils.h"

namespace cloud::audiocom
{
class ServiceConfig;
}

namespace cloud::audiocom::sync
{
enum class UploadResultCode
{
   Success,
   Cancelled,
   Expired,
   Conflict,
   ConnectionFailed,
   UnknownError,
};

struct UploadResult final
{
   UploadResultCode Code { UploadResultCode::Success };
   std::string ErrorMessage;
};

class DataUploader final
{
   DataUploader() = default;
   ~DataUploader();

   DataUploader(const DataUploader&) = delete;
   DataUploader(DataUploader&&) = delete;
   DataUploader& operator=(const DataUploader&) = delete;
   DataUploader& operator=(DataUploader&&) = delete;
public:
   static DataUploader& Get();

   void CancelAll();

   void Upload(
      const ServiceConfig& config, const UploadUrls& target,
      std::vector<uint8_t> data, std::function<void(UploadResult)> callback);

private:
   struct Response;

   void RemoveResponse(Response& response);

   std::mutex mResponseMutex;

   using ResponsesList = std::vector<std::unique_ptr<Response>>;
   ResponsesList mResponses;
};

} // namespace cloud::audiocom::sync
