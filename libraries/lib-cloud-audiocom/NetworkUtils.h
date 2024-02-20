/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  NetworkUtils.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <atomic>
#include <chrono>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

namespace audacity::network_manager
{
class IResponse;
class Request;
} // namespace audacity::network_manager

namespace cloud::audiocom
{
struct CLOUD_AUDIOCOM_API TransferStats final
{
   using Duration = std::chrono::milliseconds;

   int64_t BytesTransferred {};
   int64_t BlocksTransferred {};
   int64_t ProjectFilesTransferred {};

   Duration TransferDuration {};

   TransferStats& SetBytesTransferred(int64_t bytesTransferred);
   TransferStats& SetBlocksTransferred(int64_t blocksTransferred);
   TransferStats& SetProjectFilesTransferred(int64_t projectFilesTransferred);
   TransferStats& SetTransferDuration(Duration transferDuration);
}; // struct TransferStats

struct CLOUD_AUDIOCOM_API CancellationContext final
{
   struct Tag final
   {
   };

public:
   explicit CancellationContext(Tag);

   CancellationContext(const CancellationContext&)            = delete;
   CancellationContext(CancellationContext&&)                 = delete;
   CancellationContext& operator=(const CancellationContext&) = delete;
   CancellationContext& operator=(CancellationContext&&)      = delete;

   [[nodiscard]] static std::shared_ptr<CancellationContext> Create();

   void Cancel();

   void OnCancelled(std::function<void()> callback);
   void
   OnCancelled(std::shared_ptr<audacity::network_manager::IResponse> response);

private:
   std::atomic<bool> mCancelled { false };

   std::mutex mDeferredCallbacksMutex;
   std::vector<std::function<void()>> mDeferredCallbacks;
}; // struct CancellationContext

enum class ResponseResultCode
{
   Success,
   Cancelled,
   Expired,
   Conflict,
   ConnectionFailed,
   PaymentRequired,
   TooLarge,
   Unauthorized,
   Forbidden,
   NotFound,
   UnexpectedResponse,
   InternalClientError,
   UnknownError,
}; // enum class ResponseResultCode

struct ResponseResult final
{
   ResponseResultCode Code { ResponseResultCode::Success };
   std::string Content;
}; // struct ResponseResult

[[nodiscard]] ResponseResult GetResponseResult(
   audacity::network_manager::IResponse& response, bool readBody);

void SetCommonHeaders(audacity::network_manager::Request& request);
} // namespace cloud::audiocom
