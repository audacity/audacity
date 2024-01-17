/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DataUploader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "DataUploader.h"

#include <variant>

#include <wx/file.h>

#include "CodeConversions.h"

#include "IResponse.h"
#include "Request.h"
#include "NetworkManager.h"

#include "RequestPayload.h"

#include "BasicUI.h"

using namespace audacity::network_manager;

namespace cloud::audiocom::sync
{
using UploadData = std::variant<std::vector<uint8_t>, std::string>;

struct DataUploader::Response final
{
   DataUploader& Uploader;
   UploadUrls Target;
   std::function<void(UploadResult)> Callback;
   std::function<bool(double)> ProgressCallback;

   int RetriesCount { 3 };
   int RetriesLeft { 3 };

   std::string MimeType;
   UploadData Data;
   std::shared_ptr<IResponse> NetworkResponse;

   bool UploadFailed { false };

   UploadResultCode CurrentResult { UploadResultCode::Success };

   Response(
      DataUploader& uploader, const UploadUrls& target, UploadData data,
      std::string mimeType, std::function<void(UploadResult)> callback,
      std::function<bool(double)> progressCallback)
       : Uploader { uploader }
       , Target { target }
       , Callback { std::move(callback) }
       , ProgressCallback { std::move(progressCallback) }
       , RetriesLeft { RetriesCount }
       , MimeType { std::move(mimeType) }
       , Data { std::move(data) }
   {
      PerformUpload();
   }

   void PerformUpload()
   {
      Request request { Target.UploadUrl };
      request.setHeader(
         common_headers::ContentType,
         MimeType);

      if (std::holds_alternative<std::vector<uint8_t>>(Data))
      {
         auto data = *std::get_if<std::vector<uint8_t>>(&Data);

         NetworkResponse = NetworkManager::GetInstance().doPut(
            request, data.data(), data.size());
      }
      else
      {
         auto filePath = *std::get_if<std::string>(&Data);

         NetworkResponse = NetworkManager::GetInstance().doPut(
            request, CreateRequestPayloadStream(filePath));

      }

      NetworkResponse->setRequestFinishedCallback(
         [this](auto)
         {
            if (NetworkResponse->getError() == NetworkError::NoError)
               OnUploadSucceeded();
            else
               OnUploadFailed();
         });

      NetworkResponse->setUploadProgressCallback(
         [this](int64_t current, int64_t total)
         {
            if (total <= 0)
            {
               total = 1;
               current = 0;
            }

            if (!ProgressCallback(static_cast<double>(current) / total))
               NetworkResponse->abort();
         });
   }

   void OnUploadSucceeded()
   {
      RetriesLeft = RetriesCount;
      ConfirmUpload();
   }

   UploadResultCode GuessResultCode() const
   {
      const auto error = NetworkResponse->getError();

      if (error == NetworkError::NoError)
         return UploadResultCode::Success;
      else if (error == NetworkError::OperationCancelled)
         return UploadResultCode::Cancelled;
      else if (
         error == NetworkError::ConnectionFailed ||
         error == NetworkError::ConnectionRefused ||
         error == NetworkError::HostNotFound ||
         error == NetworkError::ProxyConnectionFailed ||
         error == NetworkError::ProxyNotFound)
         return UploadResultCode::ConnectionFailed;
      else if (error == NetworkError::HTTPError)
         return NetworkResponse->getHTTPCode() == HttpCode::Conflict ?
                   UploadResultCode::Conflict :
                   UploadResultCode::Expired;

      return UploadResultCode::UnknownError;
   }

   void OnUploadFailed()
   {
      CurrentResult = GuessResultCode();

      if (
         CurrentResult == UploadResultCode::ConnectionFailed &&
         --RetriesLeft > 0)
      {
         PerformUpload();
      }
      else
      {
         RetriesLeft = RetriesCount;
         FailUpload();
      }
   }

   void ConfirmUpload()
   {
      Data = {};
      Request request { Target.SuccessUrl };

      NetworkResponse = NetworkManager::GetInstance().doPost(request, nullptr, 0);

      NetworkResponse->setRequestFinishedCallback(
         [this](auto)
         {
            const auto code = GuessResultCode();

            if (code == UploadResultCode::Success)
            {
               Callback(UploadResult { UploadResultCode::Success, {} });
               CleanUp();
            }
            else if (
               code == UploadResultCode::ConnectionFailed && --RetriesLeft > 0)
            {
               ConfirmUpload();
            }
            else
            {
               RetriesLeft = RetriesCount;
               FailUpload();
            }
         });
   }

   void FailUpload()
   {
      if (!UploadFailed)
      {
         Data = {};

         Callback(UploadResult { CurrentResult,
                                 NetworkResponse->readAll<std::string>() });
         UploadFailed = true;
      }

      Request request { Target.FailUrl };

      NetworkResponse = NetworkManager::GetInstance().doPost(request, nullptr, 0);

      NetworkResponse->setRequestFinishedCallback(
         [this](auto)
         {
            const auto code = GuessResultCode();

            if (code == UploadResultCode::ConnectionFailed && --RetriesLeft > 0)
               FailUpload();
            else
               CleanUp();

            // Ignore other errors, server will collect garbage
            // and delete the file eventually
         });
   }

   void Cancel()
   {
      if (NetworkResponse)
         NetworkResponse->abort();
   }

   void CleanUp()
   {
      BasicUI::CallAfter([this]() { Uploader.RemoveResponse(*this); });
   }
};

DataUploader::~DataUploader()
{
   CancelAll();
}

DataUploader& DataUploader::Get()
{
   static DataUploader instance;
   return instance;
}

void DataUploader::CancelAll()
{
   ResponsesList responses;

   {
      auto lock = std::lock_guard { mResponseMutex };
      responses = std::move(mResponses);
   }

   for (auto& response : responses)
      response->Cancel();
}

void DataUploader::Upload(
   const ServiceConfig&, const UploadUrls& target, std::vector<uint8_t> data,
   std::function<void(UploadResult)> callback,
   std::function<bool(double)> progressCallback)
{
   if (!callback)
      callback = [](auto...) {};

   if (!progressCallback)
      progressCallback = [](auto...) { return true; };

   auto lock = std::lock_guard { mResponseMutex };

   mResponses.emplace_back(std::make_unique<Response>(
      *this, target, std::move(data),
      audacity::network_manager::common_content_types::ApplicationXOctetStream,
      std::move(callback), std::move(progressCallback)));
}

void DataUploader::Upload(
   const ServiceConfig& config, const UploadUrls& target, std::string filePath,
   std::function<void(UploadResult)> callback,
   std::function<bool(double)> progressCallback)
{
   if (!callback)
      callback = [](auto...) {};

   if (!progressCallback)
      progressCallback = [](auto...) { return true; };

   if (!wxFileExists(audacity::ToWXString(filePath)))
   {
      if (callback)
         callback(UploadResult {
            UploadResultCode::UnknownError,
            audacity::ToUTF8(XO("File not found").Translation()) });

      return;
   }

   auto lock = std::lock_guard { mResponseMutex };

   mResponses.emplace_back(std::make_unique<Response>(
      *this, target, std::move(filePath),
      audacity::network_manager::common_content_types::ApplicationXOctetStream,
      std::move(callback), std::move(progressCallback)));
}

void DataUploader::RemoveResponse(Response& response)
{
   auto lock = std::lock_guard { mResponseMutex };

   mResponses.erase(
      std::remove_if(
         mResponses.begin(), mResponses.end(),
         [&response](const auto& item) { return item.get() == &response; }),
      mResponses.end());
}
} // namespace cloud::audiocom::sync
