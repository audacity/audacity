/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DataUploader.cpp

  Dmitry Vedenko

**********************************************************************/

#include "DataUploader.h"

#include "IResponse.h"
#include "Request.h"
#include "NetworkManager.h"

#include "BasicUI.h"

using namespace audacity::network_manager;

namespace cloud::audiocom::sync
{
struct DataUploader::Response final
{
   DataUploader& Uploader;
   UploadUrls Target;
   std::function<void(UploadResult)> Callback;

   int RetriesCount { 3 };
   int RetriesLeft { 3 };

   std::vector<uint8_t> Data;
   std::shared_ptr<IResponse> NetworkResponse;

   bool UploadFailed { false };

   UploadResultCode CurrentResult { UploadResultCode::Success };

   Response(
      DataUploader& uploader, const UploadUrls& target,
      std::vector<uint8_t> data, std::function<void(UploadResult)> callback)
       : Uploader { uploader }
       , Target { target }
       , Callback { std::move(callback) }
       , RetriesLeft { RetriesCount }
       , Data { std::move(data) }
   {
      PerformUpload();
   }

   void PerformUpload()
   {
      Request request { Target.UploadUrl };
      request.setHeader(
         common_headers::ContentType,
         common_content_types::ApplicationXOctetStream);

      NetworkResponse = NetworkManager::GetInstance().doPut (
         request, Data.data(), Data.size());

      NetworkResponse->setRequestFinishedCallback(
         [this](auto)
         {
            if (NetworkResponse->getError() == NetworkError::NoError)
               OnUploadSucceeded();
            else
               OnUploadFailed();
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
   std::function<void(UploadResult)> callback)
{
   auto lock = std::lock_guard { mResponseMutex };

   mResponses.emplace_back(std::make_unique<Response>(
      *this, target, std::move(data), std::move(callback)));
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
