/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TaskExecutionService.cpp

  Dmitry Makarenko

**********************************************************************/
#include "TaskExecutionService.h"

#include <algorithm>
#include <thread>

#include <rapidjson/document.h>
#include <rapidjson/writer.h>

#include "BasicUI.h"
#include "CloudLibrarySettings.h"
#include "IResponse.h"
#include "NetworkManager.h"
#include "NetworkUtils.h"
#include "OAuthService.h"
#include "Request.h"
#include "ServiceConfig.h"
#include "sync/CloudSyncDTO.h"

namespace audacity::cloud::audiocom
{

namespace {

std::string TaskStatusToString(sync::TaskStatus status)
{
   switch (status)
   {
      case sync::TaskStatus::Success:
         return "success";
      case sync::TaskStatus::Started:
         return "started";
      case sync::TaskStatus::Failed:
         return "error";
      case sync::TaskStatus::Pending:
         return "pending";
      default:
         return "unknown";
   }
}

} // namespace

TaskExecutionService::TaskExecutionService()
{
}

TaskExecutionService::~TaskExecutionService()
{
   mStopRequested = true;

   if (mPollingThread.joinable())
   {
      auto start = std::chrono::steady_clock::now();
      while (mRunning &&
             std::chrono::steady_clock::now() - start < std::chrono::seconds(2))
      {
         std::this_thread::sleep_for(std::chrono::milliseconds(100));
      }

      if (mPollingThread.joinable())
      {
         if (mRunning)
         {
            mPollingThread.detach();
         }
         else
         {
            mPollingThread.join();
         }
      }
   }
}

void TaskExecutionService::Start()
{
   if (!TaskPollingEnabled.Read())
      return;

   auto& oauthService = GetOAuthService();
   if (!oauthService.HasAccessToken())
      return;

   std::lock_guard<std::mutex> lock(mMutex);

   if (mRunning)
      return;

   // Thread exited previously (eg mPollingDisabled) but not joined
   if (mPollingThread.joinable())
   {
      mPollingThread.join();
   }

   mRunning = true;
   mStopRequested = false;
   mPollingDisabled = false;

   mPollingThread = std::thread(&TaskExecutionService::PollingThread, this);
}

void TaskExecutionService::Stop()
{
   mStopRequested = true;

   if (mPollingThread.joinable())
   {
      auto start = std::chrono::steady_clock::now();
      while (mRunning &&
             std::chrono::steady_clock::now() - start < std::chrono::seconds(5))
      {
         std::this_thread::sleep_for(std::chrono::milliseconds(100));
      }

      if (mPollingThread.joinable())
      {
         mPollingThread.join();
      }
   }

   mRunning = false;
}

bool TaskExecutionService::IsRunning() const
{
   return mRunning;
}

void TaskExecutionService::PollingThread()
{
   while (!mStopRequested && !mPollingDisabled)
   {
      if (!TaskPollingEnabled.Read())
      {
         mStopRequested = true;
         break;
      }

      auto& oauthService = GetOAuthService();
      if (!oauthService.HasAccessToken())
      {
         mStopRequested = true;
         break;
      }

      PerformPoll();

      const auto intervalSeconds = mPollIntervalSeconds.load();
      const auto deadline = std::chrono::steady_clock::now() +
                               std::chrono::seconds(intervalSeconds);

      while (!mStopRequested && !mPollImmediately &&
          std::chrono::steady_clock::now() < deadline)
      {
         std::this_thread::sleep_for(std::chrono::milliseconds(500));
      }

      mPollImmediately = false;
   }

   mRunning = false;
}

void TaskExecutionService::PerformPoll()
{
   using namespace audacity::network_manager;

   try
   {
      auto& serviceConfig = GetServiceConfig();
      auto& oauthService = GetOAuthService();

      Request request(serviceConfig.GetTaskPollUrl());

      request.setHeader(
         common_headers::ContentType, common_content_types::ApplicationJson);
      request.setHeader(
         common_headers::Accept, common_content_types::ApplicationJson);

      SetCommonHeaders(request);
      SetOptionalHeaders(request);

      auto response = NetworkManager::GetInstance().doGet(request);

      bool responseReceived = false;
      std::string responseBody;
      int httpCode = 0;

      response->setRequestFinishedCallback(
         [&responseReceived, &responseBody, &httpCode](auto response)
         {
            httpCode = response->getHTTPCode();
            responseBody = response->template readAll<std::string>();
            responseReceived = true;
         });

      const int maxWaitSeconds = 30;
      for (int i = 0; i < maxWaitSeconds && !responseReceived && !mStopRequested; ++i)
      {
         std::this_thread::sleep_for(std::chrono::seconds(1));
      }

      if (!responseReceived || mStopRequested)
         return;

      if (httpCode != 200)
      {
         // Continue polling on error
         return;
      }

      auto pollResponse = sync::DeserializeAppTaskPollResponse(responseBody);
      if (!pollResponse)
      {
         // Continue polling on error
         return;
      }

      mPollIntervalSeconds = pollResponse->Polling.IntervalSeconds;
      mPollingDisabled = pollResponse->Polling.Stop;

      if (mPollingDisabled)
      {
         mStopRequested = true;
         return;
      }

      for (const auto& task : pollResponse->Tasks)
      {
         ProcessTask(task);
      }
   }
   catch (...)
   {

   }
}

void TaskExecutionService::ProcessTask(const sync::AppTask& task)
{
   std::lock_guard<std::mutex> lock(mMutex);

   // We process only one task at a time
   // The server is handing the queue, so we can safely ignore others
   if (mExecutingTask)
   {
      return;
   }

   // Safe check for already processed tasks
   auto it = std::find(mProcessedTasks.begin(), mProcessedTasks.end(), task.Id);
   if (it != mProcessedTasks.end())
   {
      return;
   }

   mProcessedTasks.push_back(task.Id);

   if (mProcessedTasks.size() > 100)
   {
      mProcessedTasks.erase(mProcessedTasks.begin(),
         mProcessedTasks.begin() + (mProcessedTasks.size() - 100));
   }

   mExecutingTask = true;

   bool acknowledged = AcknowledgeTask(task.Id);
   if (!acknowledged)
   {
      // Another instance took this task, or the task is no longer valid
      mExecutingTask = false;
      return;
   }

   BasicUI::CallAfter([this, task]()
   {
      Publish(AppTaskReceivedMessage { task });
   });
}

bool TaskExecutionService::AcknowledgeTask(const std::string& taskId)
{
   using namespace audacity::network_manager;
   using namespace rapidjson;

   try
   {
      auto& serviceConfig = GetServiceConfig();
      auto& oauthService = GetOAuthService();

      if (!oauthService.HasAccessToken())
         return false;

      StringBuffer buffer;

      Request request(serviceConfig.GetTaskAckUrl(taskId));

      request.setHeader(
         common_headers::ContentType, common_content_types::ApplicationJson);
      request.setHeader(
         common_headers::Accept, common_content_types::ApplicationJson);

      SetCommonHeaders(request);
      SetOptionalHeaders(request);

      auto response = NetworkManager::GetInstance().doPost(
         request, buffer.GetString(), buffer.GetSize());

      const int maxWaitSeconds = 10;
      for (int i = 0; i < maxWaitSeconds && !response->isFinished() && !mStopRequested; ++i)
      {
         std::this_thread::sleep_for(std::chrono::seconds(1));
      }

      if (!response->isFinished())
      {
         response->abort();
         return false;
      }

      int httpCode = response->getHTTPCode();
      return httpCode >= 200 && httpCode < 300;
   }
   catch (...)
   {
      return false;
   }
}

void TaskExecutionService::SendResult(
   const std::string& taskId, sync::TaskStatus status,
   const std::string& errorMessage)
{
   using namespace audacity::network_manager;
   using namespace rapidjson;

   try
   {
      auto& serviceConfig = GetServiceConfig();
      auto& oauthService = GetOAuthService();

      if (!oauthService.HasAccessToken())
         return;

      Document document;
      document.SetObject();

      auto statusStr = TaskStatusToString(status);
      document.AddMember(
         "status",
         Value(statusStr.c_str(), statusStr.size(), document.GetAllocator()),
         document.GetAllocator());

      if (status == sync::TaskStatus::Failed && !errorMessage.empty())
      {
         document.AddMember(
            "error",
            Value(errorMessage.c_str(), errorMessage.size(), document.GetAllocator()),
            document.GetAllocator());
      }
      else
      {
         document.AddMember(
            "error", Value().SetNull(), document.GetAllocator());
      }

      StringBuffer buffer;
      Writer<StringBuffer> writer(buffer);
      document.Accept(writer);

      Request request(serviceConfig.GetTaskResultUrl(taskId));

      request.setHeader(
         common_headers::ContentType, common_content_types::ApplicationJson);
      request.setHeader(
         common_headers::Accept, common_content_types::ApplicationJson);

      SetCommonHeaders(request);
      SetOptionalHeaders(request);

      auto response = NetworkManager::GetInstance().doPost(
         request, buffer.GetString(), buffer.GetSize());

      // No need to handle the response, even on error we can't do anything
   }
   catch (...)
   {
   }

   {
      std::lock_guard<std::mutex> lock(mMutex);
      mExecutingTask = false;
   }

   // Read out next task immediately
   mPollImmediately = true;
}

TaskExecutionService& GetTaskExecutionService()
{
   static TaskExecutionService service;
   return service;
}

} // namespace audacity::cloud::audiocom
