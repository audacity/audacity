/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TaskExecutionService.h

  Dmitry Makarenko

**********************************************************************/
#pragma once

#include <atomic>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

#include <rapidjson/fwd.h>

#include "Observer.h"
#include "sync/CloudSyncDTO.h"

namespace audacity::cloud::audiocom
{

struct AppTaskReceivedMessage final
{
   sync::AppTask Task;
};

class CLOUD_AUDIOCOM_API TaskExecutionService final :
   public Observer::Publisher<AppTaskReceivedMessage>
{
public:
   TaskExecutionService();
   ~TaskExecutionService();

   void Start();
   void Stop();
   bool IsRunning() const;

   void SendResult(
      const std::string& taskId, sync::TaskStatus status,
      const std::string& errorMessage);

private:
   void PollingThread();
   void PerformPoll();
   void ProcessTask(const sync::AppTask& task);
   bool AcknowledgeTask(const std::string& taskId);

   std::atomic<bool> mRunning { false };
   std::atomic<bool> mStopRequested { false };
   std::atomic<int> mPollIntervalSeconds { 10 };
   std::atomic<bool> mPollingDisabled { false };
   std::atomic<bool> mPollImmediately { false };

   std::thread mPollingThread;
   std::mutex mMutex;

   std::vector<std::string> mProcessedTasks;
   bool mExecutingTask { false };
};

//! Returns the instance of the TaskExecutionService
CLOUD_AUDIOCOM_API TaskExecutionService& GetTaskExecutionService();

} // namespace audacity::cloud::audiocom
