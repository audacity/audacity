/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TaskExecutionHandler.cpp

  Dmitry Makarenko

**********************************************************************/

#include <wx/app.h>

#include "AppEvents.h"
#include "CloudAudioOpenUtils.h"
#include "CloudProjectOpenUtils.h"
#include "ExportUtils.h"
#include "TaskExecutionService.h"
#include "OAuthService.h"
#include "sync/CloudSyncDTO.h"

namespace audacity::cloud::audiocom
{
namespace
{
class TaskExecutionHandler final
{
public:
   TaskExecutionHandler()
   {
      AppEvents::OnAppInitialized([this]
      {
         mTaskSubscription = GetTaskExecutionService().Subscribe(
            [this](const AppTaskReceivedMessage& message)
            {
               HandleTask(message.Task);
            });

         mAuthSubscription = GetOAuthService().Subscribe(
            [this](const AuthStateChangedMessage& message)
            {
               HandleAuthStateChanged(message);
            });

         if (GetOAuthService().HasAccessToken())
         {
            GetTaskExecutionService().Start();
         }
         else {
            // Force validation to start polling on startup
            GetOAuthService().ValidateAuth(nullptr, AudiocomTrace::TaskService, true);
         }
      });
   }

   ~TaskExecutionHandler() = default;

private:
   void HandleTask(const sync::AppTask& task)
   {
      try
      {
         bool success = false;
         std::string errorMessage;

         switch (task.Action)
         {
         case sync::TaskAction::OpenAudio:
            success = HandleOpenAudio(task, errorMessage);
            break;

         case sync::TaskAction::OpenProject:
            success = HandleOpenProject(task, errorMessage);
            break;

         case sync::TaskAction::Unknown:
         default:
            errorMessage = "Unknown task type";
            break;
         }

         auto& service = GetTaskExecutionService();
         if (success)
         {
            service.SendResult(
               task.Id, sync::TaskStatus::Success, {});
         }
         else
         {
            service.SendResult(
               task.Id, sync::TaskStatus::Failed, errorMessage);
         }
      }
      catch (const std::exception& e)
      {
         GetTaskExecutionService().SendResult(
            task.Id, sync::TaskStatus::Failed, e.what());
      }
      catch (...)
      {
         GetTaskExecutionService().SendResult(
            task.Id, sync::TaskStatus::Failed,
            "Unknown error occurred while executing task");
      }
   }

   bool HandleOpenAudio(const sync::AppTask& task, std::string& errorMessage)
   {
      const auto* taskData = std::get_if<sync::AudioOpenTaskParameters>(&task.Parameters);
      if (!taskData)
      {
         errorMessage = "Audio ID is missing";
         return false;
      }

      auto* project = sync::OpenAudioFromCloud(taskData->AudioId);
      if (!project)
      {
         errorMessage = "Failed to open audio";
         return false;
      }

      return true;
   }

   bool HandleOpenProject(const sync::AppTask& task, std::string& errorMessage)
   {
      const auto* taskData = std::get_if<sync::ProjectOpenTaskParameters>(&task.Parameters);
      if (!taskData)
      {
         errorMessage = "Project ID is missing";
         return false;
      }

      auto* potentialTarget = sync::GetPotentialTarget();

      auto* project = sync::OpenProjectFromCloud(
         potentialTarget, taskData->ProjectId, taskData->SnapshotId, false);

      if (!project)
      {
         errorMessage = "Failed to open project";
         return false;
      }

      return true;
   }

   void HandleAuthStateChanged(const AuthStateChangedMessage& message)
   {
      auto& service = GetTaskExecutionService();

      if (message.authorised)
      {
         if (!service.IsRunning())
         {
            service.Start();
         }
      }
      else
      {
         if (service.IsRunning())
         {
            service.Stop();
         }
      }
   }

   Observer::Subscription mTaskSubscription;
   Observer::Subscription mAuthSubscription;
};

static TaskExecutionHandler sTaskExecutionHandler;

} // namespace
} // namespace audacity::cloud::audiocom
