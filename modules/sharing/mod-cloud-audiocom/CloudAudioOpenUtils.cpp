/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudAudioOpenUtils.cpp

  Dmitry Vedenko / Dmitry Makarenko

**********************************************************************/
#include "CloudAudioOpenUtils.h"

#include <memory>
#include <wx/filefn.h>
#include <wx/log.h>

#include "BasicUI.h"
#include "ExportUtils.h"
#include "CloudSyncService.h"
#include "UriParser.h"

#include "AuthorizationHandler.h"

#include "ActiveProject.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "ProjectManager.h"
#include "ProjectFileManager.h"
#include "Track.h"
#include "WaveTrack.h"
#include "WaveClip.h"
#include "ui/dialogs/LinkFailedDialog.h"

namespace audacity::cloud::audiocom::sync
{

namespace {

auto MakeProgress()
{
   return BasicUI::MakeProgress(
      XO("Open from audio.com"), XO("Downloading audio"),
      BasicUI::ProgressShowCancel);
}

auto MakePoller(BasicUI::ProgressDialog& dialog)
{
   return [&dialog](double progress)
   {
      return dialog.Poll(static_cast<unsigned>(progress * 10000), 10000ULL) ==
             BasicUI::ProgressResult::Success;
   };
}

template<typename T>
T GetResult(std::future<T>& future)
{
   while (future.wait_for(std::chrono::milliseconds(50)) !=
          std::future_status::ready) {
            BasicUI::Yield();
            wxSafeYield();
          }

   return future.get();
}


AudacityProject* GetTargetProject()
{
   auto project = GetActiveProject().lock();
   if (!project || !ProjectManager::SafeToOpenProjectInto(*project)) {
      return ProjectManager::New();
   }
   return project.get();
}

void FixupProject(AudacityProject& project, const std::string& title)
{
   auto tracks = TrackList::Get(project).Any<WaveTrack>();

   if (!tracks.empty()) {
      auto firstTrack = *tracks.first;

      if (firstTrack->GetNumClips()) {
         firstTrack->GetClip(0)->SetName(title);
      }
   }

   project.SetProjectName(title);
   ProjectFileIO::Get(project).MarkTemporary();
   ProjectFileIO::Get(project).SetProjectTitle(-1);
}

}

AudacityProject* OpenAudioFromCloud(std::string_view audioId) {
   ASSERT_MAIN_THREAD();

   auto authResult =
      PerformBlockingAuth(nullptr, AudiocomTrace::OpenFromCloudMenu);

   if (authResult.Result != AuthResult::Status::Authorised)
   {
      LinkFailedDialog dialog { nullptr,
                                AudiocomTrace::OpenFromCloudMenu };
      dialog.ShowModal();
      return nullptr;
   }

   auto progressDialog = MakeProgress();

   auto future = CloudSyncService::Get().DownloadCloudAudio(
      audioId,
      MakePoller(*progressDialog));

   auto result = GetResult(future);

   progressDialog.reset();

   if (result.Status != DownloadAudioResult::StatusCode::Succeeded) {
      wxLogError(
         "Failed to download audio from cloud: %s",
         result.Result.Content);
      BasicUI::ShowErrorDialog( {},
         XO("Open from audio.com"),
         XO("Failed to download audio from cloud.\n%s")
            .Format(result.Result.Content),
         {}
         );
      return nullptr;
   }

   FilePath audioPath = result.AudioPath;

   if (!wxFileExists(audioPath)) {
      wxLogError(
         "Downloaded audio file does not exist: %s",
         audioPath);
      BasicUI::ShowErrorDialog( {},
         XO("Open from audio.com"),
         XO("Downloaded audio file does not exist."),
         {}
         );
      return nullptr;
   }

   auto project = GetTargetProject();

   ProjectFileManager::Get(*project).Import(audioPath, false);
   FixupProject(*project, result.Title);

   wxRemoveFile(audioPath);

   return project;
}

bool HandleAudioLink(std::string_view link) {
   ASSERT_MAIN_THREAD();

   const auto parsedUri = ParseUri(link);

   if (parsedUri.Scheme != "audacity" || parsedUri.Host != "open")
      return false;

   const auto queryParameters = ParseUriQuery(parsedUri.Query);

   if (queryParameters.empty())
      return false;

   const auto audioId = queryParameters.find("audioId");

   if (audioId == queryParameters.end())
      return false;

   OpenAudioFromCloud(audioId->second);

   return true;
}

} // namespace audacity::cloud::audiocom::sync
