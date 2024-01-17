/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncUI.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string>

class AudacityProject;
class TranslatableString;

namespace BasicUI
{
class WindowPlacement;
} // namespace BasicUI

namespace cloud::audiocom::sync
{
enum class CloudProjectVisibility
{
   Private,
   Unlisted,
   Public,
};

enum class DownloadConflictResolution
{
   Local,
   Remote,
};

struct SaveResult
{
   std::string Title;

   CloudProjectVisibility Visiblity { CloudProjectVisibility::Private };

   bool SaveToCloud { false };
   bool Cancelled { true };
};

class CLOUD_AUDIOCOM_API CloudSyncUI /* not final */
{
public:
   virtual ~CloudSyncUI();

   virtual SaveResult OnHandleFirstSave(
      const AudacityProject& project,
      const BasicUI::WindowPlacement& placement) = 0;

   virtual SaveResult OnHandleSave(
      const AudacityProject& project,
      const BasicUI::WindowPlacement& placement) = 0;

   virtual bool
   OnAuthorizationRequired(const BasicUI::WindowPlacement& placement) = 0;

   virtual bool OnUploadProgress(
      AudacityProject* project, double progress) = 0;
   virtual void OnUploadFailed(
      AudacityProject* project, std::string errorMessage) = 0;
   virtual void OnUploadSucceeded(AudacityProject* project) = 0;

   virtual void OnDownloadStarted() = 0;
   virtual bool OnDownloadProgress(double progress) = 0;
   virtual void OnDownloadFinished() = 0;

   virtual void ShowDownloadError(std::string errorMessage) = 0;

   virtual DownloadConflictResolution
   OnDownloadConflict(const BasicUI::WindowPlacement& placement) = 0;

   virtual void OnMixdownStarted() = 0;
   virtual void SetMixdownProgressMessage(const TranslatableString& message) = 0;
   virtual bool OnMixdownProgress(double progress) = 0;
   virtual void OnMixdownFinished() = 0;
}; // class CloudSyncUI

} // namespace cloud::audiocom::sync
