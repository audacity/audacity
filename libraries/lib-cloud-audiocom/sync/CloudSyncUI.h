/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncUI.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string>

class AudacityProject;

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

struct SaveResult
{
   std::string Title;
   int PreviewSaveFrequency { 0 };
   CloudProjectVisibility Visiblity {
      CloudProjectVisibility::Private
   };
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
   virtual void OnDownloadFailed(std::string errorMessage, bool verbose) = 0;
   virtual AudacityProject* OnDownloadSucceeded(
      AudacityProject* targetProject, const std::string& path) = 0;
}; // class CloudSyncUI

} // namespace cloud::audiocom::sync
