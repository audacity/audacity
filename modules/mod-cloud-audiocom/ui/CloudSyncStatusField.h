/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncStatusField.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include "ClientData.h"

class AudacityProject;

class wxRect;

namespace cloud::audiocom::sync
{
class ProjectCloudExtension;
class CloudSyncStatusField final : public ClientData::Base
{
public:
   explicit CloudSyncStatusField(AudacityProject& project);
   ~CloudSyncStatusField() override;

   static CloudSyncStatusField& Get(AudacityProject& project);
   static const CloudSyncStatusField& Get(const AudacityProject& project);

   int GetWidth() const;
   void OnSize(const wxRect& rect);
   bool IsVisible() const;

   TranslatableString GetText() const;

   void SetUploadProgress(double progress);
   void UploadCompleted(bool successful);
private:
   void MarkDirty();

   AudacityProject& mProject;
   ProjectCloudExtension& mCloudExtension;

   enum class State
   {
      Synced,
      Failed,
      Uploading,
   } mState { State::Synced };

   int mProgress { 0 }; // Progress, 0-100
}; // class CloudSyncStatusField
} // namespace cloud::audiocom::sync
