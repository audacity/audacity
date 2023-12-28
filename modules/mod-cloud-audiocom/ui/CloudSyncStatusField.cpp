/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncStatusField.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudSyncStatusField.h"

#include <wx/statusbr.h>

#include "Project.h"
#include "ProjectStatus.h"
#include "ProjectWindow.h"
#include "sync/ProjectCloudExtension.h"

namespace cloud::audiocom::sync
{
namespace
{
const StatusBarField FieldId { L"CloudSyncStatus" };

const AttachedProjectObjects::RegisteredFactory key {
   [](AudacityProject& project)
   { return std::make_shared<CloudSyncStatusField>(project); }
};

class CloudSyncStatusBarFieldItem final : public StatusBarFieldItem
{
public:
   CloudSyncStatusBarFieldItem ()
       : StatusBarFieldItem { FieldId }
   {
      
   }

   int GetDefaultWidth(const AudacityProject& project) const override
   {
      return CloudSyncStatusField::Get(project).GetWidth();
   }

   void OnSize(AudacityProject& project) override
   {
      const auto index = ProjectStatusFieldsRegistry::GetFieldIndex(project, name);

      if (index < 0)
         return;
      
      wxRect rect;
      if (ProjectWindow::Get(project).GetStatusBar()->GetFieldRect(index, rect))
         CloudSyncStatusField::Get(project).OnSize(rect.Deflate(2));
   }

   void
   SetText(AudacityProject& project, const TranslatableString& msg) override
   {
   }

   TranslatableString
   GetText(const AudacityProject& project) const override
   {      
      return CloudSyncStatusField::Get(project).GetText();
   }

   bool IsVisible (const AudacityProject& project) const override
   {
      return CloudSyncStatusField::Get(project).IsVisible();
   }

   void MarkDirty(const AudacityProject& project)
   {
      DispatchFieldChanged(project);
   }
}; // class CloudSyncStatusBarFieldItem

StatusBarFieldItemRegistrator rateStatusBarField {
   std::make_unique<CloudSyncStatusBarFieldItem>(),
   { {}, { Registry::OrderingHint::After, RateStatusBarField().GET() } }
};

} // namespace

CloudSyncStatusField::CloudSyncStatusField(AudacityProject& project)
    : mProject { project }
    , mCloudExtension { ProjectCloudExtension::Get(project) }
{
}

CloudSyncStatusField::~CloudSyncStatusField() = default;

CloudSyncStatusField& CloudSyncStatusField::Get(AudacityProject& project)
{
   return project.AttachedObjects::Get<CloudSyncStatusField&>(key);
}

const CloudSyncStatusField&
CloudSyncStatusField::Get(const AudacityProject& project)
{
   return Get(const_cast<AudacityProject&>(project));
}

int CloudSyncStatusField::GetWidth() const
{
   return mCloudExtension.IsCloudProject() ? 100 : 0;
}

void CloudSyncStatusField::OnSize(const wxRect& rect)
{
}

bool CloudSyncStatusField::IsVisible () const
{
   return mCloudExtension.IsCloudProject();
}

TranslatableString CloudSyncStatusField::GetText() const
{
   switch (mState)
   {  
   case State::Synced:
      return XO("Synced");
   case State::Failed:
      return XO("Failed");
   case State::Uploading:
      return XO("Uploading %d%%").Format(mProgress);
   default:
      break;
   }
}

void CloudSyncStatusField::SetUploadProgress(double progress)
{
   const int newProgress = static_cast<int>(progress * 100);

   bool stateChanged = false;

   if (mState != State::Uploading)
   {
      mState = State::Uploading;
      stateChanged = true;
   }

   if (mProgress != newProgress)
   {
      mProgress = newProgress;
      stateChanged = true;
   }

   if (stateChanged)
      MarkDirty();
}

void CloudSyncStatusField::UploadCompleted(bool successful)
{
   if (successful)
      mState = State::Synced;
   else
      mState = State::Failed;

   MarkDirty();
}

void CloudSyncStatusField::MarkDirty()
{
   auto field = dynamic_cast<CloudSyncStatusBarFieldItem*>(
      ProjectStatusFieldsRegistry::Get(FieldId));
   if (field)
      field->MarkDirty(mProject);
}
} // namespace cloud::audiocom::sync
