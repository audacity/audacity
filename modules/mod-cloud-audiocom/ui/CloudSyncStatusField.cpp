/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncStatusField.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudSyncStatusField.h"

#include <wx/bitmap.h>
#include <wx/dcbuffer.h>
#include <wx/graphics.h>
#include <wx/statusbr.h>

#include <wx/log.h>

#include "Project.h"
#include "ProjectStatus.h"
#include "ProjectWindow.h"
#include "sync/ProjectCloudExtension.h"

#include "AllThemeResources.h"
#include "Theme.h"
#include "wxPanelWrapper.h"

#include "Prefs.h"

namespace audacity::cloud::audiocom::sync
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
   CloudSyncStatusBarFieldItem()
       : StatusBarFieldItem { FieldId }
   {
   }

   int GetDefaultWidth(const AudacityProject& project) const override
   {
      return CloudSyncStatusField::Get(project).GetWidth();
   }

   void OnSize(AudacityProject& project) override
   {
      const auto index =
         ProjectStatusFieldsRegistry::GetFieldIndex(project, name);

      wxLogDebug("CloudSyncStatusBarFieldItem::OnSize(%d)", index);

      if (index < 0)
         return;

      wxRect rect;
      if (ProjectWindow::Get(project).GetStatusBar()->GetFieldRect(index, rect))
         CloudSyncStatusField::Get(project).OnSize(rect);
   }

   void
   SetText(AudacityProject& project, const TranslatableString& msg) override
   {
   }

   TranslatableString GetText(const AudacityProject& project) const override
   {
      return CloudSyncStatusField::Get(project).GetText();
   }

   bool IsVisible(const AudacityProject& project) const override
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

const auto CloudSyncFailedMessage   = XO("Failed.");
const auto CloudSyncProgressMessage = XO("Syncing %d%%");
const auto Padding                  = 2;

} // namespace

class CloudSyncStatusField::StatusWidget final :
    public wxPanelWrapper,
    public PrefsListener
{
public:
   StatusWidget(CloudSyncStatusField& owner, wxWindow* parent)
       : wxPanelWrapper { parent }
       , mOwner { owner }
   {
      SetBackgroundStyle(wxBG_STYLE_PAINT);
      UpdatePrefs();

      Bind(wxEVT_PAINT, [this](auto&) { OnPaint(); });
   }

   ~StatusWidget() override
   {
   }

   void SetRect(const wxRect& rect)
   {
      SetSize(rect);
   }

   int GetPreferredWidth(State state) const
   {
      switch (state)
      {
      case State::Failed:
         return mSyncedBitmap->GetWidth() + mCloudSyncFailedMessageWidth +
                Padding * 4;
      case State::Uploading:
         return mProgressBitmap->GetWidth() + mCloudSyncProgressMessageWidth +
                Padding * 4;
      }

      return mSyncedBitmap->GetWidth() + Padding * 2;
   }

   const wxBitmap* GetBitmap() const
   {
      return mOwner.mState == State::Uploading ? mProgressBitmap :
                                                 mSyncedBitmap;
   }

   wxString GetText() const
   {
      if (mOwner.mState == State::Uploading)
         return TranslatableString { CloudSyncProgressMessage }
            .Format(mOwner.mProgress)
            .Translation();
      else if (mOwner.mState == State::Failed)
         return CloudSyncFailedMessage.Translation();

      return {};
   }

   void OnPaint()
   {
      wxAutoBufferedPaintDC dc(this);
      std::unique_ptr<wxGraphicsContext> gc(wxGraphicsContext::Create(dc));

      auto bitmap = GetBitmap();

      const wxSize widgetSize = GetSize();
      const wxSize bitmapSize = bitmap->GetSize();

      gc->SetBrush(wxBrush(GetBackgroundColour()));
      gc->DrawRectangle(0, 0, widgetSize.x, widgetSize.y);
      gc->DrawBitmap(
         *bitmap, Padding, (widgetSize.y - bitmapSize.y) / 2.0, bitmapSize.x,
         bitmapSize.y);

      const auto text = GetText();

      if (text.empty())
         return;

      gc->SetFont(GetFont(), GetForegroundColour());
      gc->DrawText(text, Padding + bitmapSize.x + 2 * Padding, 0);
   }

   void UpdatePrefs() override
   {
      mSyncedBitmap   = &theTheme.Bitmap(bmpCloud);
      mProgressBitmap = &theTheme.Bitmap(bmpCloudProgress);

      mCloudSyncFailedMessageWidth =
         GetTextExtent(CloudSyncFailedMessage.Translation()).x;

      mCloudSyncProgressMessageWidth =
         GetTextExtent(TranslatableString { CloudSyncProgressMessage }
                          .Format(100)
                          .Translation())
            .x;
   }

private:
   CloudSyncStatusField& mOwner;

   const wxBitmap* mSyncedBitmap {};
   const wxBitmap* mProgressBitmap {};

   int mCloudSyncFailedMessageWidth {};
   int mCloudSyncProgressMessageWidth {};
}; // class CloudSyncStatusField::StatusWidget

CloudSyncStatusField::CloudSyncStatusField(AudacityProject& project)
    : mProject { project }
    , mCloudExtension { ProjectCloudExtension::Get(project) }
    , mCloudStatusChangedSubscription { mCloudExtension.SubscribeStatusChanged(
         [this](const auto& extension) { OnCloudStatusChanged(extension); },
         true) }
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
   return mCloudExtension.IsCloudProject() ?
             GetStatusWidget().GetPreferredWidth(mState) :
             0;
}

void CloudSyncStatusField::OnSize(const wxRect& rect)
{
   GetStatusWidget().SetRect(rect);
}

bool CloudSyncStatusField::IsVisible() const
{
   return mState != State::Hidden;
}

TranslatableString CloudSyncStatusField::GetText() const
{
   return {};
}

void CloudSyncStatusField::MarkDirty()
{
   auto field = dynamic_cast<CloudSyncStatusBarFieldItem*>(
      ProjectStatusFieldsRegistry::Get(FieldId));

   if (field)
      field->MarkDirty(mProject);

   GetStatusWidget().Refresh();
   GetStatusWidget().Show(mState != State::Hidden);
}

void CloudSyncStatusField::OnCloudStatusChanged(
   const CloudStatusChangedMessage& message)
{
   mState = [](ProjectSyncStatus status) {
      switch (status)
      {
      case ProjectSyncStatus::Local:
         return State::Hidden;
      case ProjectSyncStatus::Unsynced:
         return State::Dirty;
      case ProjectSyncStatus::Synced:
         return State::Synced;
      case ProjectSyncStatus::Failed:
         return State::Failed;
      case ProjectSyncStatus::Syncing:
         return State::Uploading;
      default:
         return State::Hidden;
      }
   }(message.Status);

   if (mState == State::Uploading)
      mProgress = static_cast<int>(message.Progress * 100.0);

   MarkDirty();
}

CloudSyncStatusField::StatusWidget& CloudSyncStatusField::GetStatusWidget()
{
   if (!mStatusWidget)
   {
      mStatusWidget = safenew StatusWidget(
         *this, ProjectWindow::Get(mProject).GetStatusBar());

      mStatusWidget->Show(mCloudExtension.IsCloudProject());
   }

   return *mStatusWidget;
}

const CloudSyncStatusField::StatusWidget&
CloudSyncStatusField::GetStatusWidget() const
{
   return const_cast<CloudSyncStatusField*>(this)->GetStatusWidget();
}
} // namespace audacity::cloud::audiocom::sync
