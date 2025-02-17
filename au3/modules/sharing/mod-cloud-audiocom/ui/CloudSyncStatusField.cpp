/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncStatusField.cpp

  Dmitry Vedenko

**********************************************************************/

#include "CloudSyncStatusField.h"

#include <algorithm>

#include <wx/bitmap.h>
#include <wx/dcbuffer.h>
#include <wx/graphics.h>
#include <wx/statusbr.h>

#include "Project.h"
#include "ProjectStatus.h"
#include "ProjectWindow.h"
#include "sync/ProjectCloudExtension.h"

#include "AllThemeResources.h"
#include "Theme.h"
#include "wxPanelWrapper.h"

#include "Prefs.h"

#if wxUSE_ACCESSIBILITY
#   include "WindowAccessible.h"
#endif

namespace audacity::cloud::audiocom::sync {
namespace {
const StatusBarField FieldId { L"CloudSyncStatus" };

const AttachedProjectObjects::RegisteredFactory key {
    [](AudacityProject& project)
    { return std::make_shared<CloudSyncStatusField>(project); }
};

class CloudSyncStatusBarFieldItem final : public StatusBarFieldItem
{
public:
    CloudSyncStatusBarFieldItem()
        : StatusBarFieldItem{FieldId}
    {
    }

    int GetDefaultWidth(const AudacityProject& project) const override
    {
        return CloudSyncStatusField::Get(project).GetWidth();
    }

    void OnSize(AudacityProject& project) override
    {
        const auto index
            =ProjectStatusFieldsRegistry::GetFieldIndex(project, name);

        if (index < 0) {
            return;
        }

        wxRect rect;
        if (ProjectWindow::Get(project).GetStatusBar()->GetFieldRect(index, rect)) {
            CloudSyncStatusField::Get(project).OnSize(rect);
        }
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
const auto CloudSyncProgressMessage = XO("Syncing to audio.com... (%d%%)");
const auto Padding                  = 2;
const auto ProgressBarWidth         = 240;
const auto ProgressBarHeight        = 10;
const auto ProgressBarBorderSize    = 1;

#if __WXMAC__
const auto StatusFieldPadding = 20;
#else
const auto StatusFieldPadding = 0;
#endif
} // namespace

class CloudSyncStatusField::StatusWidget final : public wxPanelWrapper, public PrefsListener
{
public:
    StatusWidget(CloudSyncStatusField& owner, wxWindow* parent)
        : wxPanelWrapper{parent}
        , mOwner{owner}
    {
        SetBackgroundStyle(wxBG_STYLE_PAINT);
        UpdatePrefs();

        Bind(wxEVT_PAINT, [this](auto&) { OnPaint(); });

#if wxUSE_ACCESSIBILITY
        SetAccessible(safenew WindowAccessible(this));
#endif
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
        switch (state) {
        case State::Failed:
            return mSyncedBitmap->GetWidth() + mCloudSyncFailedMessageWidth
                   + Padding * 4;
        case State::Uploading:
            return mProgressBitmap->GetWidth() + mCloudSyncProgressMessageWidth
                   + ProgressBarWidth + Padding * 4;
        }

        return mSyncedBitmap->GetWidth() + Padding * 2;
    }

    const wxBitmap* GetBitmap() const
    {
        return mOwner.mState == State::Uploading ? mProgressBitmap
               : mSyncedBitmap;
    }

    TranslatableString GetTranslatableText() const
    {
        if (mOwner.mState == State::Uploading) {
            return TranslatableString { CloudSyncProgressMessage }
        }
        .Format(
            mOwner.mProgress);
        else if (mOwner.mState == State::Failed) {
            return CloudSyncFailedMessage;
        }

        return {};
    }

    wxString GetText() const
    {
        return GetTranslatableText().Translation();
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

        if (text.empty()) {
            return;
        }

        gc->SetFont(GetFont(), GetForegroundColour());
        gc->DrawText(text, Padding + bitmapSize.x + 2 * Padding, 0);

        if (mOwner.mState != State::Uploading) {
            return;
        }

        gc->SetAntialiasMode(wxANTIALIAS_NONE);

        const auto progress = std::clamp(mOwner.mProgress, 0, 100);

        const auto progressFilledPen
            =gc->CreatePen(wxGraphicsPenInfo {}
                           .Colour(wxColour(0xc3c3c3))
                           .Width(ProgressBarBorderSize));

        const auto progressEmptyPen
            =gc->CreatePen(wxGraphicsPenInfo {}
                           .Colour(wxColour(0xc3c3c3))
                           .Width(ProgressBarBorderSize));
        const auto zeroPen = gc->CreatePen(
            wxGraphicsPenInfo {}.Width(0).Style(wxPENSTYLE_TRANSPARENT));

        const auto progressFilledBrush = gc->CreateBrush(wxColour(0x3cf03c));
        const auto progressEmptyBrush  = gc->CreateBrush(wxColour(0xffffff));

        const auto progressBarBorderLeft
            =Padding + bitmapSize.x + 2 * Padding + mCloudSyncProgressMessageWidth;

        const auto progressBarBorderRight
            =progressBarBorderLeft + ProgressBarWidth;

        const auto progressBarBorderTop
            =(widgetSize.y - ProgressBarHeight) / 2.0;

        const auto progressBarBorderBottom
            =progressBarBorderTop + ProgressBarHeight;

        const auto filledWidth
            =(ProgressBarWidth - ProgressBarBorderSize * 2) * progress / 100;

        const auto progressBarFillLeft
            =progressBarBorderLeft + ProgressBarBorderSize;
        const auto progressBarFillRight = progressBarFillLeft + filledWidth;

        const auto progressBarEmptyLeft
            =progressBarFillRight + (progress > 0 ? 1 : 0);
        const auto progressBarEmptyRight
            =progressBarBorderRight - ProgressBarBorderSize;

        const auto filledHeight = ProgressBarHeight - ProgressBarBorderSize;

        // Draw border
        if (progress == 0) {
            gc->SetPen(progressEmptyPen);
        } else {
            gc->SetPen(progressFilledPen);
        }

        gc->StrokeLine(
            progressBarBorderLeft, progressBarBorderTop, progressBarBorderLeft,
            progressBarBorderBottom);

        if (progress > 0) {
            gc->StrokeLine(
                progressBarFillLeft, progressBarBorderTop, progressBarFillRight,
                progressBarBorderTop);

            gc->StrokeLine(
                progressBarFillLeft, progressBarBorderBottom, progressBarFillRight,
                progressBarBorderBottom);

            gc->SetPen(zeroPen);
            gc->SetBrush(progressFilledBrush);

            gc->DrawRectangle(
                progressBarFillLeft, progressBarBorderTop + ProgressBarBorderSize,
                progressBarFillRight - progressBarFillLeft + 1, filledHeight);
        }

        if (progress < 100) {
            gc->SetPen(progressEmptyPen);

            gc->StrokeLine(
                progressBarEmptyLeft, progressBarBorderTop, progressBarEmptyRight,
                progressBarBorderTop);

            gc->StrokeLine(
                progressBarEmptyLeft, progressBarBorderBottom,
                progressBarEmptyRight, progressBarBorderBottom);

            gc->SetPen(zeroPen);
            gc->SetBrush(progressEmptyBrush);

            gc->DrawRectangle(
                progressBarEmptyLeft, progressBarBorderTop + ProgressBarBorderSize,
                progressBarEmptyRight - progressBarEmptyLeft + 1, filledHeight);
        }

        if (progress == 100) {
            gc->SetPen(progressFilledPen);
        } else {
            gc->SetPen(progressEmptyPen);
        }

        gc->StrokeLine(
            progressBarBorderRight, progressBarBorderTop, progressBarBorderRight,
            progressBarBorderBottom);
    }

    void UpdatePrefs() override
    {
        mSyncedBitmap   = &theTheme.Bitmap(bmpCloud);
        mProgressBitmap = &theTheme.Bitmap(bmpCloudProgress);

        mCloudSyncFailedMessageWidth
            =GetTextExtent(CloudSyncFailedMessage.Translation()).x;

        mCloudSyncProgressMessageWidth
            =GetTextExtent(TranslatableString { CloudSyncProgressMessage }
                           .Format(100)
                           .Translation())
              .x;
    }

    void UpdateName()
    {
        SetName(GetTranslatableText());
    }

private:
    CloudSyncStatusField& mOwner;

    const wxBitmap* mSyncedBitmap {};
    const wxBitmap* mProgressBitmap {};

    int mCloudSyncFailedMessageWidth {};
    int mCloudSyncProgressMessageWidth {};
}; // class CloudSyncStatusField::StatusWidget

CloudSyncStatusField::CloudSyncStatusField(AudacityProject& project)
    : mProject{project}
    , mCloudExtension{ProjectCloudExtension::Get(project)}
    , mCloudStatusChangedSubscription{mCloudExtension.SubscribeStatusChanged(
                                          [this](const auto& extension) { OnCloudStatusChanged(extension); },
                                          true)}
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
    return mCloudExtension.IsCloudProject()
           ? (GetStatusWidget().GetPreferredWidth(mState)
              + StatusFieldPadding)
           : 0;
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

    if (field) {
        field->MarkDirty(mProject);
    }

    auto& statusWidget = GetStatusWidget();

    statusWidget.Show(mState != State::Hidden);
    statusWidget.UpdateName();

    if (statusWidget.GetParent()) {
        statusWidget.GetParent()->Refresh();
    } else {
        statusWidget.Refresh();
    }
}

void CloudSyncStatusField::OnCloudStatusChanged(
    const CloudStatusChangedMessage& message)
{
    mState = [](ProjectSyncStatus status)
    {
        switch (status) {
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

    if (mState == State::Uploading) {
        mProgress = static_cast<int>(message.Progress * 100.0);
    }

    MarkDirty();
}

CloudSyncStatusField::StatusWidget& CloudSyncStatusField::GetStatusWidget()
{
    if (!mStatusWidget) {
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
