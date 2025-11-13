/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioListDialog.cpp

  Dmitry Vedenko / Dmitry Makarenko

**********************************************************************/

#include "AudioListDialog.h"

#include <chrono>

#include <wx/button.h>
#include <wx/grid.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/timer.h>

#include "BasicUI.h"
#include "CodeConversions.h"
#include "Internat.h"
#include "UrlEncode.h"
#include "wxWidgetsWindowPlacement.h"

#include "AuthorizationHandler.h"
#include "CloudSyncService.h"
#include "ServiceConfig.h"
#include "OAuthService.h"
#include "UserService.h"
#include "ExportUtils.h"

#include "sync/CloudSyncDTO.h"

#include "CloudAudioOpenUtils.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

namespace audacity::cloud::audiocom::sync
{

namespace
{
const auto OpenFromCloudTitle = XO("Open cloud audio file");
} // namespace

class AudioListDialog::AudioTableData final : public wxGridTableBase
{
public:
   AudioTableData(AudioListDialog& owner, int pageSize)
       : mOwner { owner }
       , mPageSize { pageSize }
   {
   }

   enum Column
   {
      Title,
      Created,
      Tags,
      Count,
      // Unused now, keep for possible future use
      Author,
   };

   int GetNumberRows() override
   {
      return static_cast<int>(mResponse.Items.size());
   }

   int GetNumberCols() override
   {
      return static_cast<int>(Column::Count);
   }

   static wxString FormatTime(int64_t time)
   {
     using namespace std::chrono;

      const auto time_passed =
         system_clock::now() - system_clock::from_time_t(time);

      if (time_passed < minutes(1))
         return XO("less than 1 minute").Translation();
      if (time_passed < hours(1))
         return XP("one minute ago", "%d minutes ago",
                   0)(static_cast<int>(
                         duration_cast<minutes>(time_passed).count()))
            .Translation();
      if (time_passed < hours(48))
         return XP("one hour ago", "%d hours ago", 0)(
                   static_cast<int>(duration_cast<hours>(time_passed).count()))
            .Translation();

      return wxDateTime(static_cast<time_t>(time)).Format();
   }

   wxString GetValue(int row, int col) override
   {
      if (row < 0 || row >= static_cast<int>(mResponse.Items.size()))
         return {};

      const auto& item = mResponse.Items[row];

      switch (col)
      {
      case Column::Title:
         return ToWXString(item.Title);
      case Column::Author:
         return ToWXString(item.AuthorName.empty() ? item.Username : item.AuthorName);
      case Column::Tags:
      {
         if (item.Tags.empty())
            return {};
         wxString tags = {};
         for (size_t i = 0; i < item.Tags.size(); ++i)
         {
            if (i) tags += ", ";
            tags += ToWXString(item.Tags[i]);
         }
         return tags;
      }
      case Column::Created:
         return FormatTime(item.Created);
      default:
         return {};
      }
   }

   void SetValue(int row, int col, const wxString& value) override {}

   wxString GetRowLabelValue(int row) override { return wxString::Format("%d", row + 1); }

   wxString GetColLabelValue(int col) override
   {
      switch (col)
      {
     case Column::Title:
        return XO("Title").Translation();
     case Column::Author:
        return XO("Author").Translation();
     case Column::Tags:
        return XO("Tags").Translation();
     case Column::Created:
        return XO("Created").Translation();
      default:
         return {};
      }
   }

   wxString GetCornerLabelValue() const override { return {}; }

   int GetColWidth(int col) const
   {
      static const int colWidths[] = { 300, 250, 200, 100 };
      const int count = static_cast<int>(Column::Count);
      return (col >= 0 && col < count) ? colWidths[col] : 0;
   }

   void Refresh(int page, const wxString& searchTerm)
   {
      using namespace std::chrono_literals;

      auto authResult = PerformBlockingAuth(
         mOwner.mProject, AudiocomTrace::OpenFromCloudMenu);

      switch (authResult.Result)
      {
      case AuthResult::Status::Authorised:
         break;
      case AuthResult::Status::Failure:
         BasicUI::ShowErrorDialog(
            wxWidgetsWindowPlacement { &mOwner }, OpenFromCloudTitle,
            XO("Failed to authorize account"), {},
            BasicUI::ErrorDialogOptions {}.Log(
               audacity::ToWString(authResult.ErrorMessage)));
         [[fallthrough]];
      default:
         mOwner.EndModal(0);
         return;
      }

      mOwner.OnBeforeRefresh();

      auto progressDialog = BasicUI::MakeGenericProgress(
         wxWidgetsWindowPlacement { &mOwner }, OpenFromCloudTitle,
         XO("Loading audio list..."));

      auto cancellationContext = concurrency::CancellationContext::Create();

      auto future = CloudSyncService::Get().GetAudioList(
         cancellationContext, page, mPageSize, UrlEncode(ToUTF8(searchTerm)));

      while (std::future_status::ready != future.wait_for(100ms))
      {
         BasicUI::Yield();
         if (progressDialog->Pulse() != BasicUI::ProgressResult::Success)
            cancellationContext->Cancel();
      }

      auto result = future.get();

      if (!mResponse.Items.empty())
      {
         wxGridTableMessage msg(
            this, wxGRIDTABLE_NOTIFY_ROWS_DELETED, 0, mResponse.Items.size());

         GetView()->ProcessTableMessage(msg);
      }

      if (std::holds_alternative<PaginatedAudioResponse>(result))
      {
         auto response = std::get_if<PaginatedAudioResponse>(&result);
         mResponse     = std::move(*response);

         if (!mResponse.Items.empty())
         {
            wxGridTableMessage msg(
               this, wxGRIDTABLE_NOTIFY_ROWS_APPENDED, mResponse.Items.size(),
               0);

            GetView()->ProcessTableMessage(msg);
         }

         mOwner.OnRefreshCompleted(true);
      }
      else
      {
         auto responseResult = std::get_if<ResponseResult>(&result);

         BasicUI::ShowErrorDialog(
            wxWidgetsWindowPlacement { &mOwner }, OpenFromCloudTitle,
            XO("Failed to get audio list"), {},
            BasicUI::ErrorDialogOptions {}.Log(
               audacity::ToWString(responseResult->Content)));

         if (mResponse.Items.empty())
            mOwner.EndModal(0);

         mOwner.OnRefreshCompleted(false);
      }
   }

   bool HasPrevPage() const
   {
      return mResponse.Pagination.CurrentPage > 1;
   }

   bool HasNextPage() const
   {
      return mResponse.Pagination.CurrentPage < mResponse.Pagination.PagesCount;
   }

   void PrevPage()
   {
      if (HasPrevPage())
         Refresh(mResponse.Pagination.CurrentPage - 1, mOwner.mLastSearchValue);
   }

   void NextPage()
   {
      if (HasNextPage())
         Refresh(mResponse.Pagination.CurrentPage + 1, mOwner.mLastSearchValue);
   }

   int GetCurrentPage() const
   {
      return mResponse.Pagination.CurrentPage;
   }

   int GetPagesCount() const
   {
      return mResponse.Pagination.PagesCount;
   }

   const CloudAudioInfo* GetSelectedAudioInfo() const
   {
      const auto sel = GetView()->GetSelectedRows();
      if (sel.empty())
         return nullptr;

      const auto row = sel[0];
      if (row < 0 || row >= static_cast<int>(mResponse.Items.size()))
         return nullptr;

      return &mResponse.Items[row];
   }

   std::string GetSelectedAudioUrl() const
   {
      const auto selectedRow = mOwner.mAudioTable->GetSelectedRows();

      if (selectedRow.empty())
         return {};

      auto& userService = GetUserService();
      auto& oauthService = GetOAuthService();
      auto& serviceConfig = GetServiceConfig();

      auto userId = audacity::ToUTF8(userService.GetUserId());
      auto& item = mResponse.Items[selectedRow[0]];

      auto audioPage = serviceConfig.GetAudioPagePath(item.Username, item.Slug, AudiocomTrace::OpenFromCloudMenu);
      auto url = oauthService.MakeAudioComAuthorizeURL(userId, audioPage);

      return url;
   }

private:
   AudioListDialog& mOwner;
   const int mPageSize;

   PaginatedAudioResponse mResponse;
};

#if wxUSE_ACCESSIBILITY

class AudioListDialog::AudioListAccessible : public WindowAccessible
{
public:
   AudioListAccessible(wxGrid& owner, AudioTableData& data)
       : WindowAccessible { owner.GetGridWindow() }
       , mOwner { owner }
       , mAudioData { data }
   {
   }

   void SetSelectedRow(int rowId)
   {
      if (mLastId != InvalidRow)
      {
         NotifyEvent(
            wxACC_EVENT_OBJECT_SELECTIONREMOVE, mOwner.GetGridWindow(),
            wxOBJID_CLIENT, mLastId);
      }

      if (&mOwner == wxWindow::FindFocus())
      {
         NotifyEvent(
            wxACC_EVENT_OBJECT_FOCUS, mOwner.GetGridWindow(), wxOBJID_CLIENT,
            rowId + 1);
      }

      NotifyEvent(
         wxACC_EVENT_OBJECT_SELECTION, mOwner.GetGridWindow(), wxOBJID_CLIENT,
         rowId + 1);

      mLastId = rowId + 1;
   }

   void TableDataUpdated()
   {
      NotifyEvent(
         wxACC_EVENT_OBJECT_REORDER, mOwner.GetGridWindow(), wxOBJID_CLIENT, 0);
   }

private:
   wxAccStatus GetChild(int childId, wxAccessible** child) override
   {
      if (childId == wxACC_SELF)
         *child = this;
      else
         *child = nullptr;

      return wxACC_OK;
   }

   wxAccStatus GetChildCount(int* childCount) override
   {
      *childCount = mAudioData.GetRowsCount();
      return wxACC_OK;
   }

   wxAccStatus
   GetDefaultAction(int WXUNUSED(childId), wxString* actionName) override
   {
      actionName->clear();
      return wxACC_OK;
   }

   // Returns the description for this object or a child.
   wxAccStatus
   GetDescription(int WXUNUSED(childId), wxString* description) override
   {
      description->clear();
      return wxACC_OK;
   }

   // Returns help text for this object or a child, similar to tooltip text.
   wxAccStatus GetHelpText(int WXUNUSED(childId), wxString* helpText) override
   {
      helpText->clear();
      return wxACC_OK;
   }

   // Returns the keyboard shortcut for this object or child.
   // Return e.g. ALT+K
   wxAccStatus
   GetKeyboardShortcut(int WXUNUSED(childId), wxString* shortcut) override
   {
      shortcut->clear();
      return wxACC_OK;
   }

   wxAccStatus GetLocation(wxRect& rect, int elementId) override
   {
      if (elementId == wxACC_SELF)
      {
         rect = mOwner.GetRect();
         rect.SetPosition(
            mOwner.GetParent()->ClientToScreen(rect.GetPosition()));
      }
      else
      {
         const auto row = elementId - 1;

         if (row > mAudioData.GetRowsCount())
            return wxACC_OK; // ?

         wxRect rowRect;

         for (int col = 0; col < mAudioData.GetColsCount(); ++col)
            rowRect.Union(mOwner.CellToRect(elementId - 1, col));

         rowRect.SetPosition(
            mOwner.CalcScrolledPosition(rowRect.GetPosition()));
         rowRect.SetPosition(
            mOwner.GetGridWindow()->ClientToScreen(rowRect.GetPosition()));

         rect = rowRect;
      }

      return wxACC_OK;
   }

   wxAccStatus GetName(int childId, wxString* name) override
   {
      if (childId == wxACC_SELF)
         return wxACC_OK;

      const auto row = childId - 1;

      if (row > mAudioData.GetRowsCount())
         return wxACC_OK; // ?

      for (int col = 0; col < mAudioData.GetColsCount(); ++col)
      {
         if (col != 0)
            *name += ", ";

         *name += mAudioData.GetColLabelValue(col) + " " +
                  mAudioData.GetValue(row, col);
      }

      return wxACC_OK;
   }

   wxAccStatus GetParent(wxAccessible**) override
   {
      return wxACC_NOT_IMPLEMENTED;
   }

   wxAccStatus GetRole(int childId, wxAccRole* role) override
   {
      if (childId == wxACC_SELF)
      {
#   if defined(__WXMSW__)
         *role = wxROLE_SYSTEM_TABLE;
#   endif

#   if defined(__WXMAC__)
         *role = wxROLE_SYSTEM_GROUPING;
#   endif
      }
      else
      {
         *role = wxROLE_SYSTEM_TEXT;
      }

      return wxACC_OK;
   }

   wxAccStatus GetSelections(wxVariant*) override
   {
      return wxACC_NOT_IMPLEMENTED;
   }

   wxAccStatus GetState(int childId, long* state) override
   {
      int flag = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_SELECTABLE;

      if (childId == wxACC_SELF)
      {
         *state = 0;
         return wxACC_FAIL;
      }

#   if defined(__WXMSW__)
      flag |= wxACC_STATE_SYSTEM_FOCUSED | wxACC_STATE_SYSTEM_SELECTED |
              wxACC_STATE_SYSTEM_UNAVAILABLE | wxACC_STATE_SYSTEM_FOCUSED;
#   endif

#   if defined(__WXMAC__)
      flag |= wxACC_STATE_SYSTEM_UNAVAILABLE;

      if (childId == mLastId)
         flag |= wxACC_STATE_SYSTEM_SELECTED | wxACC_STATE_SYSTEM_FOCUSED;
#   endif

      *state = flag;

      return wxACC_OK;
   }

   wxAccStatus GetValue(int childId, wxString* strValue) override
   {
      strValue->clear();

#   if defined(__WXMSW__)
      return wxACC_OK;
#   elif defined(__WXMAC__)
      return GetName(childId, strValue);
#   else
      return wxACC_NOT_IMPLEMENTED;
#   endif
   }

#   if defined(__WXMAC__)
   wxAccStatus Select(int childId, wxAccSelectionFlags selectFlags) override
   {
      if (childId == wxACC_SELF)
         return wxACC_OK;

      if (selectFlags & wxACC_SEL_TAKESELECTION)
         mOwner.SetGridCursor(childId - 1, 0);

      mOwner.SelectBlock(
         childId - 1, 0, childId - 1, 0, selectFlags & wxACC_SEL_ADDSELECTION);

      return wxACC_OK;
   }
#   endif

   wxAccStatus GetFocus(int* childId, wxAccessible** child) override
   {
      if (&mOwner == wxWindow::FindFocus())
      {
         if (mAudioData.GetRowsCount() == 0)
            *child = this;
         else
            *childId = mLastId;
      }

      return wxACC_OK;
   }

   wxGrid& mOwner;
   AudioTableData& mAudioData;

   static constexpr int InvalidRow = -1;
   int mLastId { InvalidRow };
}; // class AudioListAccessible

#endif

AudioListDialog::AudioListDialog(
   wxWindow* parent, AudacityProject* project)
    : wxDialogWrapper { parent, wxID_ANY, OpenFromCloudTitle }
    , mProject { project }
{
   auto header =
      safenew wxStaticText { this, wxID_ANY,
                             XO("Open cloud audio file").Translation() };
   auto searchHeader =
      safenew wxStaticText { this, wxID_ANY, XO("Search:").Translation() };

   mSearchCtrl = safenew wxTextCtrl { this,          wxID_ANY,
                                      wxEmptyString, wxDefaultPosition,
                                      wxDefaultSize, wxTE_PROCESS_ENTER };

   mAudioTable = safenew wxGrid { this, wxID_ANY };

   mAudioTableData = safenew AudioTableData { *this, 15 };

   mAudioTable->SetDefaultRowSize(32);

   mAudioTable->SetGridLineColour(
      mAudioTable->GetDefaultCellBackgroundColour());
   mAudioTable->SetCellHighlightPenWidth(0);

   mAudioTable->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTER);
   mAudioTable->SetTable(mAudioTableData, true);
   mAudioTable->SetRowLabelSize(0);

   mAudioTable->EnableEditing(false);
   mAudioTable->SetSelectionMode(wxGrid::wxGridSelectRows);
   mAudioTable->SetTabBehaviour(wxGrid::Tab_Leave);

   mAudioTable->SetMinSize({ -1, 32 * 16 + 9 });

   for (auto i = 0; i < mAudioTableData->GetNumberCols(); ++i)
      mAudioTable->SetColSize(i, mAudioTableData->GetColWidth(i));

#if wxUSE_ACCESSIBILITY
   mAccessible =
      safenew AudioListAccessible { *mAudioTable, *mAudioTableData };
   mAudioTable->GetGridWindow()->SetAccessible(mAccessible);
#endif

   mPageLabel = safenew wxStaticText { this, wxID_ANY, {} };
   mPrevPageButton =
      safenew wxButton { this, wxID_ANY, XO("Prev").Translation() };
   mNextPageButton =
      safenew wxButton { this, wxID_ANY, XO("Next").Translation() };
   mOpenButton = safenew wxButton { this, wxID_ANY, XO("Open").Translation() };
   mOpenAudioCom = safenew wxButton { this, wxID_ANY,
                                      XO("View in audio.com").Translation() };

   auto topSizer = safenew wxBoxSizer { wxVERTICAL };

   auto headerSizer = safenew wxBoxSizer { wxHORIZONTAL };
   headerSizer->Add(header, wxSizerFlags().CenterVertical().Left());
   headerSizer->AddStretchSpacer();
   headerSizer->Add(
      searchHeader, wxSizerFlags().CenterVertical().Border(wxRIGHT, 4));
   headerSizer->Add(mSearchCtrl, wxSizerFlags().CenterVertical());

   topSizer->Add(headerSizer, wxSizerFlags().Expand().Border(wxALL, 16));
   topSizer->Add(
      mAudioTable, wxSizerFlags().Expand().Border(wxLEFT | wxRIGHT, 16));

   auto pageSizer = safenew wxBoxSizer { wxHORIZONTAL };
   pageSizer->Add(mPageLabel, wxSizerFlags().CenterVertical());
   pageSizer->AddStretchSpacer();
   pageSizer->Add(mPrevPageButton, wxSizerFlags().CenterVertical());
   pageSizer->Add(mNextPageButton, wxSizerFlags().CenterVertical());
   topSizer->AddSpacer(8);
   topSizer->Add(
      pageSizer, wxSizerFlags().Expand().Border(wxLEFT | wxRIGHT, 16));

   auto buttonsSizer = safenew wxBoxSizer { wxHORIZONTAL };
   buttonsSizer->Add(mOpenAudioCom, wxSizerFlags().CenterVertical());
   buttonsSizer->AddStretchSpacer();
   buttonsSizer->Add(mOpenButton, wxSizerFlags().CenterVertical());

   topSizer->Add(buttonsSizer, wxSizerFlags().Expand().Border(wxALL, 16));

   mOpenButton->Disable();
   mOpenAudioCom->Disable();

   mSearchTimer = std::make_unique<wxTimer>(this);

   SetSizer(topSizer);
   Fit();
   Center();

   SetupHandlers();
   BasicUI::CallAfter([this]
                      { mAudioTableData->Refresh(1, mLastSearchValue); });
}

AudioListDialog::~AudioListDialog() = default;

void AudioListDialog::SetupHandlers()
{
   mPrevPageButton->Bind(
      wxEVT_BUTTON, [this](auto&) { mAudioTableData->PrevPage(); });

   mNextPageButton->Bind(
      wxEVT_BUTTON, [this](auto&) { mAudioTableData->NextPage(); });

   mOpenButton->Bind(wxEVT_BUTTON, [this](auto&) { OnOpen(); });

   mOpenAudioCom->Bind(wxEVT_BUTTON, [this](auto&) { OnOpenAudioCom(); });

   mAudioTable->Bind(
      wxEVT_GRID_CELL_LEFT_DCLICK, [this](auto&) { OnOpen(); });

   Bind(
      wxEVT_CHAR_HOOK,
      [this](auto& evt)
      {
         if (!IsEscapeKey(evt))
         {
            evt.Skip();
            return;
         }

         EndModal(wxID_CANCEL);
      });

   mAudioTable->Bind(
      wxEVT_GRID_RANGE_SELECT, [this](auto& evt) { OnGridSelect(evt); });

   mAudioTable->Bind(
      wxEVT_GRID_SELECT_CELL, [this](auto& evt) { OnSelectCell(evt); });

   mAudioTable->Bind(
      wxEVT_KEY_UP,
      [this](auto& evt)
      {
         const auto keyCode = evt.GetKeyCode();
         if (keyCode != WXK_RETURN && keyCode != WXK_NUMPAD_ENTER)
         {
            evt.Skip();
            return;
         }

         OnOpen();
      });

   mAudioTable->Bind(
      wxEVT_KEY_DOWN,
      [this](auto& evt)
      {
         const auto keyCode = evt.GetKeyCode();
         // prevent being able to up arrow past the first row (issue #6251)
         if (keyCode == WXK_UP && mAudioTable->GetGridCursorRow() == 0) {
               return;
         }
         // prevent being able to down arrow past the last row (issue #6251)
         if (keyCode == WXK_DOWN &&
            mAudioTable->GetGridCursorRow() ==
            mAudioTable->GetNumberRows() - 1) {
               return;
         }
         if (keyCode != WXK_RETURN && keyCode != WXK_NUMPAD_ENTER)
         {
            evt.Skip();
            return;
         }
      });

   mAudioTable->Bind(
      wxEVT_GRID_TABBING,
      [this](auto& evt)
      {
         // needed for correct tabbing - see issue #6190
         NavigateIn(evt.ShiftDown() ? wxNavigationKeyEvent::IsBackward :
            wxNavigationKeyEvent::IsForward);
      });

   mAudioTable->Bind(
      wxEVT_SET_FOCUS,
      [this](auto& evt)
      {
         // needed so that for screen readers a row rather than the whole
         // table is the initial focus - see issue #6190
#if wxUSE_ACCESSIBILITY
         int row = mAudioTable->GetGridCursorRow();
         if (row != -1)
            mAccessible->SetSelectedRow(row);
#endif
         evt.Skip();
      });

   mSearchCtrl->Bind(wxEVT_TEXT, [this](auto&) { OnSearchTextChanged(); });

   mSearchCtrl->Bind(
      wxEVT_TEXT_ENTER, [this](auto&) { OnSearchTextSubmitted(); });

   Bind(wxEVT_TIMER, [this](auto&) { OnSearchTextSubmitted(); });
}

void AudioListDialog::OnBeforeRefresh()
{
   mAudioTable->Enable(false);
   mPrevPageButton->Enable(false);
   mNextPageButton->Enable(false);
}

void AudioListDialog::OnRefreshCompleted(bool success)
{
   mAudioTable->Enable(success);

   mPrevPageButton->Enable(success && mAudioTableData->HasPrevPage());
   mNextPageButton->Enable(success && mAudioTableData->HasNextPage());

   FormatPageLabel();

   if (success && mAudioTableData->GetNumberRows() > 0)
   {
      mAudioTable->AutoSizeColumn(AudioTableData::Column::Tags, false);
   }

   mAudioTable->ForceRefresh();

#if wxUSE_ACCESSIBILITY
   if (mAccessible)
      mAccessible->TableDataUpdated();
#endif
}

void AudioListDialog::FormatPageLabel()
{
   if (mAudioTableData->GetPagesCount() == 0)
   {
      mPageLabel->SetLabel({});
      return;
   }

   mPageLabel->SetLabel(XO("Page %d of %d")
                           .Format(
                              mAudioTableData->GetCurrentPage(),
                              mAudioTableData->GetPagesCount())
                           .Translation());
}

void AudioListDialog::OnOpen()
{
   if (mAudioTable->GetSelectedRows().empty())
      return;

   const auto audioInfo = mAudioTableData->GetSelectedAudioInfo();

   if (audioInfo == nullptr)
      return;

   EndModal(wxID_OK);

   BasicUI::CallAfter(
      [project = mProject, selectedAudioId = audioInfo->Id]
      { OpenAudioFromCloud(selectedAudioId); });
}

void AudioListDialog::OnOpenAudioCom()
{
   if (mAudioTable->GetSelectedRows().empty())
      return;

   const auto selectedAudioUrl = mAudioTableData->GetSelectedAudioUrl();

   if (selectedAudioUrl.empty())
      return;

   BasicUI::OpenInDefaultBrowser(ToWXString(selectedAudioUrl));
}

void AudioListDialog::OnGridSelect(wxGridRangeSelectEvent& event)
{
   event.Skip();

   if (!event.Selecting())
   {
      mOpenButton->Disable();
      mOpenAudioCom->Disable();
      return;
   }

   mOpenButton->Enable();
   mOpenAudioCom->Enable();

   const auto topRow     = event.GetTopRow();
   const auto bottomRow  = event.GetBottomRow();
   const auto currentRow = mAudioTable->GetGridCursorRow();

   if (topRow != bottomRow)
   {
      if (mInRangeSelection)
         return;

      mInRangeSelection = true;
      auto switcher     = finally([this] { mInRangeSelection = false; });

      mAudioTable->SelectRow(currentRow == topRow ? bottomRow : topRow);
   }
}

void AudioListDialog::OnSelectCell(wxGridEvent& event)
{
   event.Skip();
   mAudioTable->SelectRow(event.GetRow());

#if wxUSE_ACCESSIBILITY
   if (mAccessible)
      mAccessible->SetSelectedRow(event.GetRow());
#endif
}

void AudioListDialog::OnSearchTextChanged()
{
   mSearchTimer->StartOnce(250);
}

void AudioListDialog::OnSearchTextSubmitted()
{
   if (mSearchTimer->IsRunning())
      mSearchTimer->Stop();

   const auto value = mSearchCtrl->GetValue();

   if (value == mLastSearchValue)
      return;

   mLastSearchValue = value;

   mAudioTableData->Refresh(1, mLastSearchValue);
}

} // namespace audacity::cloud::audiocom::sync
