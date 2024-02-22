/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectsListDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ProjectsListDialog.h"

#include <cassert>
#include <chrono>

#include <wx/button.h>
#include <wx/grid.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "BasicUI.h"
#include "CodeConversions.h"
#include "Internat.h"
#include "wxWidgetsWindowPlacement.h"

#include "ProjectManager.h"

#include "AuthorizationHandler.h"
#include "CloudSyncService.h"
#include "sync/CloudSyncUtils.h"
#include "sync/ProjectCloudExtension.h"

#include "CloudProjectUtils.h"

namespace cloud::audiocom::sync
{
class ProjectsListDialog::ProjectsTableData final : public wxGridTableBase
{
public:
   ProjectsTableData(ProjectsListDialog& owner, int pageSize)
       : mOwner { owner }
       , mPageSize { pageSize }
   {
   }

   int GetNumberRows() override
   {
      return mPageSize;
   }

   int GetNumberCols() override
   {
      return 2;
   }

   static wxString FormatTime(int64_t time)
   {
      using namespace std::chrono;

      const auto time_passed =
         system_clock::now() - system_clock::from_time_t(time);

      if (time_passed < minutes(1))
         return XO("less than 1 minute").Translation();
      if (time_passed < hours(1))
         return XP("one minutes ago", "%d minutes ago",
                   0)(static_cast<int>(duration_cast<minutes>(time_passed).count()))
            .Translation();
      if (time_passed < hours(48))
         return XP("one hour ago", "%d hours ago",
                   0)(static_cast<int>(duration_cast<hours>(time_passed).count()))
            .Translation();

      return wxDateTime(static_cast<time_t>(time)).Format();
   }

   wxString GetValue(int row, int col) override
   {
      if (row >= static_cast<int>(mResponse.Items.size()))
         return {};

      const auto item = mResponse.Items[row];

      switch (col)
      {
      case 0:
         return audacity::ToWXString(item.Name);
      case 1:
         return FormatTime(item.Updated);
      }

      return {};
   }

   void SetValue(int row, int col, const wxString& value) override
   {
      assert(false);
   }

   wxString GetRowLabelValue(int row) override
   {
      return {};
   }

   wxString GetColLabelValue(int col) override
   {
      static const wxString colLabels[] = {
         XO("Project Name").Translation(),
         XO("Modified").Translation(),
      };

      return col < 2 ? colLabels[col] : wxString {};
   }

   wxString GetCornerLabelValue() const override
   {
      return {};
   }

   int GetColWidth(int col) const
   {
      static const int colWidths[] = { 400, 150 };
      return col < 2 ? colWidths[col] : 0;
   }

   void Refresh(int page)
   {
      using namespace std::chrono_literals;

      auto authResult = PerformBlockingAuth(mOwner.mProject);

      switch (authResult.Result)
      {
      case AuthResult::Status::Authorised:
         break;
      case AuthResult::Status::Failure:
         BasicUI::ShowErrorDialog(
            wxWidgetsWindowPlacement { &mOwner }, XO("Open from cloud"),
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
         wxWidgetsWindowPlacement { &mOwner }, XO("Open from cloud"),
         XO("Loading projects list..."));

      auto cancellationContext = audacity::concurrency::CancellationContext::Create();

      auto future = CloudSyncService::Get().GetProjects(
         cancellationContext, page, mPageSize, {});

      while (std::future_status::ready != future.wait_for(100ms))
      {
         BasicUI::Yield();
         if (progressDialog->Pulse() != BasicUI::ProgressResult::Success)
            cancellationContext->Cancel();
      }

      auto result = future.get();

      if (std::holds_alternative<PaginatedProjectsResponse>(result))
      {
         auto response = std::get_if<PaginatedProjectsResponse>(&result);
         mResponse     = std::move(*response);
         mOwner.OnRefreshCopleted(true);
      }
      else
      {
         auto responseResult = std::get_if<ResponseResult>(&result);

         BasicUI::ShowErrorDialog(
            wxWidgetsWindowPlacement { &mOwner }, XO("Open from cloud"),
            XO("Failed to get projects list"), {},
            BasicUI::ErrorDialogOptions {}.Log(
               audacity::ToWString(responseResult->Content)));

         if (mResponse.Items.empty())
            mOwner.EndModal(0);

         mOwner.OnRefreshCopleted(false);
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
         Refresh(mResponse.Pagination.CurrentPage - 1);
   }

   void NextPage()
   {
      if (HasNextPage())
         Refresh(mResponse.Pagination.CurrentPage + 1);
   }

   int GetCurrentPage() const
   {
      return mResponse.Pagination.CurrentPage;
   }

   int GetPagesCount() const
   {
      return mResponse.Pagination.PagesCount;
   }

   std::string GetSelectedProjectId() const
   {
      const auto selectedRow = mOwner.mProjectsTable->GetSelectedRows();

      if (selectedRow.empty())
         return {};

      return mResponse.Items[selectedRow[0]].Id;
   }

private:
   ProjectsListDialog& mOwner;
   const int mPageSize;

   PaginatedProjectsResponse mResponse;
};

ProjectsListDialog::ProjectsListDialog(
   wxWindow* parent, AudacityProject* project)
    : wxDialogWrapper { parent, wxID_ANY, XO("Open from cloud") }
    , mProject { project }
{
   auto header =
      safenew wxStaticText { this, wxID_ANY,
                             XO("Cloud saved projects").Translation() };
   auto searchHeader =
      safenew wxStaticText { this, wxID_ANY, XO("Search:").Translation() };
   mSearchCtrl = safenew wxTextCtrl { this, wxID_ANY };

   mProjectsTable     = safenew wxGrid { this, wxID_ANY };
   mProjectsTableData = safenew ProjectsTableData { *this, 7 };
   mProjectsTable->SetDefaultRowSize(32);
   mProjectsTable->SetGridLineColour(
      mProjectsTable->GetDefaultCellBackgroundColour());
   mProjectsTable->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTER);
   mProjectsTable->SetTable(mProjectsTableData, true);
   mProjectsTable->SetRowLabelSize(1);

   mProjectsTable->EnableEditing(false);
   mProjectsTable->SetSelectionMode(wxGrid::wxGridSelectRows);

   for (auto i = 0; i < mProjectsTableData->GetNumberCols(); ++i)
      mProjectsTable->SetColSize(i, mProjectsTableData->GetColWidth(i));

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
      mProjectsTable, wxSizerFlags().Expand().Border(wxLEFT | wxRIGHT, 16));

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

   SetSizer(topSizer);
   Fit();
   Center();

   SetupHandlers();
   BasicUI::CallAfter([this] { mProjectsTableData->Refresh(1); });
}

void ProjectsListDialog::SetupHandlers()
{
   mPrevPageButton->Bind(
      wxEVT_BUTTON, [this](auto&) { mProjectsTableData->PrevPage(); });

   mNextPageButton->Bind(
      wxEVT_BUTTON, [this](auto&) { mProjectsTableData->NextPage(); });

   mOpenButton->Bind(wxEVT_BUTTON, [this](auto&) { OnOpen(); });

   mProjectsTable->Bind(
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
}

void ProjectsListDialog::OnBeforeRefresh()
{
   mProjectsTable->Enable(false);
   mPrevPageButton->Enable(false);
   mNextPageButton->Enable(false);
}

void ProjectsListDialog::OnRefreshCopleted(bool success)
{
   mProjectsTable->Enable(success);

   mPrevPageButton->Enable(success && mProjectsTableData->HasPrevPage());
   mNextPageButton->Enable(success && mProjectsTableData->HasNextPage());

   FormatPageLabel();

   mProjectsTable->Refresh();
}

void ProjectsListDialog::FormatPageLabel()
{
   if (mProjectsTableData->GetPagesCount() == 0)
   {
      mPageLabel->SetLabel({});
      return;
   }

   mPageLabel->SetLabel(XO("Page %d of %d")
                           .Format(
                              mProjectsTableData->GetCurrentPage(),
                              mProjectsTableData->GetPagesCount())
                           .Translation());
}

void ProjectsListDialog::OnOpen()
{
   const auto selectedProjectId = mProjectsTableData->GetSelectedProjectId();

   if (selectedProjectId.empty())
      return;

   EndModal(wxID_OK);

    BasicUI::CallAfter([project = mProject, selectedProjectId] {
        OpenProjectFromCloud(project, selectedProjectId, {}, false);
    });
}

} // namespace cloud::audiocom::sync
