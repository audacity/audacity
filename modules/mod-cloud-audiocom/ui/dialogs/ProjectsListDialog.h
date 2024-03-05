/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectsListDialog.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <memory>

#include "wxPanelWrapper.h"

class AudacityProject;

class wxButton;
class wxGrid;
class wxGridTableBase;
class wxStaticText;
class wxTextCtrl;
class wxGridRangeSelectEvent;
class wxGridEvent;
class wxCommandEvent;
class wxTimer;

namespace audacity::cloud::audiocom::sync
{

class ProjectsListDialog final : public wxDialogWrapper
{
public:
   ProjectsListDialog(wxWindow* parent, AudacityProject* project);
   ~ProjectsListDialog() override;

private:
   class ProjectsTableData;

   void SetupHandlers();

   void OnBeforeRefresh();
   void OnRefreshCopleted(bool success);
   void FormatPageLabel();

   void OnOpen();
   void OnOpenAudioCom();

   void OnGridSelect(wxGridRangeSelectEvent& event);
   void OnSelectCell(wxGridEvent& event);

   void OnSearchTextChanged();
   void OnSearchTextSubmitted();

   AudacityProject* mProject { nullptr };

   wxTextCtrl* mSearchCtrl { nullptr };

   wxGrid* mProjectsTable { nullptr };
   ProjectsTableData* mProjectsTableData { nullptr };

   wxStaticText* mPageLabel { nullptr };
   wxButton* mPrevPageButton { nullptr };
   wxButton* mNextPageButton { nullptr };

   wxButton* mOpenButton { nullptr };
   wxButton* mOpenAudioCom { nullptr };

   wxString mLastSearchValue;

   std::unique_ptr<wxTimer> mSearchTimer;

   bool mInRangeSelection { false };
};

} // namespace audacity::cloud::audiocom::sync
