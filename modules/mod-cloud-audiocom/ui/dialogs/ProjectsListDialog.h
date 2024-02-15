/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectsListDialog.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include "wxPanelWrapper.h"

class AudacityProject;
class wxButton;
class wxGrid;
class wxGridTableBase;
class wxStaticText;
class wxTextCtrl;

namespace cloud::audiocom::sync
{

class ProjectsListDialog final
   : public wxDialogWrapper
{
public:
   ProjectsListDialog(wxWindow* parent, AudacityProject* project);

private:
   class ProjectsTableData;

   void SetupHandlers();

   void OnBeforeRefresh();
   void OnRefreshCopleted(bool success);
   void FormatPageLabel();

   void OnOpen();

   AudacityProject* mProject { nullptr };

   wxTextCtrl* mSearchCtrl { nullptr };

   wxGrid* mProjectsTable { nullptr };
   ProjectsTableData* mProjectsTableData { nullptr };

   wxStaticText* mPageLabel { nullptr };
   wxButton* mPrevPageButton { nullptr };
   wxButton* mNextPageButton { nullptr };

   wxButton* mOpenButton { nullptr };
   wxButton* mOpenAudioCom { nullptr };
};

} // namespace cloud::audiocom::sync
