/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioListDialog.h

  Dmitry Vedenko / Dmitry Makarenko

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

class AudioListDialog final : public wxDialogWrapper
{
public:
   AudioListDialog(wxWindow* parent, AudacityProject* project);
   ~AudioListDialog() override;

private:
   class AudioTableData;
#if wxUSE_ACCESSIBILITY
   class AudioListAccessible;
#endif

   void SetupHandlers();

   void OnBeforeRefresh();
   void OnRefreshCompleted(bool success);
   void FormatPageLabel();

   void OnOpen();
   void OnOpenAudioCom();

   void OnGridSelect(wxGridRangeSelectEvent& event);
   void OnSelectCell(wxGridEvent& event);

   void OnSearchTextChanged();
   void OnSearchTextSubmitted();

   AudacityProject* mProject { nullptr };

   wxTextCtrl* mSearchCtrl { nullptr };

   wxGrid* mAudioTable { nullptr };
   AudioTableData* mAudioTableData { nullptr };

   wxStaticText* mPageLabel { nullptr };
   wxButton* mPrevPageButton { nullptr };
   wxButton* mNextPageButton { nullptr };

   wxButton* mOpenButton { nullptr };
   wxButton* mOpenAudioCom { nullptr };

   wxString mLastSearchValue;

   std::unique_ptr<wxTimer> mSearchTimer;

#if wxUSE_ACCESSIBILITY
   AudioListAccessible* mAccessible { nullptr };
#endif

   bool mInRangeSelection { false };
};

} // namespace audacity::cloud::audiocom::sync
