/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_HISTORY_WINDOW__
#define __AUDACITY_HISTORY_WINDOW__

#include <wx/button.h>
#include <wx/event.h>
#include <wx/frame.h>
#include <wx/listctrl.h>
#include <wx/spinctrl.h>
#include <wx/textctrl.h>

#include "widgets/wxPanelWrapper.h"

class AudacityProject;
class ShuttleGui;
class UndoManager;

class HistoryWindow final : public wxDialogWrapper {

 public:
   HistoryWindow(AudacityProject * parent, UndoManager *manager);
   ~HistoryWindow();

   void UpdateDisplay();

 private:
   void OnAudioIO(wxCommandEvent & evt);
   void DoUpdate();
   void UpdateLevels();

   void OnSize(wxSizeEvent & event);
   void OnCloseWindow(wxCloseEvent & WXUNUSED(event));
   void OnChar(wxKeyEvent & event);
   void OnItemSelected(wxListEvent & event);
   void OnDiscard(wxCommandEvent & event);

   AudacityProject   *mProject;
   UndoManager       *mManager;
   wxListCtrl        *mList;
   wxTextCtrl        *mTotal;
   wxTextCtrl        *mAvail;
   wxSpinCtrl        *mLevels;
   wxButton          *mDiscard;

   int               mSelected;
   bool              mAudioIOBusy;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
