/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_HISTORY_WINDOW__
#define __AUDACITY_HISTORY_WINDOW__

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/frame.h>
#include <wx/listctrl.h>
#include <wx/spinctrl.h>
#include <wx/textctrl.h>

class AudacityProject;
class ShuttleGui;
class UndoManager;

class HistoryWindow :public wxDialog {

 public:
   HistoryWindow(AudacityProject * parent, UndoManager *manager);
   ~HistoryWindow();

   void UpdateDisplay();

 private:
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
   wxTextCtrl        *mAvail;
   wxSpinCtrl        *mLevels;
   wxButton          *mDiscard;
   int               mSelected;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
