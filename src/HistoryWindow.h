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

   void OnShow(wxShowEvent & event);
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

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0123f234-637e-4e23-ad09-a8007bc9281a

