/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_HISTORY_WINDOW__
#define __AUDACITY_HISTORY_WINDOW__

#include "Prefs.h"
#include "widgets/wxPanelWrapper.h" // to inherit

class wxButton;
class wxListCtrl;
class wxListEvent;
class wxSpinCtrl;
class wxTextCtrl;
class AudacityProject;
class ShuttleGui;
class UndoManager;

class HistoryDialog final : public wxDialogWrapper,
                            public PrefsListener
{

 public:
   HistoryDialog(AudacityProject * parent, UndoManager *manager);

   void UpdateDisplay(wxEvent &e);
   
   bool Show( bool show = true ) override;

 private:
   void Populate(ShuttleGui & S);

   void OnAudioIO(wxCommandEvent & evt);
   void DoUpdate();
   void UpdateLevels();

   void OnShow(wxShowEvent & event);
   void OnSize(wxSizeEvent & event);
   void OnCloseWindow(wxCloseEvent & event);
   void OnListKeyDown(wxKeyEvent & event);
   void OnItemSelected(wxListEvent & event);
   void OnDiscard(wxCommandEvent & event);
   void OnDiscardClipboard(wxCommandEvent & event);
   void OnCompact(wxCommandEvent & event);
   void OnGetURL(wxCommandEvent & event);

   // PrefsListener implementation
   void UpdatePrefs() override;

   AudacityProject   *mProject;
   UndoManager       *mManager;
   wxListCtrl        *mList;
   wxTextCtrl        *mTotal;
   wxTextCtrl        *mClipboard;
   wxTextCtrl        *mAvail;
   wxSpinCtrl        *mLevels;
   wxButton          *mDiscard;
   wxButton          *mCompact;

   int               mSelected;
   bool              mAudioIOBusy;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
