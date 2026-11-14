/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_HISTORY_WINDOW__
#define __AUDACITY_HISTORY_WINDOW__

#include "Observer.h"
#include "Prefs.h"
#include "wxPanelWrapper.h" // to inherit

class wxButton;
class wxListCtrl;
class wxListEvent;
class wxSpinCtrl;
class wxTextCtrl;
struct AudioIOEvent;
class AudacityProject;
class ShuttleGui;
class UndoManager;

class HistoryDialog final : public wxDialogWrapper, public PrefsListener
{
public:
    HistoryDialog(AudacityProject* parent, UndoManager* manager);

    void UpdateDisplayForClipboard(struct ClipboardChangeMessage);
    void UpdateDisplay(struct UndoRedoMessage);
    void DoUpdateDisplay();

    bool Show(bool show = true) override;

private:
    void Populate(ShuttleGui& S);

    void OnAudioIO(AudioIOEvent);
    void DoUpdate();
    void UpdateLevels();

    void OnShow(wxShowEvent& event);
    void OnSize(wxSizeEvent& event);
    void OnCloseWindow(wxCloseEvent& event);
    void OnListKeyDown(wxKeyEvent& event);
    void OnItemSelected(wxListEvent& event);
    void OnDiscard(wxCommandEvent& event);
    void OnDiscardClipboard(wxCommandEvent& event);
    void OnCompact(wxCommandEvent& event);
    void OnGetURL(wxCommandEvent& event);

    // PrefsListener implementation
    void UpdatePrefs() override;

    Observer::Subscription mAudioIOSubscription,
                           mUndoSubscription,
                           mClipboardSubscription
    ;

    AudacityProject* mProject;
    UndoManager* mManager;
    wxListCtrl* mList;
    wxTextCtrl* mTotal;
    wxTextCtrl* mClipboard;
    wxTextCtrl* mAvail;
    wxSpinCtrl* mLevels;
    wxButton* mDiscard;
    wxButton* mCompact;

    int mSelected;
    bool mAudioIOBusy;

public:
    DECLARE_EVENT_TABLE()
};

#endif
