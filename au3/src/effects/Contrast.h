/**********************************************************************

  Audacity: A Digital Audio Editor

  Contrast.h

**********************************************************************/

#ifndef __AUDACITY_CONTRAST_DIALOG__
#define __AUDACITY_CONTRAST_DIALOG__

#include "wxPanelWrapper.h" // to inherit
#include "ContrastBase.h"

class wxButton;
class wxTextCtrl;

class NumericTextCtrl;

//----------------------------------------------------------------------------
// ContrastDialog
//----------------------------------------------------------------------------

// Declare window functions

class ContrastDialog final : public ContrastBase, public wxDialogWrapper
{
public:
    // constructors and destructors
    ContrastDialog(wxWindow* parent, wxWindowID id, const TranslatableString& title, const wxPoint& pos);

    wxButton* m_pButton_UseCurrentF;
    wxButton* m_pButton_UseCurrentB;
    wxButton* m_pButton_GetURL;
    wxButton* m_pButton_Export;
    wxButton* m_pButton_Reset;
    wxButton* m_pButton_Close;

    NumericTextCtrl* mForegroundStartT;
    NumericTextCtrl* mForegroundEndT;
    NumericTextCtrl* mBackgroundStartT;
    NumericTextCtrl* mBackgroundEndT;

private:
    // handlers
    void OnChar(wxKeyEvent& event);
    void OnGetURL(wxCommandEvent& event);
    void OnExport(wxCommandEvent& event);
    void OnGetForeground(wxCommandEvent& event);
    void OnGetBackground(wxCommandEvent& event);
    void results();
    void OnReset(wxCommandEvent& event);
    void OnClose(wxCommandEvent& event);

    wxTextCtrl* mForegroundRMSText;
    wxTextCtrl* mBackgroundRMSText;
    wxTextCtrl* mPassFailText;
    wxTextCtrl* mDiffText;

    AudacityProject& GetProject() override;

    DECLARE_EVENT_TABLE()
};

#endif
