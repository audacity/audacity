/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityDontAskAgainMessageDialog.h

  A yes-no dialog with a don't-ask-again checkbox.

**********************************************************************/

#pragma once

#include "wxPanelWrapper.h"

class WX_WRAPPERS_API AudacityDontAskAgainMessageDialog : public wxDialogWrapper
{
public:
    AudacityDontAskAgainMessageDialog(
        wxWindow* parent, const TranslatableString& caption, const TranslatableString& message);

    bool ShowDialog();
    bool IsChecked() const;

private:
    DECLARE_EVENT_TABLE();

    void OnCheckBoxEvent(wxCommandEvent& evt);
    void OnClose(wxCloseEvent& event);

    bool mChecked = false;
};
