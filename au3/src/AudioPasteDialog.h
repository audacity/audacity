/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioPasteDialog.h

  @author Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include "wxPanelWrapper.h"

class wxCheckBox;
class ShuttleGui;

class AudioPasteDialog final : public wxDialogWrapper
{
    wxCheckBox* mDontShowAgain { nullptr };
    wxULongLong mCopiedBytesNum {};
public:

    DECLARE_EVENT_TABLE()

public:
    enum DialogResult {
        KEEP = wxID_HIGHEST + 1,
        DISCARD
    };

    AudioPasteDialog(wxWindow* parent, wxULongLong nCopiedBytes, int winid = wxID_ANY);

private:
    void PopulateOrExchange(ShuttleGui& S);
    void OnContinue(wxCommandEvent&);
    void OnCancel(wxCommandEvent&);
};
