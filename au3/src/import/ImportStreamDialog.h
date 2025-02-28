/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportStreamDialog.h

  Dominic Mazzoni

  Vitaly Sverchinsky split from Import.h

**********************************************************************/

#pragma once

#include "wxPanelWrapper.h"

class ImportFileHandle;
class wxListBox;

class ImportStreamDialog final : public wxDialogWrapper
{
public:
    // constructors and destructors
    ImportStreamDialog(ImportFileHandle* _mFile, wxWindow* parent, wxWindowID id, const TranslatableString& title,
                       const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
                       long style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
    virtual ~ImportStreamDialog();

private:
    ImportFileHandle* mFile;
    wxInt32 scount;
    wxListBox* StreamList;

private:
    void OnOk(wxCommandEvent& event);
    void OnCancel(wxCommandEvent& event);

private:
    DECLARE_EVENT_TABLE()
};
