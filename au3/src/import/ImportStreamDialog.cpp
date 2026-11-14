/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportStreamDialog.cpp

  Dominic Mazzoni

  Vitaly Sverchinsky split from Import.cpp

**********************************************************************/
#include "ImportStreamDialog.h"

#include <wx/listbox.h>
#include <wx/sizer.h>

#include "ShuttleGui.h"

#include "ImportPlugin.h"
#include "IteratorX.h"

BEGIN_EVENT_TABLE(ImportStreamDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, ImportStreamDialog::OnOk)
EVT_BUTTON(wxID_CANCEL, ImportStreamDialog::OnCancel)
END_EVENT_TABLE()

ImportStreamDialog::ImportStreamDialog(ImportFileHandle* _mFile, wxWindow* parent, wxWindowID id, const TranslatableString& title,
                                       const wxPoint& position, const wxSize& size, long style)
    : wxDialogWrapper(parent, id, title, position, size, style | wxRESIZE_BORDER)
{
    SetName();

    mFile = _mFile;
    scount = mFile->GetStreamCount();
    for (wxInt32 i = 0; i < scount; i++) {
        mFile->SetStreamUsage(i, FALSE);
    }

    ShuttleGui S{ this, eIsCreating };
    {
        S.SetBorder(5);

        StreamList
            =S
              .Prop(1)
              .Position(wxEXPAND | wxALIGN_LEFT | wxALL)
              .Style(wxLB_EXTENDED | wxLB_ALWAYS_SB)
              .AddListBox(
                  transform_container<wxArrayStringEx>(
                      mFile->GetStreamInfo(),
                      std::mem_fn(&TranslatableString::Translation)));

        S.AddStandardButtons();
    }

    SetAutoLayout(true);
    GetSizer()->Fit(this);

    SetSize(400, 200);
}

ImportStreamDialog::~ImportStreamDialog()
{
}

void ImportStreamDialog::OnOk(wxCommandEvent& WXUNUSED(event))
{
    wxArrayInt selitems;
    int sels = StreamList->GetSelections(selitems);
    for (wxInt32 i = 0; i < sels; i++) {
        mFile->SetStreamUsage(selitems[i], TRUE);
    }
    EndModal(wxID_OK);
}

void ImportStreamDialog::OnCancel(wxCommandEvent& WXUNUSED(event))
{
    EndModal(wxID_CANCEL);
}
