/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityDontAskAgainMessageDialog.cpp

  A yes-no dialog with a don't-ask-again checkbox.

**********************************************************************/

#include "AudacityDontAskAgainMessageDialog.h"

#include <wx/checkbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

namespace {
constexpr auto style = wxDEFAULT_DIALOG_STYLE | wxCENTRE;
}

BEGIN_EVENT_TABLE(AudacityDontAskAgainMessageDialog, wxDialogWrapper)
EVT_CHECKBOX(wxID_ANY, AudacityDontAskAgainMessageDialog::OnCheckBoxEvent)
EVT_CLOSE(AudacityDontAskAgainMessageDialog::OnClose)
END_EVENT_TABLE()

AudacityDontAskAgainMessageDialog::AudacityDontAskAgainMessageDialog(
    wxWindow* parent, const TranslatableString& caption,
    const TranslatableString& message)
    : wxDialogWrapper(
        parent, wxID_ANY, caption, wxDefaultPosition, wxDefaultSize, style)
{
    wxStaticText* messageText
        =new wxStaticText(this, wxID_ANY, message.Translation());

    // Add a checkbox
    wxCheckBox* checkBox
        =new wxCheckBox(this, wxID_ANY, XO("Don't ask me again").Translation());

    // Add sizers to arrange controls
    wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
    constexpr auto border = 10;
    mainSizer->Add(messageText, 0, wxALL | wxALIGN_CENTER, border);

    // This is where you specified wxOK | wxCANCEL buttons which you saw in your
    // dialog wxOK had no effect because this flag isn't handled by the wxDialog,
    // but even if it did, then you would likely got two rows of buttons
    wxStdDialogButtonSizer* buttonSizer
        =CreateStdDialogButtonSizer(wxYES | wxNO);

    // Add checkbox to the sizer so that it shows first
    buttonSizer->Insert(0, checkBox, 0, wxALL | wxALIGN_CENTER, border);
    mainSizer->Add(buttonSizer, 0, wxALL | wxALIGN_CENTER, border);

    SetSizerAndFit(mainSizer);
    // Manually implement wxCENTRE flag behavior
    if ((style | wxCENTRE) != 0) {
        CentreOnParent();
    }

    SetEscapeId(wxID_NO);
}

bool AudacityDontAskAgainMessageDialog::ShowDialog()
{
    return ShowModal() == wxID_YES;
}

bool AudacityDontAskAgainMessageDialog::IsChecked() const
{
    return mChecked;
}

void AudacityDontAskAgainMessageDialog::OnCheckBoxEvent(wxCommandEvent& evt)
{
    mChecked = evt.IsChecked();
}

void AudacityDontAskAgainMessageDialog::OnClose(wxCloseEvent& event)
{
    EndModal(wxID_NO);
}
