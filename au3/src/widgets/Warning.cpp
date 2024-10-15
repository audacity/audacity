/**********************************************************************

  Audacity: A Digital Audio Editor

  Warning.cpp

  Dominic Mazzoni

*******************************************************************//**

\class WarningDialog
\brief Gives a warning message, that can be dismissed, with crucially
the ability to not see similar warnings again for this session.

*//********************************************************************/




#include "Warning.h"

#include "ShuttleGui.h"

#include <wx/artprov.h>
#include <wx/checkbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include "wxPanelWrapper.h"

class WarningDialog final : public wxDialogWrapper
{
 public:
   // constructors and destructors
   WarningDialog(wxWindow *parent,
                 const TranslatableString &message,
                 const TranslatableString &footer,
                 bool showCancelButton);

 private:
   void OnOK(wxCommandEvent& event);

   wxCheckBox *mCheckBox;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(WarningDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, WarningDialog::OnOK)
END_EVENT_TABLE()

const TranslatableString &DefaultWarningFooter()
{
   static auto result = XXO("Don't show this warning again");
   return result;
}

WarningDialog::WarningDialog(wxWindow *parent, const TranslatableString &message,
                             const TranslatableString &footer,
                             bool showCancelButton)
:  wxDialogWrapper(parent, wxID_ANY, XO("Warning"),
            wxDefaultPosition, wxDefaultSize,
            (showCancelButton ? wxDEFAULT_DIALOG_STYLE : wxCAPTION | wxSYSTEM_MENU)) // Unlike wxDEFAULT_DIALOG_STYLE, no wxCLOSE_BOX.
{
   SetName();

   SetIcon(wxArtProvider::GetIcon(wxART_WARNING, wxART_MESSAGE_BOX));
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(10);
   S.StartVerticalLay(false);
   {
      S.AddFixedText(message);
      mCheckBox = S.AddCheckBox(footer, false);
   }
   S.EndVerticalLay();

   S.SetBorder(0);
   S.AddStandardButtons(showCancelButton ? eOkButton | eCancelButton : eOkButton);

   Layout();
   GetSizer()->Fit(this);
   CentreOnParent();
}

void WarningDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
   EndModal(mCheckBox->GetValue() ? wxID_NO : wxID_YES); // return YES, if message should be shown again
}

int ShowWarningDialog(wxWindow *parent,
                      const wxString &internalDialogName,
                      const TranslatableString &message,
                      bool showCancelButton,
                      const TranslatableString &footer)
{
   auto key = WarningDialogKey(internalDialogName);
   if (!gPrefs->Read(key, (long) true)) {
      return wxID_OK;
   }

   WarningDialog dlog(parent, message, footer, showCancelButton);

   int retCode = dlog.ShowModal();
   if (retCode == wxID_CANCEL)
      return retCode;

   gPrefs->Write(key, (retCode == wxID_YES));
   gPrefs->Flush();
   return wxID_OK;
}
