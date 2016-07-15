/**********************************************************************

  Audacity: A Digital Audio Editor

  Warning.cpp

  Dominic Mazzoni

*******************************************************************//**

\class WarningDialog
\brief Gives a warning message, that can be dismissed, with crucially
the ability to not see similar warnings again for this session.

*//********************************************************************/



#include "../Audacity.h"

#include "Warning.h"

#include "../Prefs.h"
#include "../ShuttleGui.h"

#include <wx/artprov.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include "wxPanelWrapper.h"

class WarningDialog final : public wxDialogWrapper
{
 public:
   // constructors and destructors
   WarningDialog(wxWindow *parent,
                 wxString message,
                 bool showCancelButton);

 private:
   void OnOK(wxCommandEvent& event);

   wxCheckBox *mCheckBox;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(WarningDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, WarningDialog::OnOK)
END_EVENT_TABLE()

WarningDialog::WarningDialog(wxWindow *parent, wxString message, bool showCancelButton)
:  wxDialogWrapper(parent, wxID_ANY, (wxString)_("Warning"),
            wxDefaultPosition, wxDefaultSize,
            (showCancelButton ? wxDEFAULT_DIALOG_STYLE : wxCAPTION | wxSYSTEM_MENU)) // Unlike wxDEFAULT_DIALOG_STYLE, no wxCLOSE_BOX.
{
   SetName(GetTitle());

   SetIcon(wxArtProvider::GetIcon(wxART_WARNING, wxART_MESSAGE_BOX));
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(10);
   S.StartVerticalLay(false);
   {
      S.AddUnits(message);
      mCheckBox = S.AddCheckBox(_("Don't show this warning again"), wxT("false"));
   }

   S.SetBorder(0);
   S.AddStandardButtons(showCancelButton ? eOkButton | eCancelButton : eOkButton);

   Fit();
   CentreOnParent();
}

void WarningDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
   EndModal(mCheckBox->GetValue() ? wxID_NO : wxID_YES); // return YES, if message should be shown again
}

int ShowWarningDialog(wxWindow *parent,
                      const wxString &internalDialogName,
                      const wxString &message,
                      bool showCancelButton)
{
   wxString key(wxT("/Warnings/") + internalDialogName);
   if (!gPrefs->Read(key, (long) true)) {
      return wxID_OK;
   }

   WarningDialog dlog(parent, message, showCancelButton);

   int retCode = dlog.ShowModal();
   if (retCode == wxID_CANCEL)
      return retCode;

   gPrefs->Write(key, (retCode == wxID_YES));
   gPrefs->Flush();
   return wxID_OK;
}
