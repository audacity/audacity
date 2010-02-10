/**********************************************************************

  Audacity: A Digital Audio Editor

  MultiDialog.cpp

  Monty

*******************************************************************//**

\class MultiDialog
\brief A multi purpose dialog, mainly used to show lists of orphaned or
damaged block files.  It is a good alternative to having a dialog pop up
for each problem encountered, since there can be many orphans.

*//*******************************************************************/

#include "../Audacity.h"

#include "MultiDialog.h"

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/icon.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/statbmp.h>
#include <wx/artprov.h>
#include <wx/radiobox.h>

class MultiDialog : public wxDialog
{
public:
   // constructors and destructors
   MultiDialog(wxString prompt,
               wxString title,
               const wxChar **buttons);
   
private:
   void OnOK( wxCommandEvent &event );
   wxRadioBox *mBox;

private:
   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(MultiDialog, wxDialog)
   EVT_BUTTON( wxID_OK, MultiDialog::OnOK )
END_EVENT_TABLE()
   
MultiDialog::MultiDialog(wxString prompt,
                         wxString title,
                         const wxChar **buttons):
   wxDialog(NULL, (wxWindowID)-1, title)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *icon_text = new wxBoxSizer( wxHORIZONTAL );

   wxBitmap bitmap = wxArtProvider::GetIcon(wxART_WARNING,
                                            wxART_MESSAGE_BOX);
   wxStaticBitmap *icon = new wxStaticBitmap(this, -1, bitmap);
   icon_text->Add( icon, 0, wxCENTER );

   wxStaticText *statText = new wxStaticText(this, -1, prompt);
   icon_text->Add(statText, 1, wxCENTER|wxLEFT,15 );

   vSizer->Add(icon_text, 0, wxALIGN_LEFT|wxALL, 5);
   
   int count=0;
   while(buttons[count])count++;
   wxString *prompts= new wxString[count];

   count=0;
   while(buttons[count]){
      prompts[count]=buttons[count];
      count++;
   }

   mBox = new wxRadioBox(this,-1,
                         _(" Please select an action "),
                         wxDefaultPosition, wxDefaultSize,
                         count, prompts,
                         1, wxRA_SPECIFY_COLS);
   mBox->SetName(_("Please select an action"));
   mBox->SetSelection(0);

   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   
   vSizer->Add(mBox, 1, wxGROW|wxALIGN_CENTER|wxALL, 5);
   vSizer->Add(ok, 0, wxALIGN_CENTER|wxALL, 5);

   mainSizer->Add(vSizer, 0, wxALL, 5);
   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
   delete[] prompts;
}

void MultiDialog::OnOK(wxCommandEvent &event)
{
   EndModal(mBox->GetSelection());
}

int ShowMultiDialog(wxString prompt,
                    wxString title,
                    const wxChar **buttons)
{
   MultiDialog dlog(prompt,title,buttons);
   dlog.CentreOnParent();
   return dlog.ShowModal();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: b84d77e0-4375-43f0-868e-3130e18c14c8

