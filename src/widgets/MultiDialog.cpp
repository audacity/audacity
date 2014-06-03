/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   MultiDialog.h

   Monty
   Vaughan Johnson

*******************************************************************//**

\class MultiDialog
\brief A multi purpose dialog, mainly used to show lists of orphaned or
damaged block files.  It is a good alternative to having a dialog pop up
for each problem encountered, since there can be many orphans.

*//*******************************************************************/

#include "../Audacity.h"
#include "../Project.h"

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
   MultiDialog(wxString message,
               wxString title,
               const wxChar **buttons, wxString boxMsg, bool log);
   ~MultiDialog() {};

private:
   void OnOK( wxCommandEvent &event );
   void OnShowLog(wxCommandEvent& event);

   wxRadioBox* mRadioBox;

   DECLARE_EVENT_TABLE()
};

#define ID_SHOW_LOG_BUTTON 3333

BEGIN_EVENT_TABLE(MultiDialog, wxDialog)
   EVT_BUTTON( wxID_OK, MultiDialog::OnOK )
   EVT_BUTTON(ID_SHOW_LOG_BUTTON, MultiDialog::OnShowLog)
END_EVENT_TABLE()

MultiDialog::MultiDialog(wxString message,
                         wxString title,
                         const wxChar **buttons, wxString boxMsg, bool log)
   : wxDialog(NULL, (wxWindowID)-1, title,
               wxDefaultPosition, wxDefaultSize,
               wxCAPTION) // not wxDEFAULT_DIALOG_STYLE because we don't want wxCLOSE_BOX and wxSYSTEM_MENU
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *iconAndTextSizer = new wxBoxSizer( wxHORIZONTAL );

   wxBitmap bitmap = wxArtProvider::GetIcon(wxART_WARNING,
                                            wxART_MESSAGE_BOX);
   wxStaticBitmap *icon = new wxStaticBitmap(this, -1, bitmap);
   iconAndTextSizer->Add( icon, 0, wxCENTER );

   wxStaticText *statText = new wxStaticText(this, -1, message);
   statText->SetName(message); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
   iconAndTextSizer->Add(statText, 1, wxCENTER|wxLEFT,15 );

   vSizer->Add(iconAndTextSizer, 0, wxALIGN_LEFT|wxALL, 5);


   int count=0;
   while(buttons[count])count++;
   wxString *buttonLabels = new wxString[count];

   count=0;
   while(buttons[count]){
      buttonLabels[count] = buttons[count];
      count++;
   }

   mRadioBox = new wxRadioBox(this,-1,
                         boxMsg,
                         wxDefaultPosition, wxDefaultSize,
                         count, buttonLabels,
                         1, wxRA_SPECIFY_COLS);
   mRadioBox->SetName(boxMsg);
   mRadioBox->SetSelection(0);
   vSizer->Add(mRadioBox, 1, wxEXPAND | wxALIGN_CENTER | wxALL, 5);


   wxBoxSizer* buttonSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton* pButton;
   if(log)
   {
      pButton = new wxButton(this, ID_SHOW_LOG_BUTTON, _("Show Log for Details"));
      buttonSizer->Add(pButton, 0, wxALIGN_LEFT | wxALL, 5);
      pButton->SetDefault(); // Encourage user to look at files.

      buttonSizer->AddSpacer(40);
   }

   pButton = new wxButton(this, wxID_OK, _("OK"));
   if(!log)
      pButton->SetDefault();
   buttonSizer->Add(pButton, 0, wxALIGN_RIGHT | wxALL, 5);

   vSizer->Add(buttonSizer, 0, wxALIGN_CENTER | wxALL, 5);


   mainSizer->Add(vSizer, 0, wxALL, 5);
   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
   delete[] buttonLabels;
}

void MultiDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(mRadioBox->GetSelection());
}

void MultiDialog::OnShowLog(wxCommandEvent & WXUNUSED(event))
{
   GetActiveProject()->OnShowLog();
}


int ShowMultiDialog(wxString message,
                    wxString title,
                    const wxChar **buttons, wxString boxMsg, bool log)
{
   MultiDialog dlog(message, title, buttons, boxMsg, log);
   dlog.CentreOnParent();
   return dlog.ShowModal();
}

