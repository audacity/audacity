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


#include "MultiDialog.h"

#include "../ShuttleGui.h"

#include <wx/app.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/icon.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/statbmp.h>
#include <wx/artprov.h>
#include <wx/radiobox.h>
#include <wx/bmpbuttn.h>


#include "wxPanelWrapper.h"
#include "../LogWindow.h"
#include "../Theme.h"
#include "../AllThemeResources.h"
#include "../widgets/HelpSystem.h"

class MultiDialog final : public wxDialogWrapper
{
public:
   MultiDialog(wxWindow * pParent, 
               const TranslatableString &message,
               const TranslatableString &title,
               const TranslatableStrings &buttons,
               const wxString &helpPage,
               const TranslatableString &boxMsg, bool log);
   ~MultiDialog() {};

private:
   void OnOK( wxCommandEvent &event );
   void OnShowLog(wxCommandEvent& event);
   void OnHelp(wxCommandEvent& event);

   wxRadioBox* mRadioBox;
   ManualPageID mHelpPage;

   DECLARE_EVENT_TABLE()
};

#define ID_SHOW_LOG_BUTTON 3333

BEGIN_EVENT_TABLE(MultiDialog, wxDialogWrapper)
   EVT_BUTTON( wxID_OK, MultiDialog::OnOK )
   EVT_BUTTON(ID_SHOW_LOG_BUTTON, MultiDialog::OnShowLog)
   EVT_BUTTON(wxID_HELP, MultiDialog::OnHelp)
END_EVENT_TABLE()

MultiDialog::MultiDialog(wxWindow * pParent,
                         const TranslatableString &message,
                         const TranslatableString &title,
                         const TranslatableStrings &buttons,
                         const wxString &helpPage,
                         const TranslatableString &boxMsg, 
                         bool log
   )
   : wxDialogWrapper(pParent, wxID_ANY, title,
               wxDefaultPosition, wxDefaultSize,
               wxCAPTION), // not wxDEFAULT_DIALOG_STYLE because we don't want wxCLOSE_BOX and wxSYSTEM_MENU
    mHelpPage( helpPage)
{
   SetName();

   ShuttleGui S{ this, eIsCreating };
   {
      S.SetBorder( 5 );
      S.StartVerticalLay( 0 );
      {
         S.StartHorizontalLay(wxALIGN_LEFT | wxALL, 0);
         {
            S.SetBorder( 0 );
            wxBitmap bitmap = wxArtProvider::GetIcon(wxART_WARNING,
               wxART_MESSAGE_BOX);
            auto icon = safenew wxStaticBitmap(S.GetParent(), -1, bitmap);
            S
               .Position( wxCENTER )
               .AddWindow( icon );

            S.SetBorder( 15 );
            S.Prop(1).AddVariableText( message, false, wxCENTER | wxLEFT );
         }
         S.EndHorizontalLay();

         const auto buttonLabels = transform_container<wxArrayStringEx>(
            buttons, std::mem_fn( &TranslatableString::Translation ) );

         const auto count = buttons.size();
         
         const auto boxStr = boxMsg.Translation();

         S.SetBorder( 5 );

         mRadioBox = safenew wxRadioBox(S.GetParent(), -1,
            boxStr,
            wxDefaultPosition, wxDefaultSize,
            count, count ? &buttonLabels[0] : nullptr,
            1, wxRA_SPECIFY_COLS);
         mRadioBox->SetSelection(0);
         S.Prop( 1 )
            .Name( boxMsg )
            .Position(wxEXPAND | wxALL)
            .AddWindow( mRadioBox );


         S.StartHorizontalLay(wxALIGN_CENTER | wxALL, 0);
         {
            if (log)
            {
               S
                  .Id(ID_SHOW_LOG_BUTTON)
                  .AddButton(
                     XXO("Show Log for Details"), wxALIGN_LEFT | wxALL,
                     // set default to encourage user to look at files.
                     true);

               S.AddSpace(40, 0);
            }

            auto pButton = S.Id(wxID_OK)
               .AddButton(XXO("OK"), wxALIGN_CENTER, !log);

            if (!mHelpPage.empty()) {
               auto pHelpBtn = S.Id(wxID_HELP)
                  .AddBitmapButton(theTheme.Bitmap(bmpHelpIcon), wxALIGN_CENTER, false);
               pHelpBtn->SetToolTip(XO("Help").Translation());
               pHelpBtn->SetLabel(XO("Help").Translation());       // for screen readers
            }
         }
         S.EndHorizontalLay();
      }
      S.EndVerticalLay();
   }

   SetAutoLayout(true);
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
}

void MultiDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(mRadioBox->GetSelection());
}

void MultiDialog::OnShowLog(wxCommandEvent & WXUNUSED(event))
{
   LogWindow::Show();
}

void MultiDialog::OnHelp(wxCommandEvent & WXUNUSED(event))
{
   HelpSystem::ShowHelp(FindWindow(wxID_HELP), mHelpPage, true);
}

int ShowMultiDialog(const TranslatableString &message,
   const TranslatableString &title,
   const TranslatableStrings &buttons,
   const wxString &helpPage,
   const TranslatableString &boxMsg, bool log)
{
   wxWindow * pParent = wxTheApp->GetTopWindow();

   // We want a parent we can display over, so don't make it a parent if top
   // window is a STAY_ON_TOP.
   if (pParent) {
      if ((pParent->GetWindowStyle() & wxSTAY_ON_TOP) == wxSTAY_ON_TOP)
         pParent = NULL;
   }
   MultiDialog dlog(pParent,
      message, title, buttons, helpPage, boxMsg, log);
   // If dialog does not have a parent, cannot be centred on it.
   if (pParent != NULL)
      dlog.CentreOnParent();
   else {
      dlog.CenterOnScreen();
      // and after centring move the dialog left by the size of the dialog.
      // Likely to help if we have the splash screen visible, or if
      // we're spanning two equally sized monitors.
      // Unlikely to make things worse.
      wxSize Size = dlog.GetSize();
      Size.SetHeight( 10 );
      wxPoint Pos = dlog.GetPosition() -Size;
      dlog.Move(Pos);
   }
   return dlog.ShowModal();
}

const TranslatableString &DefaultMultiDialogMessage()
{
   static auto result = XO("Please select an action");
   return result;
}
