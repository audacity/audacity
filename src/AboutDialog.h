/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_ABOUT_DLG__
#define __AUDACITY_ABOUT_DLG__

#include <wx/dialog.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/bitmap.h>

#include "ShuttleGui.h"

struct AboutDialogCreditItem {
   wxString description;
   int role;
};

WX_DECLARE_LIST(AboutDialogCreditItem, AboutDialogCreditItemsList);

class AboutDialog:public wxDialog {
   DECLARE_DYNAMIC_CLASS(AboutDialog)

 public:
   AboutDialog(wxWindow * parent);
   virtual ~ AboutDialog();

   void OnOK(wxCommandEvent & event);

   wxBoxSizer *topsizer;
   wxStaticBitmap *icon;
   wxBitmap *logo; //vvv

    DECLARE_EVENT_TABLE()
    
 private:
   enum Role {
      roleTeamDeveloper,
      roleTeamSupport,
      roleEmeritusDeveloper,
      roleEmeritusSupport,
      roleContributor,
      roleLibrary,
      roleThanks
   };
   
   AboutDialogCreditItemsList creditItems;
   void PopulateAudacityPage( ShuttleGui & S );
   void PopulateLicensePage( ShuttleGui & S );
   void PopulateInformationPage (ShuttleGui & S );

   void CreateCreditsList();
   void AddCredit(const wxString& description, Role role);
   wxString GetCreditsByRole(AboutDialog::Role role);

   void AddBuildinfoRow( wxString* htmlstring, const wxChar * libname, const wxChar * libdesc, wxString status);
   void AddBuildinfoRow( wxString* htmlstring, const wxChar * libname, const wxChar * libdesc);
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 6c53e56f-0045-4eaa-be7f-be6d034a8221

