/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_ABOUT_DLG__
#define __AUDACITY_ABOUT_DLG__

#include <vector>
#include "widgets/wxPanelWrapper.h" // to inherit

extern const wxString VerCheckArgs();
extern const wxString VerCheckUrl();
extern const wxString VerCheckHtml();

class wxStaticBitmap;

class ShuttleGui;

struct AboutDialogCreditItem {
   wxString description;
   int role;

   AboutDialogCreditItem(wxString &&description_, int role_)
      : description(description_), role(role_)
   {}

   // No copy, use the move
   AboutDialogCreditItem(const AboutDialogCreditItem&) PROHIBITED;
   AboutDialogCreditItem& operator= (const AboutDialogCreditItem&) PROHIBITED;

   // Move constructor, because wxString lacks one
   AboutDialogCreditItem(AboutDialogCreditItem &&moveMe)
      : role(moveMe.role)
   {
      description.swap(moveMe.description);
   }

   ~AboutDialogCreditItem() {}
};

using AboutDialogCreditItemsList = std::vector<AboutDialogCreditItem>;

class AboutDialog final : public wxDialogWrapper {
   DECLARE_DYNAMIC_CLASS(AboutDialog)

 public:
   AboutDialog(wxWindow * parent);
   virtual ~ AboutDialog();

   static AboutDialog *ActiveIntance();

   void OnOK(wxCommandEvent & event);

   wxStaticBitmap *icon;

   DECLARE_EVENT_TABLE()

 private:
   enum Role {
      roleTeamMember,
      roleEmeritusTeam,
      roleDeceased,
      roleContributor,
      roleGraphics,
      roleLibrary,
      roleThanks
   };

   AboutDialogCreditItemsList creditItems;
   void PopulateAudacityPage( ShuttleGui & S );
   void PopulateLicensePage( ShuttleGui & S );
   void PopulateInformationPage (ShuttleGui & S );

   void CreateCreditsList();
   void AddCredit(wxString &&description, Role role);
   wxString GetCreditsByRole(AboutDialog::Role role);

   void AddBuildinfoRow( wxString* htmlstring, const wxChar * libname, const wxChar * libdesc, const wxString &status);
   void AddBuildinfoRow( wxString* htmlstring, const wxChar * libname, const wxChar * libdesc);
};

#endif
