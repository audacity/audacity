/**********************************************************************

  Audacity: A Digital Audio Editor

  SplashDialog.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_SPLASH_DLG__
#define __AUDACITY_SPLASH_DLG__

#include <wx/dialog.h>

class wxBoxSizer;
class wxStaticBitmap;
class wxBitmap;
class ShuttleGui;
class AudacityProject;
class wxCheckbox;
class HtmlWindow;

class SplashDialog:public wxDialog {
   DECLARE_DYNAMIC_CLASS(SplashDialog)
public:
   SplashDialog(wxWindow * parent);
   virtual ~ SplashDialog();
   void OnOK(wxCommandEvent & event);
   static void Show2( wxWindow * pParent );

   DECLARE_EVENT_TABLE()

private:

   void Populate( ShuttleGui & S );
   void OnDontShow( wxCommandEvent & Evt );

   HtmlWindow * mpHtml;
   wxStaticBitmap* m_pIcon;
   wxBitmap* m_pLogo; //vvv
   static SplashDialog * pSelf;
};

#endif
