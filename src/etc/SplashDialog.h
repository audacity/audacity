/**********************************************************************

  Audacity: A Digital Audio Editor

  SplashDialog.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_SPLASH_DLG__
#define __AUDACITY_SPLASH_DLG__

#include "wxPanelWrapper.h" // to inherit

class wxBitmap;
class ShuttleGui;
class AudacityProject;
class HtmlWindow;

class SplashDialog final : public wxDialogWrapper {
   DECLARE_DYNAMIC_CLASS(SplashDialog)
public:

   static void DoHelpWelcome( AudacityProject &project );

   SplashDialog(wxWindow * parent);
   virtual ~ SplashDialog();
   void OnOK(wxCommandEvent & event);
   static void Show2( wxWindow * pParent );

   DECLARE_EVENT_TABLE()

private:

   void OnChar(wxMouseEvent &event);
   void Populate( ShuttleGui & S );
   void OnDontShow( wxCommandEvent & Evt );

   HtmlWindow * mpHtml;
   std::unique_ptr<wxBitmap> m_pLogo; //vvv
   static SplashDialog * pSelf;
};

#endif
