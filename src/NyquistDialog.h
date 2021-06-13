/**********************************************************************

  Audacity: A Digital Audio Editor

  NyquistDialog.h

  Roger B. Dannenberg

**********************************************************************/

#ifndef __AUDACITY_NYQUIST_WINDOW__
#define __AUDACITY_NYQUIST_WINDOW__

#include <wx/defs.h>

#include "BatchCommands.h"
#include "Prefs.h"

class wxWindow;
class wxTextCtrl;
class wxListCtrl;
class wxListEvent;
class wxButton;
class wxTextCtrl;
class AudacityProject;
class ShuttleGui;

class NyquistDialog : public wxDialogWrapper {
 public:
   // constructors and destructors
   NyquistDialog(
      wxWindow * parent, AudacityProject &project, bool bInherited=false);
   virtual ~NyquistDialog();
 public:
   // Populate methods NOT virtual.
   void Populate();
   void PopulateOrExchange( ShuttleGui & S );
   virtual void OnRunCommand(wxCommandEvent& event);
   virtual void OnBrowse(wxCommandEvent& event);
   virtual void OnCancel(wxCommandEvent & event);
   //virtual void OnHelp(wxCommandEvent & event);

   virtual wxString GetHelpPageName() {return "Apply_Macro";};

   void PopulateMacros();
   static CommandID MacroIdOfName( const wxString & MacroName );
   void ApplyNyquist();

   // Nyquist-to-Host interface
   // same (for now) as Nyquist.cpp StaticOSCallback
   static void StaticOSCallback(void* userdata);
   void OSCallback();
   bool mStop;
   bool mBreak;
   bool mCont;
   static void StaticOutputCallback(int c, void* userdata);
   void OutputCallback(int c);


   wxButton *mOK;
   wxButton *mCancel;
   wxTextCtrl *mResults;
   wxTextCtrl *mNyquistFile;
   wxTextCtrl* mNyquistOut;

   // It seems that Audacity always calls wxWidgets through a translation
   // layer. Passing TranslatableString to a wxWidgets method is explicitly
   // blocked (see comments on TranslatableString). Do we know why? I don't.
   // We can't pass .Translate() to a &wxString, so this translation function
   // is called to do it...
   void writeText(TranslatableString ts) {
       wxString s(ts.Translation());
       mNyquistOut->WriteText(s);
   }

   void writeText(const char* ts) {
       mNyquistOut->WriteText(ts);
   }

   /*
   bool mAbort;
   bool mbExpanded;
   wxString mActiveMacro;
   wxString mMacroBeingRenamed;
   */
protected:
   const AudacityProject &mProject;
   //const MacroCommandsCatalog mCatalog;

   DECLARE_EVENT_TABLE()
};

#endif
