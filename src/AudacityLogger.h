/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityLogger.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#ifndef __AUDACITY_LOGGER__
#define __AUDACITY_LOGGER__

#include "Audacity.h"

#include "Experimental.h"

#include "MemoryX.h"
#include <wx/log.h> // to inherit
#include <wx/event.h> // to inherit wxEvtHandler

class wxFrame;
class wxTextCtrl;

class AudacityLogger final : public wxEvtHandler, public wxLog {
 public:
 
   // Get the singleton instance or null
   static AudacityLogger *Get();

   void Show(bool show = true);

   bool SaveLog(const wxString &fileName) const;
   bool ClearLog();

#if defined(EXPERIMENTAL_CRASH_REPORT)
   wxString GetLog();
#endif

 protected:
   void Flush()  override;
   void DoLogText(const wxString & msg) override;

 private:
   AudacityLogger();

   void OnCloseWindow(wxCloseEvent & e);
   void OnClose(wxCommandEvent & e);
   void OnClear(wxCommandEvent & e);
   void OnSave(wxCommandEvent & e);

   Destroy_ptr<wxFrame> mFrame;
   wxTextCtrl *mText;
   wxString mBuffer;
   bool mUpdated;
};

#endif
