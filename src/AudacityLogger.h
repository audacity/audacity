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

#include <wx/event.h>
#include <wx/log.h>
#include <wx/frame.h>
#include <wx/textctrl.h>
#include <wx/string.h>

#include "Experimental.h"

class AudacityLogger:public wxEvtHandler, public wxLog {
 public:
   AudacityLogger();
   virtual ~AudacityLogger();

   void Show(bool show = true);
   void Destroy();

#if defined(EXPERIMENTAL_CRASH_REPORT)
   wxString GetLog();
#endif

 protected:
   virtual void Flush();
   virtual void DoLogString(const wxChar *szString, time_t t);

 private:
   void OnCloseWindow(wxCloseEvent & e);
   void OnClose(wxCommandEvent & e);
   void OnClear(wxCommandEvent & e);
   void OnSave(wxCommandEvent & e);

   wxFrame *mFrame;
   wxTextCtrl *mText;
   wxString mBuffer;
   bool mUpdated;
};

#endif
