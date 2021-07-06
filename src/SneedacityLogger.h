/**********************************************************************

  Sneedacity: A Digital Audio Editor

  SneedacityLogger.h

  Dominic Mazzoni

  This is the main source file for Sneedacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#ifndef __SNEEDACITY_LOGGER__
#define __SNEEDACITY_LOGGER__





#include "MemoryX.h"
#include "Prefs.h"
#include <wx/log.h> // to inherit
#include <wx/event.h> // to inherit wxEvtHandler

class wxFrame;
class wxTextCtrl;

class SNEEDACITY_DLL_API SneedacityLogger final : public wxEvtHandler,
                             public wxLog,
                             public PrefsListener
{
 public:

   ~SneedacityLogger() override;
 
   // Get the singleton instance or null
   static SneedacityLogger *Get();

   void Show(bool show = true);

   bool SaveLog(const wxString &fileName) const;
   bool ClearLog();

   wxString GetLog(int count = 0);

 protected:
   void Flush()  override;
   void DoLogText(const wxString & msg) override;

 private:
   SneedacityLogger();

   void OnCloseWindow(wxCloseEvent & e);
   void OnClose(wxCommandEvent & e);
   void OnClear(wxCommandEvent & e);
   void OnSave(wxCommandEvent & e);

   // PrefsListener implementation
   void UpdatePrefs() override;

   Destroy_ptr<wxFrame> mFrame;
   wxTextCtrl *mText;
   wxString mBuffer;
   bool mUpdated;
};

#endif
