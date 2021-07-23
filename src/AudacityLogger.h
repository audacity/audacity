/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityLogger.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LOGGER__
#define __AUDACITY_LOGGER__

#include <functional>
#include "MemoryX.h"
#include "Prefs.h"
#include <wx/log.h> // to inherit
#include <wx/event.h> // to inherit wxEvtHandler

class wxFrame;
class wxTextCtrl;

class AUDACITY_DLL_API AudacityLogger final : public wxEvtHandler,
                             public wxLog
{
 public:

   ~AudacityLogger() override;
 
   // Get the singleton instance or null
   static AudacityLogger *Get();

   void Show(bool show = true);

   bool SaveLog(const wxString &fileName) const;
   bool ClearLog();

   wxString GetLog(int count = 0);

   //! Get all the accumulated text since program start or last ClearLog()
   const wxString &GetBuffer() const { return mBuffer; }

 protected:
   void Flush()  override;

   //! Type of function called by Flush
   /*! @return true if flush completed
    */
   using Listener = std::function< bool() >;

   //! Set the unique listener, returning any previous one
   Listener SetListener(Listener listener);

protected:
   void DoLogText(const wxString & msg) override;

 private:
   AudacityLogger();

   void OnCloseWindow(wxCloseEvent & e);
   void OnClose(wxCommandEvent & e);
   void OnClear(wxCommandEvent & e);
   void OnSave(wxCommandEvent & e);

   Listener mListener;
   wxString mBuffer;
   bool mUpdated;
};

#endif
