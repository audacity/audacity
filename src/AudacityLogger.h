/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityLogger.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#ifndef __AUDACITY_LOGGER__
#define __AUDACITY_LOGGER__





#include <functional>
#include <wx/log.h> // to inherit
#include <wx/event.h> // to inherit wxEvtHandler

class wxFrame;
class wxTextCtrl;

class AUDACITY_DLL_API AudacityLogger final
   : public wxEvtHandler, public wxLog
{
 public:

   ~AudacityLogger() override;
 
   // Get the singleton instance or null
   static AudacityLogger *Get();

   bool SaveLog(const wxString &fileName) const;
   bool ClearLog();

   //! Retrieve all or some of the lines since most recent ClearLog or start of program
   /*! If `count == 0` or is more than the number of lines, return all; else return the last `count` lines */
   wxString GetLog(int count = 0);

   void Flush() override;

   //! Get all the accumulated text since program start or last ClearLog()
   const wxString &GetBuffer() const { return mBuffer; }

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

   Listener mListener;
   wxString mBuffer;
   bool mUpdated;
};

#endif
