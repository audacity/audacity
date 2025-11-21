/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityLogger.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LOGGER__
#define __AUDACITY_LOGGER__

#include <wx/log.h> // to inherit
#include <wx/event.h> // to inherit wxEvtHandler

class wxFrame;
class wxTextCtrl;

class FILES_API AudacityLogger final : public wxEvtHandler, public wxLog
{
public:

    ~AudacityLogger() override;

    // Get the singleton instance or null
    static AudacityLogger* Get();

    bool SaveLog(const wxString& fileName) const;
    bool ClearLog();

    //! Retrieve all or some of the lines since most recent ClearLog or start of program
    /*! If `count == 0` or is more than the number of lines, return all; else return the last `count` lines */
    wxString GetLog(int count = 0);

    //! Get all the accumulated text since program start or last ClearLog()
    const wxString& GetBuffer() const { return mBuffer; }

    void Flush() override;

    //! Type of function called by Flush
    /*! @return true if flush completed
     */
    using Listener = std::function< bool () >;

    //! Set the unique listener, returning any previous one
    Listener SetListener(Listener listener);

protected:
    void DoLogText(const wxString& msg) override;

private:
    AudacityLogger();

    Listener mListener;
    wxString mBuffer;
    bool mUpdated;
};

#endif
