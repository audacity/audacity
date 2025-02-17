/**********************************************************************

  Audacity: A Digital Audio Editor

  Journal.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_JOURNAL__
#define __AUDACITY_JOURNAL__

#include "Identifier.h"
class wxArrayString;
class wxArrayStringEx;
class wxString;

#include "AudacityException.h"

// Whether the journalling feature is shown to the end user
#undef END_USER_JOURNALLING

namespace Journal {
//\brief Whether recording is enabled; but recording will happen only if this
// was true at application start up
WX_INIT_API
bool RecordEnabled();

//\brief Change the enablement of recording and store in preferences
//\return whether successful
WX_INIT_API
bool SetRecordEnabled(bool value);

//\brief Whether actually replaying.
// IsRecording() && IsReplaying() is possible
WX_INIT_API
bool IsReplaying();

//\brief Set the played back journal file at start up
WX_INIT_API
void SetInputFileName(const wxString& path);

//\brief Initialize playback if a file name has been set, and initialize
// output if recording is enabled.
// Must be called after wxWidgets initializes.
// Application initialization is late enough.
// @param dataDir the output journal.txt will be in this directory, and the
// input file, if it was relative, is made absolute with respect to it
// @return true if successful
WX_INIT_API
bool Begin(const FilePath& dataDir);

//\brief Consume next line from the input journal (skipping blank lines and
// comments) and tokenize it.
// Throws SyncException if no next line or not replaying
WX_INIT_API
wxArrayStringEx GetTokens();

//\brief if playing back and commands remain, may execute one.
// May throw SyncException if playing back but none remain, or if other error
// conditions are encountered.
// Returns true if any command was dispatched
WX_INIT_API
bool Dispatch();

//\brief If recording, output the strings; if playing back, require
// identical strings.  None of them may contain newlines
WX_INIT_API
void Sync(const wxString& string);
WX_INIT_API
void Sync(const wxArrayString& strings);
WX_INIT_API
void Sync(std::initializer_list< const wxString > strings);

//! Function that returns a value which will be written to the journal
/*! In future, might generalize to more return values and of other types */
using InteractiveAction = std::function< int () >;

//! Call action only if not replaying; synchronize on string and int values
/*!
    If not replaying, call the function, and if recording, output the string
    and the return value.

    If replaying, skip the action; Sync on the string; parse a value from
    the journal; throw SyncException if the value is ill-formed; otherwise
    output the value (if also recording), and return it
    */
WX_INIT_API
int IfNotPlaying(
    const wxString& string, const InteractiveAction& action);

//\brief Get the value that the application will return to the command line
WX_INIT_API
int GetExitCode();

//\brief thrown when playback of a journal doesn't match the recording
class WX_INIT_API SyncException : public AudacityException
{
public:
    //! Constructs an exception with a message; message is logged into the `journallog.txt` file.
    explicit SyncException(const wxString& message);
    ~SyncException() override;

    // The delayed handler action forces the program to quit gracefully,
    // so that the test playback failure is prompty reported.  This is
    // unlike other AudacityExceptions that roll back the project and
    // continue.
    void DelayedHandlerAction() override;
};
}

#endif
