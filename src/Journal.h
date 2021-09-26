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

namespace Journal
{
   //\brief Whether recording is enabled; but recording will happen only if this
   // was true at application start up
   bool RecordEnabled();

   //\brief Change the enablement of recording and store in preferences
   //\return whether successful
   bool SetRecordEnabled(bool value);

   //\brief Whether actually replaying.
   // IsRecording() && IsReplaying() is possible
   bool IsReplaying();

   //\brief Set the played back journal file at start up
   void SetInputFileName( const wxString &path );

   //\brief Initialize playback if a file name has been set, and initialize
   // output if recording is enabled.
   // Must be called after wxWidgets initializes.
   // Application initialization is late enough.
   // @param dataDir the output journal.txt will be in this directory, and the
   // input file, if it was relative, is made absolute with respect to it
   // @return true if successful
   bool Begin( const FilePath &dataDir );

   //\brief Consume next line from the input journal (skipping blank lines and
   // comments) and tokenize it.
   // Throws SyncException if no next line or not replaying
   wxArrayStringEx GetTokens();

   //\brief if playing back and commands remain, may execute one.
   // May throw SyncException if playing back but none remain, or if other error
   // conditions are encountered.
   // Returns true if any command was dispatched
   bool Dispatch();

   //\brief If recording, output the strings; if playing back, require
   // identical strings.  None of them may contain newlines
   void Sync( const wxString &string );
   void Sync( const wxArrayString &strings );
   void Sync( std::initializer_list< const wxString > strings );

   //! Function that returns a value which will be written to the journal
   /*! In future, might generalize to more return values and of other types */
   using InteractiveAction = std::function< int() >;

   //! Call action only if not replaying; synchronize on string and int values
   /*!
    If not replaying, call the function, and if recording, output the string
    and the return value.

    If replaying, skip the action; Sync on the string; parse a value from
    the journal; throw SyncException if the value is ill-formed; otherwise
    output the value (if also recording), and return it
    */
   int IfNotPlaying(
      const wxString &string, const InteractiveAction &action );

   //\brief Get the value that the application will return to the command line
   int GetExitCode();

   //\brief thrown when playback of a journal doesn't match the recording
   class SyncException : public AudacityException {
   public:
      SyncException();
      ~SyncException() override;

      // The delayed handler action forces the program to quit gracefully,
      // so that the test playback failure is prompty reported.  This is
      // unlike other AudacityExceptions that roll back the project and
      // continue.
      void DelayedHandlerAction() override;
   };
}

#endif
