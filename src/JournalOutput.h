/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file JournalOutput.h
  @brief The output stream of the journal system

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_JOURNAL_OUTPUT__
#define __AUDACITY_JOURNAL_OUTPUT__

#include <initializer_list>

class wxArrayString;
class wxString;

namespace Journal
{
   constexpr auto SeparatorCharacter = ',';
   constexpr auto EscapeCharacter = '\\';
   constexpr auto CommentCharacter = '#';

   //\brief Whether actually recording.
   // IsRecording() && IsReplaying() is possible
   bool IsRecording();

   //\brief open the output stream, return success code
   bool OpenOut( const wxString &fullPath );

   //\brief write the strings to the output journal, if recording
   // None of them may contain newlines
   void Output( const wxString &string );
   void Output( const wxArrayString &strings );
   void Output( std::initializer_list< const wxString > strings );

   //\brief if recording, emit a comment in the output journal that will have
   // no effect on playback
   void Comment( const wxString &string );
}

#endif
