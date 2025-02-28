/*!********************************************************************

  Audacity: A Digital Audio Editor

  @name JournalWindowPaths.h

  @brief utilities to identify corresponding windows between recording
  and playback runs

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_JOURNAL_WINDOW_PATHS__
#define __AUDACITY_JOURNAL_WINDOW_PATHS__

class Identifier;
class wxWindow;

namespace Journal {
namespace WindowPaths {
using Path = Identifier;

// When recording, find a string to identify the window in the journal
Path FindPath(const wxWindow& window);

// When playing, find a window by path, corresponding to the window that had the
// same path in a previous run
wxWindow* FindByPath(const Path& path);
}
}

#endif
