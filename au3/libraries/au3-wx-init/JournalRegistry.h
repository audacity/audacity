/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file JournalRegistry.h
  @brief Journal system's error status, command dictionary, initializers

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_JOURNAL_REGISTRY__
#define __AUDACITY_JOURNAL_REGISTRY__

#include <functional>
#include <unordered_map>
class wxArrayStringEx;
class wxString;

namespace Journal {
//\brief Whether the initialization or playback of journalling failed
WX_INIT_API
bool GetError();

//\brief Make GetError() return false
// If replaying, then the program will return non-zero status to the command
// line
WX_INIT_API
void SetError();

//\brief Type of a function that interprets a line of the input journal.
// It may indicate failure either by throwing SyncException or returning
// false (which will cause Journal::Dispatch to throw a SyncException)
using Dispatcher = std::function< bool (const wxArrayStringEx& fields) >;

//\brief Associates a dispatcher with a keyword in the default dictionary.
// The keyword will also be the first field passed to the dispatcher.  This
// struct is meant for static construction
struct WX_INIT_API RegisteredCommand {
    explicit RegisteredCommand(
        const wxString& name, Dispatcher dispatcher);
};

//\brief type of the dictionary of registered commands
using Dictionary = std::unordered_map< wxString, Journal::Dispatcher >;

//\brief read-only access to the dictionary
const Dictionary& GetDictionary();

//\brief Function performing additional initialization steps for journals
// Return value is success
using Initializer = std::function< bool () >;

//\brief Registers an initialization step in its constructor.
// Typically statically constructed
struct WX_INIT_API RegisteredInitializer {
    explicit RegisteredInitializer(Initializer initializer);
};

using Initializers = std::vector< Initializer >;

//\brief Get all registered initializers
WX_INIT_API
const Initializers& GetInitializers();
}

#endif
