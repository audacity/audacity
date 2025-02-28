/*!********************************************************************

  Audacity: A Digital Audio Editor

  @name JournalEvents.h

  @brief utilities to record events to journal and recreate them on playback

  Paul Licameli

**********************************************************************/

#include <vector>

namespace Journal {
namespace Events {
//\brief Whether events are being recorded to the journal
bool IsWatching();

//\brief Stop watching events, and give the user an error message
void FailedEventSerialization();
}
}
