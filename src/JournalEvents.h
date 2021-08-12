/*!********************************************************************

  Audacity: A Digital Audio Editor

  @name JournalEvents.h

  @brief utilities to record events to journal and recreate them on playback

  Paul Licameli

**********************************************************************/

#include <vector>

namespace Journal
{
namespace Events
{
   //\brief Initialization, to be called after wxWidgets has initialized
   void Initialize();

   //! Install the global event filter for recording
   void Watch();
}
}
