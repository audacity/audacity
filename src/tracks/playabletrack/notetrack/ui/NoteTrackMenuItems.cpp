/**********************************************************************

Audacity: A Digital Audio Editor

@file NoteTrackMenuItems.cpp
@brief Injects menu items using NoteTrack but not the views of it

Paul Licameli split from TrackMenus.cpp

**********************************************************************/

/*!
 There is no "New Note Track" menu item corresponding to the other track types.  Note tracks are created
 when importing MIDI files.
 */

// Attach a menu item for diagnostic information

#include "AudioIOBase.h"
#include "../../../../commands/CommandContext.h"
#include "../../../../commands/CommandManager.h"
#include "CommonCommandFlags.h"
#include "HelpUtilities.h"
#include "NoteTrack.h"

#ifdef EXPERIMENTAL_MIDI_OUT
namespace {
struct Handler : CommandHandlerObject {
void OnMidiDeviceInfo(const CommandContext &context)
{
   auto &project = context.project;
   auto gAudioIO = AudioIOBase::Get();
   auto info = GetMIDIDeviceInfo();
   ShowDiagnostics( project, info,
      XO("MIDI Device Info"), wxT("midideviceinfo.txt") );
}
};

static CommandHandlerObject &findCommandHandler(AudacityProject &) {
   // Handler is not stateful.  Doesn't need a factory registered with
   // AudacityProject.
   static Handler instance;
   return instance;
};

using namespace MenuTable;
#define FN(X) (& Handler :: X)
AttachedItem sAttachment{
   { wxT("Help/Other/Diagnostics"),
      { OrderingHint::After, wxT("DeviceInfo") } },
   ( FinderScope{ findCommandHandler },
      Command( wxT("MidiDeviceInfo"), XXO("&MIDI Device Info..."),
         FN(OnMidiDeviceInfo),
         AudioIONotBusyFlag() )
   )
};
#undef FN
}
#endif
