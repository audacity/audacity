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
#include "CommandContext.h"
#include "MenuRegistry.h"
#include "CommonCommandFlags.h"
#include "HelpUtilities.h"
#include "NoteTrack.h"

namespace {
void OnMidiDeviceInfo(const CommandContext& context)
{
    auto& project = context.project;
    auto gAudioIO = AudioIOBase::Get();
    auto info = GetMIDIDeviceInfo();
    ShowDiagnostics(project, info,
                    XO("MIDI Device Info"), wxT("midideviceinfo.txt"));
}

using namespace MenuRegistry;
AttachedItem sAttachment{
    Command(wxT("MidiDeviceInfo"), XXO("&MIDI Device Info..."),
            OnMidiDeviceInfo, AudioIONotBusyFlag()),
    { wxT("Help/Other/Diagnostics"),
      { OrderingHint::After, wxT("DeviceInfo") } }
};
}
