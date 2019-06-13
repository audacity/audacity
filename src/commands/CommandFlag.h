//
//  CommandFlag.h
//  Audacity
//
//  Created by Paul Licameli on 11/22/16.
//
//

#ifndef __AUDACITY_COMMAND_FLAG__
#define __AUDACITY_COMMAND_FLAG__

// Flags used in command handling.

#include <bitset>

// Increase the template parameter as needed to allow more flags
constexpr size_t NCommandFlags = 64;
static_assert(
   NCommandFlags <= 8 * sizeof( unsigned long long ),
   "NoFlagsSpecified may have incorrect value"
);

// Type to specify conditions for enabling of a menu item
using CommandFlag = std::bitset<NCommandFlags>;
using CommandMask = CommandFlag;

// Special constant values
constexpr CommandFlag
   AlwaysEnabledFlag{},      // all zeroes
   NoFlagsSpecified{ ~0ULL }; // all ones

// Construct one statically to register (and reserve) a bit position in the set
class ReservedCommandFlag : public CommandFlag
{
public:
   ReservedCommandFlag();
};

// Widely used command flags, but this list need not be exhaustive.  It may be
// extended, with special purpose flags of limited use, by constucting static
// ReservedCommandFlag values

extern const ReservedCommandFlag
   AudioIONotBusyFlag,
   TimeSelectedFlag, // This is equivalent to check if there is a valid selection, so it's used for Zoom to Selection too
   TracksSelectedFlag,
   TracksExistFlag,
   LabelTracksExistFlag,
   WaveTracksSelectedFlag,
   UnsavedChangesFlag,
   HasLastEffectFlag,
   UndoAvailableFlag,
   RedoAvailableFlag,
   ZoomInAvailableFlag,
   ZoomOutAvailableFlag,
   StereoRequiredFlag,  //lda
   TrackPanelHasFocus,  //lll
   LabelsSelectedFlag,
   AudioIOBusyFlag,  //lll
   PlayRegionLockedFlag,  //msmeyer
   PlayRegionNotLockedFlag,  //msmeyer
   CutCopyAvailableFlag,
   WaveTracksExistFlag,
   NoteTracksExistFlag,  //gsw
   NoteTracksSelectedFlag,  //gsw
   IsNotSyncLockedFlag,  //awd
   IsSyncLockedFlag,  //awd
   IsRealtimeNotActiveFlag,  //lll
   CaptureNotBusyFlag,
   CanStopAudioStreamFlag,
   NotMinimizedFlag, // prl
   PausedFlag, // jkc
   HasWaveDataFlag, // jkc
   PlayableTracksExistFlag,
   AudioTracksSelectedFlag,
   NoAutoSelect // jkc
;

#endif
