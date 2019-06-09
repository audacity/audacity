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
#include <functional>
#include <utility>

class AudacityProject;

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

struct CommandFlagOptions{
   CommandFlagOptions() = default;
   CommandFlagOptions && QuickTest() &&
   { quickTest = true; return std::move( *this ); }

   // If true, assume this is a cheap test to be done always.  If false, the
   // test may be skipped and the condition assumed to be unchanged since the
   // last more comprehensive testing
   bool quickTest = false;
};

// Construct one statically to register (and reserve) a bit position in the set
// an associate it with a test function; those with quickTest = true are cheap
// to compute and always checked
class ReservedCommandFlag : public CommandFlag
{
public:
   using Predicate = std::function< bool( const AudacityProject& ) >;
   ReservedCommandFlag( const Predicate &predicate,
      const CommandFlagOptions &options = {} );
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
