/**********************************************************************

Audacity: A Digital Audio Editor

CommonCommandFlags.h

Paul Licameli split from Menus.cpp

**********************************************************************/

#ifndef __AUDACITY_COMMON_COMMAND_FLAGS__
#define __AUDACITY_COMMON_COMMAND_FLAGS__

#include "CommandFlag.h"

AUDACITY_DLL_API
bool EditableTracksSelectedPred(const AudacityProject& project);

AUDACITY_DLL_API
bool AudioIOBusyPred(const AudacityProject& project);

AUDACITY_DLL_API
bool TimeSelectedPred(const AudacityProject& project);

AUDACITY_DLL_API
const CommandFlagOptions& cutCopyOptions();

extern AUDACITY_DLL_API const ReservedCommandFlag
& AudioIONotBusyFlag(),
&StereoRequiredFlag(),     //lda
&NoiseReductionTimeSelectedFlag(),
&TimeSelectedFlag(),    // This is equivalent to check if there is a valid selection, so it's used for Zoom to Selection too
&WaveTracksSelectedFlag(),
&TracksExistFlag(),
&EditableTracksSelectedFlag(),
&AnyTracksSelectedFlag(),
&TrackPanelHasFocus();     //lll

extern AUDACITY_DLL_API const ReservedCommandFlag
& AudioIOBusyFlag(),   // lll
&CaptureNotBusyFlag();

extern AUDACITY_DLL_API const ReservedCommandFlag
& LabelTracksExistFlag(),
&UndoAvailableFlag(),
&RedoAvailableFlag(),
&ZoomInAvailableFlag(),
&ZoomOutAvailableFlag(),
&WaveTracksExistFlag(),
&IsNotSyncLockedFlag(),     //awd
&IsSyncLockedFlag(),     //awd
&NotMinimizedFlag(),    // prl
&PausedFlag(),    // jkc
&NoAutoSelect()    // jkc
;

#endif
