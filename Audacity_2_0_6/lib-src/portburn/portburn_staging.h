/*
 * PortBurn
 * Common utilities for staging audio files in a temporary directory.
 *
 * Dominic Mazzoni
 * License: LGPL
 *
 * A library for cross-platform audio CD burning
 *
 * On some of the platforms supported by PortBurn, we need to stage
 * the audio data ourselves.  This file defines an interface to the
 * common functions.
 */

#ifndef __PORTBURN_STAGING__
#define __PORTBURN_STAGING__

void *PortBurn_TempDirStaging(const char *temporary_directory);

int PortBurn_StartStagingTrack(void *handle, const char *name, int raw);
int PortBurn_AddStagingFrame(void *handle, short *buffer);
int PortBurn_EndStagingTrack(void *handle);

int PortBurn_FinishStaging(void *handle);

int PortBurn_GetNumStagedTracks(void *handle);
const char *PortBurn_GetStagedFilename(void *handle, int index);
const char *PortBurn_GetStagedTrackNname(void *handle, int index);
int PortBurn_GetStagedLengthInFrames(void *handle, int index);

void PortBurn_CleanupStaging(void *handle);

#endif /* __PORTBURN_STAGING__ */
