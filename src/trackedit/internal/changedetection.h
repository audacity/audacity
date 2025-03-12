/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "trackedit/itrackeditproject.h"

/**
 * Au3's Undo/Redo depends on the entire state of the model having been "cached".
 * On Undo/Redo, the model is loaded, replacing the current one.
 * This class takes two structures, one created before and one after Undo/Redo.
 * It compares these, and calls the corresponding notifications in AU4,
 * to update as small a GUI section as needed, for reflecting the changes before and after.
 */
namespace au::trackedit::changeDetection {
/**
 * Takes two structures, one created before and one after Undo/Redo.
 * It compares these, and calls the corresponding notifications in Audacity,
 * to update as small a GUI section as needed, for reflecting the changes before and after.
 * @param before The TracksAndClips structure before the change
 * @param after The TracksAndClips structure after the change
 * @param trackeditProject a TrackEditProjectPtr expected to be valid
 */
void notifyOfUndoRedo(const TracksAndClips& before, const TracksAndClips& after, ITrackeditProjectPtr trackeditProject);
}
