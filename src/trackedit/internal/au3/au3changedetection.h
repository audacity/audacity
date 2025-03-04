/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::trackedit {
/**
 * Au3's Undo/Redo depends on the entire state of the model having been "cached" in the project with the
 * "autosave" table entry.
 * On Undo/Redo, the entire "autosaved" model is loaded, replacing the current one.
 * This class takes two structures, one created before and one after Undo/Redo.
 * It compares these, and calls the corresponding notifications in AU4,
 * to update as small a GUI section as needed, for reflecting the changes before and after.
 */
class Au3ChangeDetection
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3ChangeDetection() = default;

    /**
     * Takes two structures, one created before and one after Undo/Redo.
     * It compares these, and calls the corresponding notifications in Audacity,
     * to update as small a GUI section as needed, for reflecting the changes before and after.
     * @param before The TracksAndClips structure before the change
     * @param after The TracksAndClips structure after the change
     */
    void notifyOfUndoRedo(const TracksAndClips& before, const TracksAndClips& after);

private:
    /**
     * Searches for, and notifies of, Tracks changes
     * @param before a vector of Tracks before the change
     * @param after a vector of Tracks after the change
     * @return true if a change has been detected and notified.
     */
    bool _forTracks(const TrackList& before, const TrackList& after);

    /**
     * Searches for, and notifies of, clip changes.
     * Note that the two lists passed need to have equal length.
     * @param before a vector of Clips lists before the change
     * @param after a vector of Clips lists after the change
     */
    void _forClips(const std::vector<Clips>& before, const std::vector<Clips>& after);
};
}
