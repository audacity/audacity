/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::trackedit {

/**
 * Au3's undo/redo depends on the entire state of the model having been "cached" in the project with the
 * "autosave" table entry.
 * On Undo/Redo, the entire "autosaved" model is loaded, replacing the current one.
 * This class takes two structures, one created before and one after Undo/Redo.
 * It compares these, and calls the corresponding notifications in AU4,
 * to update as small a GUI section as needed, for reflecting the changes before and after.
 */
class Au3ChangeDetection {
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3ChangeDetection() = default;

    void notifyOfUndoRedo(const TracksAndClips& before, const TracksAndClips& after);
};
}
