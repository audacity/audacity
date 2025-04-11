/*
 * Audacity: A Digital Audio Editor
 */
#include "undomanager.h"
#include "changedetection.h"

namespace au::trackedit {
bool UndoManager::undo()
{
    if (!canUndo()) {
        return false;
    }

    auto trackeditProject = globalContext()->currentProject()->trackeditProject();

    const TracksAndClips before = trackeditProject->buildTracksAndClips();

    projectHistory()->undo();

    const TracksAndClips after = trackeditProject->buildTracksAndClips();

    changeDetection::notifyOfUndoRedo(before, after, trackeditProject);

    return true;
}

bool UndoManager::canUndo()
{
    return projectHistory()->undoAvailable();
}

bool UndoManager::redo()
{
    if (!canRedo()) {
        return false;
    }

    auto trackeditProject = globalContext()->currentProject()->trackeditProject();

    TracksAndClips before = trackeditProject->buildTracksAndClips();

    projectHistory()->redo();

    TracksAndClips after = trackeditProject->buildTracksAndClips();

    changeDetection::notifyOfUndoRedo(before, after, trackeditProject);

    return true;
}

bool UndoManager::canRedo()
{
    return projectHistory()->redoAvailable();
}

bool UndoManager::undoRedoToIndex(size_t index)
{
    if (projectHistory()->currentStateIndex() == index) {
        return false;
    }

    auto trackeditProject = globalContext()->currentProject()->trackeditProject();

    const TracksAndClips before = trackeditProject->buildTracksAndClips();

    projectHistory()->undoRedoToIndex(index);

    const TracksAndClips after = trackeditProject->buildTracksAndClips();

    changeDetection::notifyOfUndoRedo(before, after, trackeditProject);

    return true;
}
}
