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

    const TracksAndItems before = trackeditProject->buildTracksAndItems();

    projectHistory()->undo();

    const TracksAndItems after = trackeditProject->buildTracksAndItems();

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

    TracksAndItems before = trackeditProject->buildTracksAndItems();

    projectHistory()->redo();

    TracksAndItems after = trackeditProject->buildTracksAndItems();

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

    const TracksAndItems before = trackeditProject->buildTracksAndItems();

    projectHistory()->undoRedoToIndex(index);

    const TracksAndItems after = trackeditProject->buildTracksAndItems();

    changeDetection::notifyOfUndoRedo(before, after, trackeditProject);

    return true;
}
}
