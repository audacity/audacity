/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::trackedit {
class IUndoManager
{
public:
    virtual ~IUndoManager() = default;

    virtual bool undo() = 0;
    virtual bool canUndo() = 0;
    virtual bool redo() = 0;
    virtual bool canRedo() = 0;
    virtual bool undoRedoToIndex(size_t index) = 0;
};
}
