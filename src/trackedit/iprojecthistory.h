/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"

#include "modularity/imoduleinterface.h"

#include "global/types/translatablestring.h"
#include "trackedittypes.h"

namespace au::trackedit {
class IProjectHistory : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectHistory)

public:
    virtual ~IProjectHistory() = default;

    virtual void init() = 0;

    virtual bool undoAvailable() const = 0;
    virtual void undo() = 0;
    virtual bool redoAvailable() const = 0;
    virtual void redo() = 0;
    virtual void pushHistoryState(const std::string& longDescription, const std::string& shortDescription) = 0;
    virtual void pushHistoryState(const std::string& longDescription, const std::string& shortDescription,
                                  trackedit::UndoPushType flags) = 0;

    virtual void undoRedoToIndex(size_t index) = 0;

    virtual const muse::TranslatableString topMostUndoActionName() const = 0;
    virtual const muse::TranslatableString topMostRedoActionName() const = 0;
    virtual size_t undoRedoActionCount() const = 0;
    virtual size_t currentStateIndex() const = 0;
    virtual const muse::TranslatableString lastActionNameAtIdx(size_t idx) const = 0;

    virtual muse::async::Notification historyChanged() const = 0;

    virtual void rollbackState() = 0;
    virtual void modifyState(bool autoSave = false) = 0;
    virtual void markUnsaved() = 0;

    /**
     * @brief Start and end user interaction.
     *
     * @details Use these methods when you know the user starts an interaction that will either
     * - add a new history entry, or
     * - modify the current state,
     * but you don't know which one it will be.
     *
     * Calling `startUserInteraction()` will protect against inadvertent state modifications in the middle of an interaction,
     * which can introduce bugs in subsequent usage of undo.
     * If, by the time `endUserInteraction()` is called, no new history entry was pushed, the current state will be modified automatically.
     */
    virtual void startUserInteraction() = 0;

    /**
     * @ref startUserInteraction()
     */
    virtual void endUserInteraction() = 0;
};

using IProjectHistoryPtr = std::shared_ptr<IProjectHistory>;
}
