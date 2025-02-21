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
    virtual void undoUnsaved() = 0;
    virtual void clearUnsaved() = 0;
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
};

using IProjectHistoryPtr = std::shared_ptr<IProjectHistory>;
}
