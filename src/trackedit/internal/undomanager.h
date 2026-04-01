/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iundomanager.h"

#include "iprojecthistory.h"

#include "context/iglobalcontext.h"
#include "modularity/ioc.h"

namespace au::trackedit {
class UndoManager : public IUndoManager, public muse::Contextable
{
private:
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<au::trackedit::IProjectHistory> projectHistory { this };

public:
    UndoManager(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    bool undo() override;
    bool canUndo() override;
    bool redo() override;
    bool canRedo() override;
    bool undoRedoToIndex(size_t index) override;
};
}
