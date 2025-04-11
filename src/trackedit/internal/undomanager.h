/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iundomanager.h"

#include "itrackandclipoperations.h"
#include "iprojecthistory.h"

#include "context/iglobalcontext.h"
#include "modularity/ioc.h"

namespace au::trackedit {
class UndoManager : public IUndoManager, public muse::Injectable
{
private:
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;

public:
    bool undo() override;
    bool canUndo() override;
    bool redo() override;
    bool canRedo() override;
    bool undoRedoToIndex(size_t index) override;
};
}
