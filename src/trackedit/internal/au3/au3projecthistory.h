/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "iprojecthistory.h"

#include <memory>
#include <vector>

#include "context/iglobalcontext.h"
#include "modularity/ioc.h"
#include "framework/interactive/iinteractive.h"

#include "au3wrap/au3types.h"

class SampleBlock;

namespace au::trackedit {
class Au3ProjectHistory : public IProjectHistory, public muse::Contextable
{
    muse::ContextInject<context::IGlobalContext> globalContext { this };
    muse::ContextInject<muse::IInteractive> interactive { this };

public:
    Au3ProjectHistory(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init() override;

    bool undoAvailable() const override;
    void undo() override;
    bool redoAvailable() const override;
    void redo() override;
    void pushHistoryState(
        const std::string& longDescription, const std::string& shortDescription) override;
    void pushHistoryState(
        const std::string& longDescription, const std::string& shortDescription, UndoPushType flags) override;
    void rollbackState() override;
    void modifyState(bool autoSave) override;
    void modifyState(const std::type_index& undoStateExtensionTypeIndex) override;
    void markUnsaved() override;

    void startUserInteraction() override;
    void endUserInteraction(bool modifyState) override;
    bool interactionOngoing() const override { return m_interactionOngoing; }

    void undoRedoToIndex(size_t index) override;

    const muse::TranslatableString topMostUndoActionName() const override;
    const muse::TranslatableString topMostRedoActionName() const override;
    size_t undoRedoActionCount() const override;
    size_t currentStateIndex() const override;
    const muse::TranslatableString lastActionNameAtIdx(size_t idx) const override;

    muse::async::Channel<HistoryEvent> historyChanged() const override;

private:
    au3::Au3Project& projectRef() const;

    void doUndo();
    void doRedo();

    //! Central guard for edit-locked sample blocks: returns whether the pending
    //! edit may be committed, asking the user if it removes locked blocks.
    bool confirmLockedBlocksChange() const;
    void rollbackRefusedEdit();
    void updateLockedBlocks();

    //! Locked blocks present in the last committed state
    std::vector<std::shared_ptr<SampleBlock> > m_lockedBlocks;

    muse::async::Channel<HistoryEvent> m_historyChanged;

    bool m_interactionOngoing = false;
};
}
