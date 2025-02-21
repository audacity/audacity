/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "iprojecthistory.h"

#include "context/iglobalcontext.h"
#include "modularity/ioc.h"

#include "au3wrap/au3types.h"

namespace au::trackedit {
class Au3ProjectHistory : public IProjectHistory
{
    muse::Inject<context::IGlobalContext> globalContext;

public:
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
    void markUnsaved() override;
    void startUserInteraction() override;
    void endUserInteraction() override;

    void undoRedoToIndex(size_t index) override;

    const muse::TranslatableString topMostUndoActionName() const override;
    const muse::TranslatableString topMostRedoActionName() const override;
    size_t undoRedoActionCount() const override;
    size_t currentStateIndex() const override;
    const muse::TranslatableString lastActionNameAtIdx(size_t idx) const override;

    muse::async::Notification historyChanged() const override;

private:
    au3::Au3Project& projectRef() const;

    void doUndo();
    void doRedo();

    void notifyAboutHistoryChanged();

    muse::async::Notification m_historyChanged;

    bool m_interactionOngoing = false;
};
}
