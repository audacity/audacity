/*
* Audacity: A Digital Audio Editor
*/

#include "au3projecthistory.h"

#include "libraries/lib-project-history/ProjectHistory.h"
#include "libraries/lib-project-history/UndoManager.h"
#include "libraries/lib-project/Project.h"

#include "au3wrap/internal/wxtypes_convert.h"

using namespace au::trackedit;
using namespace au::au3;

void au::trackedit::Au3ProjectHistory::init()
{
    auto& project = projectRef();
    ::ProjectHistory::Get(project).InitialState();
}

bool au::trackedit::Au3ProjectHistory::undoAvailable() const
{
    auto& project = projectRef();
    return ::ProjectHistory::Get(project).UndoAvailable();
}

void au::trackedit::Au3ProjectHistory::undo()
{
    doUndo();

    notifyAboutHistoryChanged();
}

bool au::trackedit::Au3ProjectHistory::redoAvailable() const
{
    auto& project = projectRef();
    return ::ProjectHistory::Get(project).RedoAvailable();
}

void au::trackedit::Au3ProjectHistory::redo()
{
    doRedo();

    notifyAboutHistoryChanged();
}

void au::trackedit::Au3ProjectHistory::undoUnsaved()
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);
    while (undoManager.UnsavedChanges())
    {
        undoManager.Undo(
            [&]( const UndoStackElem& elem ){
            ::ProjectHistory::Get(project).PopState(elem.state);
        });
    }

    notifyAboutHistoryChanged();
}

void au::trackedit::Au3ProjectHistory::clearUnsaved()
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);
    undoManager.ClearStates();

    notifyAboutHistoryChanged();
}

void au::trackedit::Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription)
{
    auto& project = projectRef();
    ::ProjectHistory::Get(project).PushState(TranslatableString { longDescription, {} }, TranslatableString { shortDescription, {} });

    notifyAboutHistoryChanged();
}

void Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription, UndoPushType flags)
{
    auto& project = projectRef();
    UndoPush undoFlags = static_cast<UndoPush>(flags);
    ::ProjectHistory::Get(project).PushState(TranslatableString { longDescription, {} }, TranslatableString { shortDescription, {} },
                                             undoFlags);

    notifyAboutHistoryChanged();
}

void Au3ProjectHistory::undoRedoToIndex(size_t index)
{
    if (currentStateIndex() == index) {
        return;
    }

    while (currentStateIndex() > index && undoAvailable()) {
        doUndo();
    }
    while (currentStateIndex() < index && redoAvailable()) {
        doRedo();
    }

    notifyAboutHistoryChanged();
}

const muse::TranslatableString Au3ProjectHistory::topMostUndoActionName() const
{
    if (!undoAvailable()) {
        return {};
    }

    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);

    int currentStateIndex = undoManager.GetCurrentState();

    ::TranslatableString actionName;
    undoManager.GetShortDescription(currentStateIndex, &actionName);

    return muse::TranslatableString::untranslatable(wxToString(actionName.Translation()));
}

const muse::TranslatableString Au3ProjectHistory::topMostRedoActionName() const
{
    if (!redoAvailable()) {
        return {};
    }

    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);

    int currentStateIndex = undoManager.GetCurrentState();

    ::TranslatableString actionName;
    undoManager.GetShortDescription(currentStateIndex + 1, &actionName);

    return muse::TranslatableString::untranslatable(wxToString(actionName.Translation()));
}

size_t Au3ProjectHistory::undoRedoActionCount() const
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);
    return undoManager.GetNumStates();
}

size_t Au3ProjectHistory::currentStateIndex() const
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);
    return undoManager.GetCurrentState();
}

const muse::TranslatableString Au3ProjectHistory::lastActionNameAtIdx(size_t idx) const
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);

    ::TranslatableString actionName;
    undoManager.GetShortDescription(idx, &actionName);

    return muse::TranslatableString::untranslatable(wxToString(actionName.Translation()));
}

muse::async::Notification Au3ProjectHistory::historyChanged() const
{
    return m_historyChanged;
}

Au3Project& au::trackedit::Au3ProjectHistory::projectRef() const
{
    return *reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
}

void Au3ProjectHistory::doUndo()
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);
    undoManager.Undo(
        [&]( const UndoStackElem& elem ){
        ::ProjectHistory::Get(project).PopState(elem.state);
    });
}

void Au3ProjectHistory::doRedo()
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);
    undoManager.Redo(
        [&]( const UndoStackElem& elem ){
        ::ProjectHistory::Get(project).PopState(elem.state);
    });
}

void Au3ProjectHistory::notifyAboutHistoryChanged()
{
    m_historyChanged.notify();
}
