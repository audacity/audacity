/*
* Audacity: A Digital Audio Editor
*/

#include "au3projecthistory.h"

#include "UndoManager.h"
#include "au3wrap/iau3project.h"
#include "libraries/lib-project-history/ProjectHistory.h"
#include "libraries/lib-project/Project.h"

using namespace au::trackedit;
using namespace au::au3;

void au::trackedit::Au3ProjectHistory::init()
{
    auto& project = projectRef();
    ::ProjectHistory::Get(project).InitialState();
}

bool au::trackedit::Au3ProjectHistory::undoAvailable()
{
    auto& project = projectRef();
    return ::ProjectHistory::Get(project).UndoAvailable();
}

void au::trackedit::Au3ProjectHistory::undo()
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);
    undoManager.Undo(
        [&]( const UndoStackElem& elem ){
        ::ProjectHistory::Get(project).PopState(elem.state);
    });

    m_interactionOngoing = false;
    m_isUndoRedoAvailableChanged.notify();
}

bool au::trackedit::Au3ProjectHistory::redoAvailable()
{
    auto& project = projectRef();
    return ::ProjectHistory::Get(project).RedoAvailable();
}

void au::trackedit::Au3ProjectHistory::redo()
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);
    undoManager.Redo(
        [&]( const UndoStackElem& elem ){
        ::ProjectHistory::Get(project).PopState(elem.state);
    });

    m_interactionOngoing = false;
    m_isUndoRedoAvailableChanged.notify();
}

void au::trackedit::Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription)
{
    pushHistoryState(longDescription, shortDescription, UndoPushType::NONE);
}

void Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription, UndoPushType flags)
{
    LOGI() << "pushHistoryState(\"" << shortDescription << "\", " << flags << ")";
    auto& project = projectRef();
    UndoPush undoFlags = static_cast<UndoPush>(flags);
    ::ProjectHistory::Get(project).PushState(TranslatableString { longDescription, {} }, TranslatableString { shortDescription, {} },
                                             undoFlags);

    m_interactionOngoing = false;
    m_isUndoRedoAvailableChanged.notify();
}

void Au3ProjectHistory::startUserInteraction()
{
    LOGI() << "startUserInteraction()";
    IF_ASSERT_FAILED(!m_interactionOngoing) {
        return;
    }
    m_interactionOngoing = true;
}

void Au3ProjectHistory::endUserInteraction()
{
    LOGI() << "endUserInteraction()";
    if (m_interactionOngoing) {
        m_interactionOngoing = false;
        // No new history entry was pushed -> update the state.
        modifyState(false);
    }
}

void Au3ProjectHistory::modifyState(bool autoSave)
{
    LOGI() << "modifyState(" << (autoSave ? "true" : "false") << ")";
    if (m_interactionOngoing) {
        LOGW() << "Attempt to modify state during undoable action";
        return;
    }
    auto& project = projectRef();
    ::ProjectHistory::Get(project).ModifyState(autoSave);
}

void Au3ProjectHistory::markUnsaved()
{
    LOGI() << "markUnsaved()";
    auto& project = projectRef();
    ::UndoManager::Get(project).MarkUnsaved();
}

muse::async::Notification Au3ProjectHistory::isUndoRedoAvailableChanged() const
{
    return m_isUndoRedoAvailableChanged;
}

Au3Project& au::trackedit::Au3ProjectHistory::projectRef()
{
    return *reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
}
