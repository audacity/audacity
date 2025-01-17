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

    m_isUndoRedoAvailableChanged.notify();
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
}

void au::trackedit::Au3ProjectHistory::clearUnsaved()
{
    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);
    undoManager.ClearStates();
}

void au::trackedit::Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription)
{
    auto& project = projectRef();
    ::ProjectHistory::Get(project).PushState(TranslatableString { longDescription, {} }, TranslatableString { shortDescription, {} });

    m_isUndoRedoAvailableChanged.notify();
}

void Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription, UndoPushType flags)
{
    auto& project = projectRef();
    UndoPush undoFlags = static_cast<UndoPush>(flags);
    ::ProjectHistory::Get(project).PushState(TranslatableString { longDescription, {} }, TranslatableString { shortDescription, {} },
                                             undoFlags);

    m_isUndoRedoAvailableChanged.notify();
}

muse::async::Notification Au3ProjectHistory::isUndoRedoAvailableChanged() const
{
    return m_isUndoRedoAvailableChanged;
}

Au3Project& au::trackedit::Au3ProjectHistory::projectRef()
{
    return *reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
}
