/*
* Audacity: A Digital Audio Editor
*/

#include "au3projecthistory.h"

#include "UndoManager.h"
#include "au3wrap/iau3project.h"
#include "libraries/lib-project-history/ProjectHistory.h"
#include "libraries/lib-project/Project.h"

using namespace au::trackedit;

void au::trackedit::Au3ProjectHistory::init()
{
    auto project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    ::ProjectHistory::Get(*project).InitialState();
}

bool au::trackedit::Au3ProjectHistory::undoAvailable()
{
    auto project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return ::ProjectHistory::Get(*project).UndoAvailable();
}

void au::trackedit::Au3ProjectHistory::undo()
{
    auto project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    auto& undoManager = UndoManager::Get(*project);
    undoManager.Undo(
        [&]( const UndoStackElem& elem ){
            ::ProjectHistory::Get(*project).PopState(elem.state);
        });
}

bool au::trackedit::Au3ProjectHistory::redoAvailable()
{
    auto project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return ::ProjectHistory::Get(*project).RedoAvailable();
}

void au::trackedit::Au3ProjectHistory::redo()
{
    auto project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    auto& undoManager = UndoManager::Get(*project);
    undoManager.Redo(
        [&]( const UndoStackElem& elem ){
            ::ProjectHistory::Get(*project).PopState(elem.state);
        });
}

void au::trackedit::Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription)
{
    auto project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    ::ProjectHistory::Get(*project).PushState(TranslatableString { longDescription, {} }, TranslatableString { shortDescription, {} });
}
