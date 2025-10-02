/*
* Audacity: A Digital Audio Editor
*/

#include "au3projecthistory.h"

#include "libraries/lib-project-history/ProjectHistory.h"
#include "libraries/lib-project-history/UndoManager.h"
#include "libraries/lib-project/Project.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "global/async/async.h"

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

    m_interactionOngoing = false;
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

    m_interactionOngoing = false;
    notifyAboutHistoryChanged();
}

void au::trackedit::Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription)
{
    pushHistoryState(longDescription, shortDescription, UndoPushType::NONE);
}

void Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription, UndoPushType flags)
{
    //! Why does this `doPushHistoryState` have to be called later?
    //!
    //! On the one hand, `pushHistoryState` may be called as a result of a QML item triggering an action that will lead to its deletion
    //! (e.g. deleting a track from a context menu).
    //!
    //! On the other hand, when the QML-triggered action is called after an undo, this undo history item gets discarded.
    //! If this undo item involved the creation of audio blocks, such as when generating audio, `pushHistoryState` will delete these blocks.
    //! If there was many of them, it will open a progress dialog which, to be updated, leads to `QCoreApplication::processEvents()` calls.
    //!
    //! This crashes because QCoreApplication::processEvents() will now process an event that deletes the QML item that is at the origin of this very call...
    //! Qt then throws the message "Object 0x..... destroyed while one of its QML signal handlers is in progress."
    //!
    //! https://github.com/audacity/audacity/issues/9530
    muse::async::Async::call(this, [=]{ doPushHistoryState(longDescription, shortDescription, flags); });
}

void Au3ProjectHistory::doPushHistoryState(const std::string& longDescription, const std::string& shortDescription, UndoPushType flags)
{
    LOGI() << "pushHistoryState(\"" << shortDescription << "\", " << flags << ")";
    auto& project = projectRef();
    UndoPush undoFlags = static_cast<UndoPush>(flags);
    ::ProjectHistory::Get(project).PushState(TranslatableString { longDescription, {} }, TranslatableString { shortDescription, {} },
                                             undoFlags);

    m_interactionOngoing = false;
    notifyAboutHistoryChanged();
}

void au::trackedit::Au3ProjectHistory::rollbackState()
{
    auto& project = projectRef();
    ::ProjectHistory::Get(project).RollbackState();
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
