/*
* Audacity: A Digital Audio Editor
*/

#include "au3projecthistory.h"

#include "au3-project-history/ProjectHistory.h"
#include "au3-project-history/UndoManager.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-project/Project.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/Sequence.h"
#include "au3-wave-track/SampleBlock.h"

#include "framework/global/translation.h"

#include <unordered_set>

#include <QDateTime>
#include <QDir>
#include <QFile>

using namespace au::trackedit;
using namespace au::au3;

namespace {
std::vector<std::shared_ptr<SampleBlock> > lockedBlocksInTracks(Au3Project& project)
{
    std::vector<std::shared_ptr<SampleBlock> > result;
    for (const WaveTrack* track : ::TrackList::Get(project).Any<const WaveTrack>()) {
        for (const auto& clip : track->Intervals()) {
            for (size_t ch = 0; ch < clip->NChannels(); ++ch) {
                for (const auto& block : clip->GetSequence(ch)->GetBlockArray()) {
                    if (block.sb->IsEditLocked()) {
                        result.push_back(block.sb);
                    }
                }
            }
        }
    }
    return result;
}
}

void au::trackedit::Au3ProjectHistory::init()
{
    auto& project = projectRef();
    ::ProjectHistory::Get(project).InitialState();
    updateLockedBlocks();

    // Each (re)enabling starts a new dump folder, beginning with the current state
    m_xmlDumpFolder.clear();
    if (configuration()) {
        configuration()->historyXmlDumpEnabledChanged().onNotify(this, [this]() {
            m_xmlDumpFolder.clear();
            dumpXmlIfEnabled();
        }, muse::async::Asyncable::Mode::SetReplace);
    }
    dumpXmlIfEnabled();

    m_historyChanged.send(HistoryEvent::RestoredState);
}

bool au::trackedit::Au3ProjectHistory::undoAvailable() const
{
    if (!globalContext()->currentProject()) {
        return false;
    }

    auto& project = projectRef();
    return ::ProjectHistory::Get(project).UndoAvailable();
}

void au::trackedit::Au3ProjectHistory::undo()
{
    doUndo();
    // Undoing past a lock removes locked blocks too
    if (!confirmLockedBlocksChange()) {
        doRedo();
    }
    updateLockedBlocks();

    m_interactionOngoing = false;
    m_historyChanged.send(HistoryEvent::RestoredState);
}

bool au::trackedit::Au3ProjectHistory::redoAvailable() const
{
    if (!globalContext()->currentProject()) {
        return false;
    }

    auto& project = projectRef();
    return ::ProjectHistory::Get(project).RedoAvailable();
}

void au::trackedit::Au3ProjectHistory::redo()
{
    doRedo();
    updateLockedBlocks();

    m_interactionOngoing = false;
    m_historyChanged.send(HistoryEvent::RestoredState);
}

void au::trackedit::Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription)
{
    pushHistoryState(longDescription, shortDescription, UndoPushType::NONE);
}

void Au3ProjectHistory::pushHistoryState(const std::string& longDescription, const std::string& shortDescription, UndoPushType flags)
{
    LOGI() << "pushHistoryState(\"" << shortDescription << "\", " << flags << ")";
    auto& project = projectRef();
    if (!confirmLockedBlocksChange()) {
        rollbackRefusedEdit();
        return;
    }
    UndoPush undoFlags = static_cast<UndoPush>(flags);
    ::ProjectHistory::Get(project).PushState(::TranslatableString::untranslatable(QString::fromStdString(longDescription)),
                                             ::TranslatableString::untranslatable(QString::fromStdString(shortDescription)),
                                             undoFlags);
    updateLockedBlocks();
    dumpXmlIfEnabled();

    m_interactionOngoing = false;
    m_historyChanged.send(HistoryEvent::NewState);
}

void au::trackedit::Au3ProjectHistory::rollbackState()
{
    auto& project = projectRef();
    ::ProjectHistory::Get(project).RollbackState();
    updateLockedBlocks();
    m_interactionOngoing = false;
    m_historyChanged.send(HistoryEvent::RestoredState);
}

void Au3ProjectHistory::startUserInteraction()
{
    LOGI() << "startUserInteraction()";
    IF_ASSERT_FAILED(!m_interactionOngoing) {
        return;
    }
    // Modify the state, so that if the interaction gets canceled,
    // rollbackState would revert to the state at the beginning of the action.
    modifyState(false);
    m_interactionOngoing = true;
}

void Au3ProjectHistory::endUserInteraction(bool modifyState)
{
    LOGI() << "endUserInteraction()";
    if (m_interactionOngoing) {
        m_interactionOngoing = false;
        if (modifyState) {
            // No new history entry was pushed -> update the state.
            this->modifyState(true);
        }
    }
}

void Au3ProjectHistory::modifyState(bool autoSave)
{
    LOGD() << "modifyState(" << (autoSave ? "true" : "false") << ")";
    if (m_interactionOngoing) {
        LOGW() << "Attempt to modify state during undoable action";
        return;
    }
    if (!confirmLockedBlocksChange()) {
        rollbackRefusedEdit();
        return;
    }
    auto& project = projectRef();
    ::ProjectHistory::Get(project).ModifyState(autoSave);
    updateLockedBlocks();
    dumpXmlIfEnabled();
}

void Au3ProjectHistory::modifyState(const std::type_index& restorerType)
{
    if (m_interactionOngoing) {
        return;
    }
    auto& project = projectRef();
    ::ProjectHistory::Get(project).ModifyState(restorerType);
}

void Au3ProjectHistory::markUnsaved()
{
    LOGD() << "markUnsaved()";
    auto& project = projectRef();
    ::UndoManager::Get(project).MarkUnsaved();
}

void Au3ProjectHistory::undoRedoToIndex(size_t index)
{
    if (currentStateIndex() == index) {
        return;
    }

    const auto goTo = [this](size_t target) {
        while (currentStateIndex() > target && undoAvailable()) {
            doUndo();
        }
        while (currentStateIndex() < target && redoAvailable()) {
            doRedo();
        }
    };

    const size_t startIndex = currentStateIndex();
    goTo(index);
    if (!confirmLockedBlocksChange()) {
        goTo(startIndex);
    }
    updateLockedBlocks();

    m_historyChanged.send(HistoryEvent::RestoredState);
}

const muse::TranslatableString Au3ProjectHistory::topMostUndoActionName() const
{
    if (!undoAvailable()) {
        return {};
    }

    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);

    int currentStateIndex = undoManager.GetCurrentState();

    TranslatableString actionName;
    undoManager.GetShortDescription(currentStateIndex, &actionName);

    return muse::TranslatableString::untranslatable(muse::String::fromQString(actionName.translated()));
}

const muse::TranslatableString Au3ProjectHistory::topMostRedoActionName() const
{
    if (!redoAvailable()) {
        return {};
    }

    auto& project = projectRef();
    auto& undoManager = UndoManager::Get(project);

    int currentStateIndex = undoManager.GetCurrentState();

    TranslatableString actionName;
    undoManager.GetShortDescription(currentStateIndex + 1, &actionName);

    return muse::TranslatableString::untranslatable(muse::String::fromQString(actionName.translated()));
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

    TranslatableString actionName;
    undoManager.GetShortDescription(idx, &actionName);

    return muse::TranslatableString::untranslatable(muse::String::fromQString(actionName.translated()));
}

muse::async::Channel<HistoryEvent> Au3ProjectHistory::historyChanged() const
{
    return m_historyChanged;
}

Au3Project& au::trackedit::Au3ProjectHistory::projectRef() const
{
    return *reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
}

bool Au3ProjectHistory::confirmLockedBlocksChange() const
{
    if (m_lockedBlocks.empty()) {
        return true;
    }

    std::unordered_set<const SampleBlock*> current;
    for (const auto& block : lockedBlocksInTracks(projectRef())) {
        current.insert(block.get());
    }

    // Blocks unlocked in the meantime (e.g. "Unlock all blocks") don't count
    const bool lockedBlockRemoved = std::any_of(m_lockedBlocks.begin(), m_lockedBlocks.end(), [&](const auto& block) {
        return block->IsEditLocked() && !current.count(block.get());
    });
    if (!lockedBlockRemoved) {
        return true;
    }

    const muse::IInteractive::Result result = interactive()->warningSync(
        muse::trc("trackedit", "This audio is locked"),
        muse::trc("trackedit", "This edit changes audio that is locked, for example by an effect being processed.\n\n"
                               "Proceeding would abort that processing."),
        { muse::IInteractive::Button::Cancel, muse::IInteractive::Button::Ok },
        muse::IInteractive::Button::Cancel);

    return result.standardButton() == muse::IInteractive::Button::Ok;
}

void Au3ProjectHistory::rollbackRefusedEdit()
{
    rollbackState();
    // The edit may already have been reported to the UI (e.g. a clip split),
    // and rollbackState doesn't notify: resync everything
    if (const auto prj = globalContext()->currentTrackeditProject()) {
        prj->reload();
    }
}

void Au3ProjectHistory::dumpXmlIfEnabled()
{
    if (!configuration() || !configuration()->historyXmlDumpEnabled() || !globalContext()->currentProject()) {
        return;
    }

    auto& project = projectRef();
    if (m_xmlDumpFolder.empty()) {
        const QString projectName = QString::fromStdString(project.GetProjectName().ToStdString());
        const QString folder = globalConfiguration()->userAppDataPath().toQString() + "/history-xml/"
                               + (projectName.isEmpty() ? QString("untitled") : projectName) + " "
                               + QDateTime::currentDateTime().toString("yyyy-MM-dd hh-mm-ss");
        QDir().mkpath(folder);
        m_xmlDumpFolder = folder.toStdString();
        LOGI() << "Dumping project XML per history step to: " << m_xmlDumpFolder;
    }

    // One file per step; a commit that modifies the current step (e.g. end of a
    // drag) overwrites it. After an undo, the next step gets a new name.
    auto& undoManager = UndoManager::Get(project);
    const int index = undoManager.GetCurrentState();
    ::TranslatableString name;
    undoManager.GetShortDescription(index, &name);
    QString fileName = QString("%1 %2.xml").arg(index, 3, 10, QChar('0')).arg(name.Translation().ToStdString().c_str());
    fileName.replace('/', '-');

    QFile file(QString::fromStdString(m_xmlDumpFolder) + "/" + fileName);
    if (file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        file.write(ProjectFileIO::Get(project).GenerateDoc().ToUTF8().data());
    } else {
        LOGE() << "Could not write " << file.fileName();
    }
}

void Au3ProjectHistory::updateLockedBlocks()
{
    if (!globalContext()->currentProject()) {
        m_lockedBlocks.clear();
        return;
    }
    m_lockedBlocks = lockedBlocksInTracks(projectRef());
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
