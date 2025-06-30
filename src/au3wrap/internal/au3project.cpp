/*
* Audacity: A Digital Audio Editor
*/
#include "au3project.h"

#include "global/defer.h"

#include "libraries/lib-project-history/ProjectHistory.h"
#include "libraries/lib-realtime-effects/SavedMasterEffectList.h"
#include "libraries/lib-file-formats/AcidizerTags.h"
#include "libraries/lib-import-export/Import.h"
#include "libraries/lib-import-export/ImportPlugin.h"
#include "libraries/lib-import-export/ImportProgressListener.h"
#include "libraries/lib-numeric-formats/ProjectTimeSignature.h"
#include "libraries/lib-project-file-io/ProjectFileIO.h"
#include "libraries/lib-project/Project.h"
#include "libraries/lib-tags/Tags.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "TempoChange.h"

//! HACK
//! Static variable is not initialized
//! static SampleBlockFactory::Factory::Scope scope{ []( AudacityProject &project )
//! so, to fix it, included this file here
#include "libraries/lib-project-file-io/SqliteSampleBlock.cpp"

#include "wxtypes_convert.h"
#include "../au3types.h"
#include "domconverter.h"
#include "trackcolor.h"

#include "log.h"

#include <random>

#include "project/projecterrors.h"

using namespace au::au3;

namespace {
size_t randomIndex()
{
    std::random_device rd;
    std::mt19937 generator(rd());
    std::uniform_int_distribution<size_t> distribution(0, 20);
    return distribution(generator);
}
}

std::shared_ptr<IAu3Project> Au3ProjectCreator::create() const
{
    TrackColor::Init(randomIndex());
    return std::make_shared<Au3ProjectAccessor>();
}

struct au::au3::Au3ProjectData
{
    std::shared_ptr<Au3Project> project;

    Au3Project& projectRef() { return *project.get(); }
};

Au3ProjectAccessor::Au3ProjectAccessor()
    : m_data(std::make_shared<Au3ProjectData>())
{
    m_data->project = Au3Project::Create();
    mTrackListSubstription = Au3TrackList::Get(m_data->projectRef()).Subscribe([this](const TrackListEvent& event)
    {
        if (event.mType == TrackListEvent::ADDITION) {
            const auto tempo = ProjectTimeSignature::Get(m_data->projectRef()).GetTempo();
            if (const auto track = event.mpTrack.lock()) {
                DoProjectTempoChange(*track, tempo);
            }
        }
    });
}

void Au3ProjectAccessor::open()
{
    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    projectFileIO.OpenProject();
}

muse::Ret Au3ProjectAccessor::load(const muse::io::path_t& filePath)
{
    muse::Ret ret = muse::make_ok();

    auto& project = m_data->projectRef();
    auto& projectFileIO = ProjectFileIO::Get(project);
    std::string sstr = filePath.toStdString();
    FilePath fileName = wxString::FromUTF8(sstr.c_str(), sstr.size());

    std::optional<ProjectFileIO::TentativeConnection> conn;

    try {
        conn.emplace(projectFileIO.LoadProject(fileName, false /*ignoreAutosave*/).value());
    } catch (AudacityException& exception) {
        LOGE() << "failed load project: " << filePath << ", exception received";
        ret = project::make_ret(project::Err::AudacityExceptionError, filePath.toString());
        return ret;
    } catch (std::bad_optional_access)
    {
        if (static_cast<project::Err>(projectFileIO.GetLastErrorCode()) == project::Err::ProjectFileIsWriteProtected) {
            ret = project::make_ret(project::Err::ProjectFileIsWriteProtected, filePath.toString());
        } else {
            ret = project::make_ret(project::Err::DatabaseError, filePath.toString());
        }
        return ret;
    }

    muse::Defer([&conn]() {
        conn->Commit();
    });

    const bool bParseSuccess = conn.has_value();
    if (!bParseSuccess) {
        LOGE() << "failed load project: " << filePath;
        ret = make_ret(muse::Ret::Code::UnknownError, "Failed to load project: " + filePath.toString());
        return false;
    }

    //! TODO Look like, need doing all from method  ProjectFileManager::FixTracks
    //! and maybe what is done before this method (ProjectFileManager::ReadProjectFile)
    Au3TrackList& tracks = Au3TrackList::Get(project);
    for (auto pTrack : tracks) {
        pTrack->LinkConsistencyFix();
    }

    updateSavedState();

    return ret;
}

bool Au3ProjectAccessor::save(const muse::io::path_t& filePath)
{
    auto& project = m_data->projectRef();

    // Update saved state before SaveProject serializes the project.
    updateSavedState();

    auto& projectFileIO = ProjectFileIO::Get(project);
    auto result = projectFileIO.SaveProject(wxFromString(filePath.toString()), &TrackList::Get(project));
    if (result) {
        UndoManager::Get(project).StateSaved();
    }

    return result;
}

void Au3ProjectAccessor::updateSavedState()
{
    auto& project = m_data->projectRef();

    SavedMasterEffectList::Get(project).UpdateCopy();

    m_lastSavedTracks = TrackList::Create(nullptr);
    for (auto t : TrackList::Get(project).Any<WaveTrack>()) {
        m_lastSavedTracks->Add(t->Duplicate(Track::DuplicateOptions {}.Backup()), TrackList::DoAssignId::No);
    }
}

void Au3ProjectAccessor::clearSavedState()
{
    m_lastSavedTracks->Clear();
    m_lastSavedTracks.reset();
}

void Au3ProjectAccessor::close()
{
    auto& project = m_data->projectRef();

    //! ============================================================================
    //! NOTE Step 1 - Go back to the last saved state if needed
    //! ============================================================================
    auto& undoManager = UndoManager::Get(project);
    if (undoManager.GetSavedState() >= 0) {
        constexpr auto doAutoSave = false;
        ProjectHistory::Get(project).SetStateTo(
            undoManager.GetSavedState(), doAutoSave);
    }

    //! ============================================================================
    //! NOTE Step 2 - We compact the project only around the tracks that were actually
    //! saved. This will allow us to later on clear the UndoManager states bypassing
    //! the deletion of blocks 1 by 1, which can take a long time for projects with
    //!  large unsaved audio ; see https://github.com/audacity/audacity/issues/7382
    //! ============================================================================
    auto& projectFileIO = ProjectFileIO::Get(project);

    if (m_lastSavedTracks) {
        // Lock all blocks in all tracks of the last saved version, so that
        // the sample blocks aren't deleted from the database when we destroy the
        // sample block objects in memory.
        for (auto wt : m_lastSavedTracks->Any<WaveTrack>()) {
            WaveTrackUtilities::CloseLock(*wt);
        }

        // Attempt to compact the project
        projectFileIO.Compact({ m_lastSavedTracks.get() });

        if (
            !projectFileIO.WasCompacted() && undoManager.UnsavedChanges()) {
            // If compaction failed, we must do some work in case of close
            // without save.  Don't leave the document blob from the last
            // push of undo history, when that undo state may get purged
            // with deletion of some new sample blocks.
            // REVIEW: UpdateSaved() might fail too.  Do we need to test
            // for that and report it?
            projectFileIO.UpdateSaved(m_lastSavedTracks.get());
        }
    }

    //! ============================================================================
    //! NOTE Step 3
    //! Set (or not) the bypass flag to indicate that deletes that would happen
    //! during undoManager.ClearStates() below are not necessary. Must be called
    //! between `CompactProjectOnClose()` and `undoManager.ClearStates()`.
    //! ============================================================================
    projectFileIO.SetBypass();

    // This can reduce reference counts of sample blocks in the project's
    // tracks.
    undoManager.ClearStates();

    // Delete all the tracks to free up memory
    TrackList::Get(project).Clear();

    projectFileIO.CloseProject();
}

std::string Au3ProjectAccessor::title() const
{
    if (!m_data->project) {
        return std::string();
    }

    return wxToStdSting(m_data->project->GetProjectName());
}

uintptr_t Au3ProjectAccessor::au3ProjectPtr() const
{
    return reinterpret_cast<uintptr_t>(m_data->project.get());
}
