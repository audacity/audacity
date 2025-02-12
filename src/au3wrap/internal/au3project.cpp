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

#include "log.h"

using namespace au::au3;

class ImportProgress final : public ImportProgressListener
{
public:

    ImportProgress(AudacityProject& project)
    {
    }

    bool OnImportFileOpened(ImportFileHandle& importFileHandle) override
    {
        mImportFileHandle = &importFileHandle;
        //! TODO AU4: handle more than one stream
        // // File has more than one stream - display stream selector
        // if (importFileHandle.GetStreamCount() > 1)
        // {
        //     ImportStreamDialog ImportDlg(&importFileHandle, NULL, -1, XO("Select stream(s) to import"));

        //     if (ImportDlg.ShowModal() == wxID_CANCEL)
        //         return false;
        // }
        // // One stream - import it by default
        // else
        //     importFileHandle.SetStreamUsage(0,TRUE);
        return true;
    }

    void OnImportProgress(double progress) override
    {
        constexpr double ProgressSteps { 1000.0 };
        if (!mProgressDialog) {
            wxFileName ff(mImportFileHandle->GetFilename());
            auto title = XO("Importing %s").Format(mImportFileHandle->GetFileDescription());
            mProgressDialog = BasicUI::MakeProgress(title, Verbatim(ff.GetFullName()));
        }
        auto result = mProgressDialog->Poll(progress * ProgressSteps, ProgressSteps);
        if (result == BasicUI::ProgressResult::Cancelled) {
            mImportFileHandle->Cancel();
        } else if (result == BasicUI::ProgressResult::Stopped) {
            mImportFileHandle->Stop();
        }
    }

    void OnImportResult(ImportResult result) override {}

private:

    ImportFileHandle* mImportFileHandle { nullptr };
    std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;
};

std::shared_ptr<IAu3Project> Au3ProjectCreator::create() const
{
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

bool Au3ProjectAccessor::load(const muse::io::path_t& filePath)
{
    auto& project = m_data->projectRef();
    auto& projectFileIO = ProjectFileIO::Get(project);
    std::string sstr = filePath.toStdString();
    FilePath fileName = wxString::FromUTF8(sstr.c_str(), sstr.size());

    std::optional<ProjectFileIO::TentativeConnection> conn;

    try {
        conn.emplace(projectFileIO.LoadProject(fileName, false /*ignoreAutosave*/).value());
    } catch (AudacityException&) {
        LOGE() << "failed load project: " << filePath << ", exception received";
        return false;
    }

    muse::Defer([&conn]() {
        conn->Commit();
    });

    const bool bParseSuccess = conn.has_value();
    if (!bParseSuccess) {
        LOGE() << "failed load project: " << filePath;
        return false;
    }

    //! TODO Look like, need doing all from method  ProjectFileManager::FixTracks
    //! and maybe what is done before this method (ProjectFileManager::ReadProjectFile)
    Au3TrackList& tracks = Au3TrackList::Get(project);
    for (auto pTrack : tracks) {
        pTrack->LinkConsistencyFix();
    }

    updateSavedState();

    return true;
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

bool Au3ProjectAccessor::import(const muse::io::path_t& filePath)
{
    auto& project = m_data->projectRef();
    auto& projectFileIO = ProjectFileIO::Get(project);

    auto oldTags = Tags::Get(project).shared_from_this();
    bool committed = false;
    auto cleanup = finally([&]{
        if (!committed) {
            Tags::Set(project, oldTags);
        }
    });
    auto newTags = oldTags->Duplicate();
    Tags::Set(project, newTags);

    bool initiallyEmpty = TrackList::Get(project).empty();
    TrackHolders newTracks;
    TranslatableString errorMessage;
    ImportProgress importProgress(project);
    std::optional<LibFileFormats::AcidizerTags> acidTags;
    bool success = Importer::Get().Import(
        project, wxFromString(filePath.toString()), &importProgress, &WaveTrackFactory::Get(project),
        newTracks, newTags.get(), acidTags, errorMessage);

    if (!success) {
        return false;
    }

    const auto projectTempo = ProjectTimeSignature::Get(project).GetTempo();
    for (auto track : newTracks) {
        DoProjectTempoChange(*track, projectTempo);
    }

    // no more errors, commit
    committed = true;

    addImportedTracks(filePath, std::move(newTracks));
}

void Au3ProjectAccessor::addImportedTracks(const muse::io::path_t& fileName, TrackHolders&& newTracks)
{
    auto& project = m_data->projectRef();
    auto& projectFileIO = ProjectFileIO::Get(project);
    auto& tracks = TrackList::Get(project);

    std::vector<Track*> results;

    wxFileName fn(fileName.toStdString());

    bool initiallyEmpty = tracks.empty();
    double newRate = 0;
    wxString trackNameBase = fn.GetName();
    int i = -1;

    // Fix the bug 2109.
    // In case the project had soloed tracks before importing,
    // all newly imported tracks are muted.
    const bool projectHasSolo
        =!(tracks.Any<PlayableTrack>() + &PlayableTrack::GetSolo).empty();
    if (projectHasSolo) {
        for (auto& group : newTracks) {
            if (auto pTrack = dynamic_cast<PlayableTrack*>(group.get())) {
                pTrack->SetMute(true);
            }
        }
    }

    for (auto& group : newTracks) {
        if (auto pTrack = dynamic_cast<WaveTrack*>(group.get())) {
            results.push_back(pTrack);
        }
        tracks.Add(group);
    }
    newTracks.clear();

    // Now name them

    // Add numbers to track names only if there is more than one (mono or stereo)
    // track (not necessarily, more than one channel)
    const bool useSuffix = results.size() > 1;

    for (const auto& newTrack : results) {
        ++i;
        newTrack->SetSelected(true);
        if (useSuffix) {
            //i18n-hint Name default name assigned to a clip on track import
            newTrack->SetName(XC("%s %d", "clip name template")
                              .Format(trackNameBase, i + 1).Translation());
        } else {
            newTrack->SetName(trackNameBase);
        }

        newTrack->TypeSwitch([&](WaveTrack& wt) {
            if (newRate == 0) {
                newRate = wt.GetRate();
            }
            const auto trackName = wt.GetName();
            for (const auto& interval : wt.Intervals()) {
                interval->SetName(trackName);
            }
        });
    }

    //! TODO AU4: if project was empty, set:
    //! - project name/title
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
