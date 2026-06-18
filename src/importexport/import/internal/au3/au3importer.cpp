#include "au3importer.h"

#include "framework/global/io/path.h"
#include "global/log.h"
#include "global/types/string.h"

#include "../legacyaupimporter.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3-label-track/LabelTrack.h"
#include "au3-stretching-sequence/TempoChange.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-import-export/ImportPlugin.h"
#include "au3-import-export/ImportProgressListener.h"
#include "au3-numeric-formats/ProjectTimeSignature.h"
#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-tags/Tags.h"
#include "au3-strings/TranslatableString.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/internal/domconverter.h"
#include "projectscene/view/tracksitemsview/dropcontroller.h"
#include "trackedit/internal/au3/au3trackdata.h"

#include "tempodetection.h"
using au::trackedit::ITrackDataPtr;
using au::trackedit::Au3TrackData;
using Au3TrackDataPtr = std::shared_ptr<Au3TrackData>;

using namespace au::au3;

class ImportProgress final : public ImportProgressListener
{
public:

    ImportProgress(AudacityProject& project)
    {
        //! TODO: AU4
        UNUSED(project);
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
            //: %1 is the description of the file format being imported
            auto title = TranslatableString("import-export", "Importing %1").arg(mImportFileHandle->GetFileDescription());
            mProgressDialog = BasicUI::MakeProgress(title, ::au3::untranslatable(ff.GetFullName()));
        }
        auto result = mProgressDialog->Poll(progress * ProgressSteps, ProgressSteps);
        if (result == BasicUI::ProgressResult::Cancelled) {
            mImportFileHandle->Cancel();
        } else if (result == BasicUI::ProgressResult::Stopped) {
            mImportFileHandle->Stop();
        }
    }

    void OnImportResult(ImportResult result) override
    {
        //! TODO: AU4
        UNUSED(result);
    }

private:

    ImportFileHandle* mImportFileHandle { nullptr };
    std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;
};

au::importexport::Au3Importer::Au3Importer(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx)
    , m_tempoDetection(std::make_unique<TempoDetection>(ctx))
{
}

au::importexport::Au3Importer::~Au3Importer() = default;

au::importexport::FileInfo au::importexport::Au3Importer::fileInfo(const muse::io::path_t& filePath)
{
    if (LegacyAupImporter::isLegacyAupFile(filePath)) {
        FileInfo fileInfo;
        fileInfo.path = filePath;
        fileInfo.trackCount = 1;
        return fileInfo;
    }

    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    auto importPlugins = Importer::sImportPluginList();

    FileInfo fileInfo;
    for (const auto plugin : importPlugins) {
        if (!plugin->SupportsExtension(suffix(filePath))) {
            continue;
        }

        auto inFile = plugin->Open(filePath.toStdString(), project);
        if ((inFile != NULL) && (inFile->GetStreamCount() > 0)) {
            fileInfo.path = filePath;
            fileInfo.duration = inFile->GetDuration();
            fileInfo.trackCount = inFile->GetRequiredTrackCount();

            return fileInfo;
        }
    }

    return FileInfo{};
}

bool au::importexport::Au3Importer::import(const muse::io::path_t& filePath)
{
    const std::string ext = suffix(filePath);
    for (const std::string& labelExt : labelsImporter()->supportedExtensions()) {
        if (labelExt == ext) {
            return labelsImporter()->importData(filePath).success();
        }
    }

    if (LegacyAupImporter::isLegacyAupFile(filePath)) {
        return importLegacyAup(filePath);
    }

    const bool projectWasEmpty = isProjectEmpty();

    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());

    auto oldTags = Tags::Get(*project).shared_from_this();
    bool committed = false;
    auto cleanup = finally([&]{
        if (!committed) {
            Tags::Set(*project, oldTags);
        }
    });
    auto newTags = oldTags->Duplicate();
    Tags::Set(*project, newTags);

    TrackHolders newTracks;
    TranslatableString errorMessage;
    std::optional<LibFileFormats::AcidizerTags> acidTags;
    {
        ImportProgress importProgress(*project);
        bool success = Importer::Get().Import(
            *project, wxFromString(filePath.toString()), &importProgress, &WaveTrackFactory::Get(*project),
            newTracks, newTags.get(), acidTags, errorMessage);

        if (!success) {
            return false;
        }
    } // ImportProgress (and its dialog) destroyed here, before tempo detection

    const auto projectTempo = ProjectTimeSignature::Get(*project).GetTempo();
    for (auto track : newTracks) {
        DoProjectTempoChange(*track, projectTempo);
    }

    // no more errors, commit
    committed = true;

    std::vector<WaveTrack*> importedWaveTracks;
    addImportedTracks(filePath, std::move(newTracks), &importedWaveTracks);

    std::vector<trackedit::TrackId> dstTrackIds;
    for (const auto* wt : importedWaveTracks) {
        dstTrackIds.push_back(static_cast<trackedit::TrackId>(wt->GetId()));
    }

    m_tempoDetection->onFilesImported({ filePath }, importedWaveTracks, dstTrackIds, acidTags, projectWasEmpty);

    return true;
}

bool au::importexport::Au3Importer::importLegacyAup(const muse::io::path_t& filePath)
{
    LegacyAupImporter legacyImporter;
    LegacyAupImporter::Result legacyProject = legacyImporter.resolve(filePath);
    if (!legacyProject.success) {
        LOGE() << "Failed to import legacy .aup project: " << filePath << ", error: " << legacyProject.error;
        return false;
    }

    for (const std::string& warning : legacyProject.warnings) {
        LOGW() << "Legacy .aup import warning: " << warning;
    }

    for (const LegacyAupImporter::Track& track : legacyProject.tracks) {
        const trackedit::TrackId trackId = tracksInteraction()->addWaveTrack(track.channels);
        if (trackId < 0) {
            return false;
        }

        if (!track.title.empty()) {
            tracksInteraction()->changeTrackTitle(trackId, muse::String::fromUtf8(track.title));
        }

        for (const LegacyAupImporter::Clip& clip : track.clips) {
            if (!importIntoTrackInternal(clip.filePath, trackId, clip.startTime, false, false)) {
                return false;
            }
        }
    }

    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    auto& tracks = Au3TrackList::Get(*project);
    const auto trackeditProject = globalContext()->currentTrackeditProject();

    for (const LegacyAupImporter::LabelTrack& track : legacyProject.labelTracks) {
        Au3LabelTrack* labelTrack = !track.title.empty()
                                    ? ::LabelTrack::Create(tracks, wxFromString(muse::String::fromUtf8(track.title)))
                                    : ::LabelTrack::Create(tracks);

        for (const LegacyAupImporter::Label& label : track.labels) {
            labelTrack->AddLabel(SelectedRegion(label.startTime, label.endTime),
                                 wxFromString(muse::String::fromUtf8(label.title)));
        }

        if (trackeditProject) {
            trackeditProject->notifyAboutTrackAdded(DomConverter::labelTrack(labelTrack));

            const auto& labels = labelTrack->GetLabels();
            for (size_t i = 0; i < labels.size(); ++i) {
                trackeditProject->notifyAboutLabelAdded(DomConverter::label(labelTrack, &labels[i]));
            }
        }
    }

    applyImportedProjectTitleIfNeeded(filePath);
    return true;
}

bool au::importexport::Au3Importer::importIntoTrack(const muse::io::path_t& filePath,
                                                    trackedit::TrackId dstTrackId,
                                                    muse::secs_t startTime)
{
    return importIntoTrackInternal(filePath, dstTrackId, startTime, true, true);
}

bool au::importexport::Au3Importer::importIntoTrackInternal(const muse::io::path_t& filePath,
                                                            trackedit::TrackId dstTrackId,
                                                            muse::secs_t startTime,
                                                            bool applyProjectTitle,
                                                            bool runTempoDetection)
{
    const bool projectWasEmpty = isProjectEmpty();

    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());

    TrackHolders tmpTracks;
    auto oldTags = Tags::Get(*project).shared_from_this();
    bool committed = false;
    auto cleanup = finally([&]{
        if (!committed) {
            Tags::Set(*project, oldTags);
        }
    });
    auto newTags = oldTags->Duplicate();
    Tags::Set(*project, newTags);
    std::optional<LibFileFormats::AcidizerTags> acidTags;
    TranslatableString errorMessage;

    {
        ImportProgress importProgressListener(*project);
        const wxString wxPath = filePath.toString().toUtf8().constData();
        const bool ok = Importer::Get().Import(
            *project,
            wxPath,
            &importProgressListener,
            &WaveTrackFactory::Get(*project),
            tmpTracks,
            newTags.get(),
            acidTags,
            errorMessage
            );

        if (!ok || tmpTracks.empty()) {
            return false;
        }
    } // ImportProgress (and its dialog) destroyed here, before tempo detection

    std::string baseName = filename(filePath, false).toStdString();
    std::vector<ITrackDataPtr> importedData;
    std::vector<WaveTrack*> importedWaveTracks;
    for (auto& holder : tmpTracks) {
        if (auto* wt = dynamic_cast<WaveTrack*>(holder.get())) {
            importedWaveTracks.push_back(wt);
            for (const auto& interval : wt->Intervals()) {
                interval->SetName(baseName);
            }
        }

        holder->ShiftBy(startTime);
        importedData.push_back(std::make_shared<Au3TrackData>(holder));
    }

    bool modifiedState = false;
    selectionController()->setSelectedTracks({ dstTrackId }, true);
    muse::Ret pasteRet = tracksInteraction()->paste(importedData, 0.0, false /* moveClips */, false /* moveAllTracks */,
                                                    true /* isMultiSelectionCopy */, modifiedState);
    if (!pasteRet) {
        return false;
    }

    if (applyProjectTitle) {
        applyImportedProjectTitleIfNeeded(filePath);
    }

    std::vector<trackedit::TrackId> dstTrackIds(importedWaveTracks.size(), dstTrackId);
    if (runTempoDetection) {
        m_tempoDetection->onFilesImported({ filePath }, importedWaveTracks, dstTrackIds, acidTags, projectWasEmpty);
    }

    return true;
}

bool au::importexport::Au3Importer::importFromSystemClipboard(
    const std::vector<muse::io::path_t>& filePaths, muse::secs_t startTime)
{
    // this is basically the same as drag&drop import so utilizing DropController to do the job
    projectscene::DropController dc;
    dc.setContext(iocContext());

    trackedit::TrackId startingTrack = -1;
    auto selectedTracks = selectionController()->selectedTracks();
    if (!selectedTracks.empty()) {
        startingTrack = selectedTracks.front();
    }

    QStringList files;
    for (const auto& path : filePaths) {
        files.append(path.toQString());
    }

    dc.probeAudioFiles(files);
    int requiredTracksCount = dc.requiredTracksCount();
    dc.prepareConditionalTracks(startingTrack, requiredTracksCount);
    auto trackIds = dc.draggedTracksIds(startingTrack, requiredTracksCount);
    std::vector<trackedit::TrackId> dstTrackIds;
    for (const QVariant& v : trackIds) {
        dstTrackIds.push_back(v.toInt());
    }

    dc.handleDroppedFiles(dstTrackIds, startTime);

    return true;
}

std::vector<std::string> au::importexport::Au3Importer::supportedExtensions() const
{
    static const std::vector<std::string> audioExtensions = [] {
        std::unordered_set<std::string> uniq;

        const auto fileTypes = Importer::Get().GetFileTypes(FileNames::FileType {});

        if (fileTypes.size() > 1) {
            const auto& exts = fileTypes[1].extensions;
            for (const auto& wxExt : exts) {
                std::string ext = wxExt.ToStdString();

                if (ext.empty() || ext == "*") {
                    continue;
                }

                if (!ext.empty() && ext.front() == '.') {
                    ext.erase(ext.begin());
                }

                uniq.emplace(ext);
            }
        }

        std::vector<std::string> out;
        out.reserve(uniq.size());
        for (auto& e : uniq) {
            out.push_back(e);
        }

        return out;
    }();

    // Audio formats (from au3) plus whatever the labels importer handles.
    std::vector<std::string> out = audioExtensions;
    out.push_back("aup");
    const std::vector<std::string> labelExtensions = labelsImporter()->supportedExtensions();
    out.insert(out.end(), labelExtensions.cbegin(), labelExtensions.cend());
    return out;
}

void au::importexport::Au3Importer::applyImportedProjectTitleIfNeeded(const muse::io::path_t& filePath)
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    auto& projectFileIO = ProjectFileIO::Get(*project);

    if (!projectFileIO.IsTemporary() || !project->GetProjectName().empty()) {
        return;
    }

    project->SetProjectName(wxFromString(filename(filePath, false).toString()));
    project->SetInitialImportPath(wxFromString(dirpath(filePath).toString()));
    projectFileIO.SetProjectTitle();
}

bool au::importexport::Au3Importer::isProjectEmpty() const
{
    auto trackeditProject = globalContext()->currentTrackeditProject();
    if (!trackeditProject) {
        return true;
    }

    return !trackeditProject->hasAudioContent().val;
}

void au::importexport::Au3Importer::addImportedTracks(const muse::io::path_t& fileName, TrackHolders&& newTracks,
                                                      std::vector<WaveTrack*>* outWaveTracks)
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    auto& tracks = TrackList::Get(*project);
    auto& projectFileIO = ProjectFileIO::Get(*project);

    std::vector<Track*> results;

    wxFileName fn(wxFromString(fileName.toString()));

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
            if (outWaveTracks) {
                outWaveTracks->push_back(pTrack);
            }
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
            //: Name default name assigned to a clip on track import
            newTrack->SetName(::TranslatableString("import-export", "%1 %2", "clip name template")
                              .arg(trackNameBase).arg(i + 1).translated().toStdString());
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

    applyImportedProjectTitleIfNeeded(fileName);

    auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    for (const auto& newTrack : results) {
        prj->notifyAboutTrackAdded(DomConverter::track(newTrack));
        for (const auto& clip : prj->clipList(newTrack->GetId())) {
            prj->notifyAboutClipAdded(clip);
        }
    }
}
