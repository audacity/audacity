#include "au3importer.h"

#include "BasicUI.h"
#include "TempoChange.h"
#include "Track.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "io/path.h"
#include "libraries/lib-import-export/ImportPlugin.h"
#include "libraries/lib-import-export/ImportProgressListener.h"
#include "libraries/lib-numeric-formats/ProjectTimeSignature.h"
#include "libraries/lib-project-file-io/ProjectFileIO.h"
#include "libraries/lib-tags/Tags.h"
#include "modules/import-export/RegisterImportPlugins.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"

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

void au::importexport::Au3Importer::init()
{
    RegisterImportPlugins();
}

bool au::importexport::Au3Importer::import(const muse::io::path_t& filePath)
{
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
    ImportProgress importProgress(*project);
    std::optional<LibFileFormats::AcidizerTags> acidTags;
    bool success = Importer::Get().Import(
        *project, wxFromString(filePath.toString()), &importProgress, &WaveTrackFactory::Get(*project),
        newTracks, newTags.get(), acidTags, errorMessage);

    if (!success) {
        return false;
    }

    const auto projectTempo = ProjectTimeSignature::Get(*project).GetTempo();
    for (auto track : newTracks) {
        DoProjectTempoChange(*track, projectTempo);
    }

    // no more errors, commit
    committed = true;

    addImportedTracks(filePath, std::move(newTracks));

    return true;
}

void au::importexport::Au3Importer::addImportedTracks(const muse::io::path_t& fileName, TrackHolders&& newTracks)
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    auto& tracks = TrackList::Get(*project);

    std::vector<Track*> results;

    wxFileName fn(fileName.toStdString());

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
