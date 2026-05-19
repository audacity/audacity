#include "au3importer.h"

#include "framework/global/io/path.h"

#include "au3-basic-ui/BasicUI.h"
#include "importexport/export/OriginalFileInfo.h"
#include "au3-stretching-sequence/TempoChange.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-import-export/ImportPlugin.h"
#include "au3-import-export/ImportProgressListener.h"
#include "au3-import-export/ExportPlugin.h"
#include "au3-import-export/ExportPluginRegistry.h"
#include "au3-import-export/ExportUtils.h"
#include "au3-preferences/Prefs.h"
#include "au3-numeric-formats/ProjectTimeSignature.h"
#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-tags/Tags.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "projectscene/view/tracksitemsview/dropcontroller.h"
#include "trackedit/internal/au3/au3trackdata.h"

#include "tempodetection.h"

#include <QFileInfo>
#include <QVariantMap>

#include <algorithm>
#include <cmath>
#include <limits>
#include <optional>
#include <type_traits>
#include <tuple>
#include <variant>

using au::trackedit::ITrackDataPtr;
using au::trackedit::Au3TrackData;
using Au3TrackDataPtr = std::shared_ptr<Au3TrackData>;

using namespace au::au3;

namespace {
int bitDepthFromSampleFormat(sampleFormat format)
{
    if (format == int16Sample) {
        return 16;
    }
    if (format == int24Sample) {
        return 24;
    }
    if (format == floatSample) {
        return 32;
    }
    return 0;
}

std::tuple<ExportPlugin*, int> findExportFormat(const wxString& formatID, const wxString& extension)
{
    if (!formatID.empty()) {
        auto [plugin, formatIndex] = ExportPluginRegistry::Get().FindFormat(formatID);
        if (plugin) {
            return { plugin, formatIndex };
        }
    }

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        const FormatInfo formatInfo = plugin->GetFormatInfo(formatIndex);
        if (formatInfo.extensions.Index(extension, false) != wxNOT_FOUND) {
            return { plugin, formatIndex };
        }
    }

    return { nullptr, -1 };
}

std::optional<double> numericValue(const ExportValue& value)
{
    return std::visit([](const auto& v) -> std::optional<double> {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, int> || std::is_same_v<T, double>) {
            return static_cast<double>(v);
        } else {
            return std::nullopt;
        }
    }, value);
}

std::optional<ExportValue> closestNumericOptionValue(const ExportOption& option, double target)
{
    std::optional<ExportValue> closest;
    double closestDistance = std::numeric_limits<double>::max();

    for (const auto& value : option.values) {
        const auto numeric = numericValue(value);
        if (!numeric) {
            continue;
        }

        const double distance = std::abs(*numeric - target);
        if (distance < closestDistance) {
            closest = value;
            closestDistance = distance;
        }
    }

    return closest;
}

void applyCodecSettings(ExportOptionsEditor& editor, const QVariantMap& codecSettings)
{
    const bool hasBitDepth = codecSettings.contains("bitDepth");
    const bool hasBitRate = codecSettings.contains("bitRate");
    const double bitDepth = codecSettings.value("bitDepth").toDouble();
    double bitRate = codecSettings.value("bitRate").toDouble();
    if (bitRate > 1000.0) {
        bitRate /= 1000.0;
    }

    for (int index = 0; index < editor.GetOptionsCount(); ++index) {
        ExportOption option;
        if (!editor.GetOption(index, option)) {
            continue;
        }

        const std::string title = option.title.Translation().Lower().ToStdString();
        const bool isBitDepth = title.find("bit depth") != std::string::npos;
        const bool isBitRate = title.find("bit rate") != std::string::npos
                               || title.find("bitrate") != std::string::npos
                               || title.find("quality") != std::string::npos;

        if (hasBitDepth && isBitDepth) {
            if (auto value = closestNumericOptionValue(option, bitDepth)) {
                editor.SetValue(option.id, *value);
            }
        } else if (hasBitRate && isBitRate) {
            const auto value = closestNumericOptionValue(option, bitRate);
            const auto numeric = value ? numericValue(*value) : std::nullopt;
            if (value && numeric && *numeric >= 32.0) {
                editor.SetValue(option.id, *value);
            }
        }
    }
}

au::importexport::ExportParameters exportParametersFor(ExportPlugin& plugin, int formatIndex,
                                                       const QVariantMap& codecSettings)
{
    au::importexport::ExportParameters parameters;

    auto editor = plugin.CreateOptionsEditor(formatIndex, nullptr);
    if (!editor) {
        return parameters;
    }

    if (gPrefs) {
        editor->Load(*gPrefs);
    }

    applyCodecSettings(*editor, codecSettings);

    for (const auto& [id, value] : ExportUtils::ParametersFromEditor(*editor)) {
        parameters.emplace_back(id, std::visit([](const auto& v) -> au::importexport::OptionValue {
            return v;
        }, value));
    }

    return parameters;
}
}

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

bool au::importexport::Au3Importer::importIntoTrack(const muse::io::path_t& filePath,
                                                    trackedit::TrackId dstTrackId,
                                                    muse::secs_t startTime)
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

    applyImportedProjectTitleIfNeeded(filePath);

    std::vector<trackedit::TrackId> dstTrackIds(importedWaveTracks.size(), dstTrackId);
    m_tempoDetection->onFilesImported({ filePath }, importedWaveTracks, dstTrackIds, acidTags, projectWasEmpty);

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
    static const std::vector<std::string> supportedExtensions = [] {
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

    return supportedExtensions;
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
    
    // Capture original file info for "Overwrite Original" feature
    // Check if this is the first import (not whether project is physically empty)
    auto& fileInfo = OriginalFileInfo::Get(*project);
    const bool captureOriginalFileInfo = fileInfo.GetImportedFileCount() == 0;
    if (captureOriginalFileInfo) {
        // First file being imported - capture its path and format
        QString filePath = QString::fromStdString(fileName.toString().toStdString());
        QString displayName = QString::fromStdString(fn.GetFullName().ToStdString());
        const wxString extension = fn.GetExt();
        const wxString formatID = extension.Upper();
        auto [plugin, formatIndex] = findExportFormat(formatID, extension);
        
        fileInfo.SetOriginalFile(filePath, displayName);
        fileInfo.SetExportFormatID(QString::fromStdString(plugin
                                                           ? plugin->GetFormatInfo(formatIndex).format.ToStdString()
                                                           : formatID.ToStdString()));
    }
    // Increment import count on every import (whether first or subsequent)
    fileInfo.IncrementImportedFileCount();

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

    if (captureOriginalFileInfo) {
        QVariantMap codecSettings;
        codecSettings.insert("format", fileInfo.GetExportFormatID());
        codecSettings.insert("fileExtension", QString::fromStdString(fn.GetExt().Lower().ToStdString()));

        int channels = 0;
        int bitDepth = 0;
        double sampleRate = 0.0;
        double importedDuration = 0.0;
        for (const auto* wt : results) {
            if (!wt) {
                continue;
            }

            channels = std::max(channels, static_cast<int>(wt->NChannels()));
            if (sampleRate <= 0.0) {
                sampleRate = wt->GetRate();
            }
            if (bitDepth == 0) {
                bitDepth = bitDepthFromSampleFormat(wt->GetSampleFormat());
            }
            importedDuration = std::max(importedDuration, wt->GetEndTime());
        }

        if (sampleRate > 0.0) {
            codecSettings.insert("sampleRate", static_cast<int>(std::lround(sampleRate)));
        }
        if (channels > 0) {
            codecSettings.insert("channels", channels);
        }
        if (bitDepth > 0) {
            codecSettings.insert("bitDepth", bitDepth);
        }

        const QFileInfo originalFile(fileInfo.GetOriginalFilePath());
        if (originalFile.exists() && originalFile.size() > 0) {
            if (importedDuration > 0.0) {
                codecSettings.insert("bitRate", static_cast<qlonglong>((originalFile.size() * 8.0) / importedDuration));
            }
        }

        fileInfo.SetCodecSettings(codecSettings);

        auto [plugin, formatIndex] = findExportFormat(wxString(fileInfo.GetExportFormatID().toStdString()), fn.GetExt());
        if (plugin) {
            fileInfo.SetExportParameters(exportParametersFor(*plugin, formatIndex, codecSettings));
        }
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
