/*
* Audacity: A Digital Audio Editor
*/

#include "au3exporter.h"

#include <algorithm>
#include <optional>

#include "framework/global/async/asyncable.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3-import-export/ExportPluginRegistry.h"
#include "au3-import-export/ExportUtils.h"
#include "au3-mixer/MixerOptions.h"
#include "au3-tags/Tags.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-strings/Internat.h"
#include "au3-strings/TranslatableString.h"

#include "RegisterExportPlugins.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "importexport/export/exportutils.h"
#include "trackedit/itrackeditproject.h"

#include "translation.h"

using namespace au::au3;
using namespace au::importexport;

namespace {
ExportPlugin* formatPlugin(const std::string& format)
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            return plugin;
        }
    }

    return nullptr;
}

std::vector<bool> prepareChannelMask(TrackList& trackList, bool selectedOnly)
{
    auto tracks = trackList.Any<WaveTrack>();
    std::vector<bool> channelMask(
        tracks.sum([](const auto track) { return track->NChannels(); }),
        false);
    unsigned trackIndex = 0;
    for (const auto track : tracks) {
        if (track->GetSolo()) {
            channelMask.assign(channelMask.size(), false);
            for (unsigned i = 0; i < track->NChannels(); ++i) {
                channelMask[trackIndex++] = true;
            }
            break;
        }
        if (!track->GetMute() && (!selectedOnly || track->GetSelected())) {
            for (unsigned i = 0; i < track->NChannels(); ++i) {
                channelMask[trackIndex++] = true;
            }
        } else {
            trackIndex += track->NChannels();
        }
    }

    return channelMask;
}

std::string separateFileName(const std::string& prefix, std::optional<int> number, const std::string& title,
                             std::vector<std::string>& usedNames)
{
    const std::string itemName = title.empty() ? muse::trc("export", "untitled") : title;
    wxString name = wxFromStdString(au::importexport::utils::separateFileName(prefix, number, itemName));
    Internat::SanitiseFilename(name, wxT("_"));
    return au::importexport::utils::makeFileNameUnique(wxToStdString(name), usedNames);
}

class ExclusiveTrackSelection
{
public:
    ExclusiveTrackSelection(TrackList& tracks, const Track* selectedTrack)
    {
        for (Track* track : tracks) {
            m_previous.emplace_back(track, track->GetSelected());
            track->SetSelected(track == selectedTrack);
        }
    }

    ~ExclusiveTrackSelection()
    {
        for (const auto& [track, selected] : m_previous) {
            track->SetSelected(selected);
        }
    }

private:
    std::vector<std::pair<Track*, bool> > m_previous;
};
}

class ProgressDelegate : public ExportProcessorDelegate, public muse::async::Asyncable
{
    muse::ProgressPtr m_progress;
    std::atomic<bool> m_cancelled { false };
public:
    ProgressDelegate(muse::ProgressPtr progress)
        : m_progress(progress)
    {
        m_progress->canceled().onNotify(this, [this] { m_cancelled = true; });
    }

    bool IsCancelled() const override { return m_cancelled; }
    bool IsStopped()   const override { return false; }
    void SetStatusString(const ::TranslatableString&) override {}
    void OnProgress(double) override {}
};

class DialogExportProgressDelegate : public ExportProcessorDelegate
{
    std::atomic<bool> mCancelled { false };
    std::atomic<bool> mStopped { false };
    std::atomic<double> mProgress {};

    TranslatableString mStatus;

    std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;
public:

    bool IsCancelled() const override
    {
        return mCancelled;
    }

    bool IsStopped() const override
    {
        return mStopped;
    }

    void SetStatusString(const ::TranslatableString& str) override
    {
        mStatus = str;
    }

    void OnProgress(double progress) override
    {
        mProgress = progress;
    }

    void UpdateUI()
    {
        constexpr long long ProgressSteps = 1000ul;

        if (!mProgressDialog) {
            //: Title of the export progress dialog
            mProgressDialog = BasicUI::MakeProgress(::TranslatableString("import-export", "Export"), mStatus);
        } else {
            mProgressDialog->SetMessage(mStatus);
        }

        const auto result = mProgressDialog->Poll(mProgress * ProgressSteps, ProgressSteps);

        if (result == BasicUI::ProgressResult::Cancelled) {
            if (!mStopped) {
                mCancelled = true;
            }
        } else if (result == BasicUI::ProgressResult::Stopped) {
            if (!mCancelled) {
                mStopped = true;
            }
        }
    }
};

void Au3Exporter::init()
{
    RegisterExportPlugins();
    ExportPluginRegistry::Get().Initialize();
}

muse::Ret Au3Exporter::prepareFormat(const Options& options)
{
    const std::string formatName = options.count(OptionKey::Format)
                                   ? options.at(OptionKey::Format).toString()
                                   : exportConfiguration()->currentFormat();
    const int exportSampleRate = options.count(OptionKey::ExportSampleRate)
                                 ? options.at(OptionKey::ExportSampleRate).toInt()
                                 : exportConfiguration()->exportSampleRate();

    ExportParameters parameters;
    if (options.count(OptionKey::Parameters)) {
        for (const auto& entryVal : options.at(OptionKey::Parameters).toList()) {
            const auto& m = entryVal.toMap();
            const int id = m.at("id").toInt();
            const auto& valueVal = m.at("value");
            OptionValue value;
            switch (valueVal.type()) {
            case muse::Val::Type::Bool:
                value = valueVal.toBool();
                break;
            case muse::Val::Type::Int:
                value = valueVal.toInt();
                break;
            case muse::Val::Type::Double:
                value = valueVal.toDouble();
                break;
            case muse::Val::Type::String:
                value = valueVal.toString();
                break;
            default: break;
            }
            parameters.emplace_back(id, value);
        }
    } else {
        const int fmt = formatIndex(formatName);
        const ExportPlugin* plugin = formatPlugin(formatName);
        if (plugin) {
            auto editor = plugin->CreateOptionsEditor(fmt, nullptr);
            editor->Load(*gPrefs);
            for (const auto& [id, val] : ExportUtils::ParametersFromEditor(*editor)) {
                parameters.emplace_back(id, std::visit([](auto v) -> OptionValue { return v; }, val));
            }
        }
    }

    const int formatIdx = formatIndex(formatName);
    if (formatIdx == -1) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }
    m_format = formatIdx;

    m_plugin = formatPlugin(formatName);
    if (!m_plugin) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    m_parameters.clear();
    for (const auto& [id, val] : parameters) {
        m_parameters.emplace_back(id, std::visit([](auto v) -> ExportValue { return v; }, val));
    }

    m_sampleRate = exportSampleRate;

    return muse::make_ok();
}

std::string Au3Exporter::formatExtension(const Options& options) const
{
    const std::string formatName = options.count(OptionKey::Format)
                                   ? options.at(OptionKey::Format).toString()
                                   : exportConfiguration()->currentFormat();
    const std::vector<std::string> extensions = formatExtensions(formatName);
    return extensions.empty() ? std::string() : extensions.front();
}

muse::Ret Au3Exporter::runExport(Au3Project& project, const wxFileName& wxfilename, muse::ProgressPtr progress)
{
    try {
        auto processor = m_plugin->CreateProcessor(m_format);
        if (!processor->Initialize(project,
                                   m_parameters,
                                   wxfilename.GetFullPath(),
                                   m_t0, m_t1, m_selectedOnly,
                                   m_sampleRate, m_numChannels,
                                   m_mixerSpec,
                                   m_tags)) {
            return muse::make_ret(muse::Ret::Code::InternalError);
        }

        auto exportTask = ExportTask([actualFilename = wxfilename,
                                      targetFilename = wxfilename,
                                      processor = std::shared_ptr<ExportProcessor>(processor.release())]
                                     (ExportProcessorDelegate& delegate)
        {
            auto result = ExportResult::Error;
            auto cleanup = finally([&] {
                if (result == ExportResult::Success || result == ExportResult::Stopped) {
                    if (actualFilename != targetFilename) {
                        //may fail...
                        ::wxRenameFile(actualFilename.GetFullPath(),
                                       targetFilename.GetFullPath(),
                                       true);
                    }
                } else {
                    ::wxRemoveFile(actualFilename.GetFullPath());
                }
            });

            result = processor->Process(delegate);
            return result;
        });

        auto f = exportTask.get_future();
        ExportResult result = ExportResult::Error;

        if (progress) {
            ProgressDelegate delegate(progress);
            std::thread(std::move(exportTask), std::ref(delegate)).detach();
            while (f.wait_for(std::chrono::milliseconds(50)) != std::future_status::ready) {}
            result = f.get();
        } else {
            DialogExportProgressDelegate delegate;
            std::thread(std::move(exportTask), std::ref(delegate)).detach();
            while (f.wait_for(std::chrono::milliseconds(50)) != std::future_status::ready) {
                delegate.UpdateUI();
            }
            result = f.get();
        }

        if (result != ExportResult::Success) {
            return muse::make_ret(muse::Ret::Code::InternalError);
        }
    } catch (const ExportException& e) {
        return muse::make_ret(muse::Ret::Code::InternalError, e.What().ToStdString());
    }

    return muse::make_ret(muse::Ret::Code::Ok);
}

muse::Ret Au3Exporter::prepareMix(Au3Project& project, const Options& options)
{
    const int exportChannelsType = options.count(OptionKey::ExportChannelsType)
                                   ? options.at(OptionKey::ExportChannelsType).toInt()
                                   : exportConfiguration()->exportChannelsType();
    const int exportChannels = options.count(OptionKey::ExportChannels)
                               ? options.at(OptionKey::ExportChannels).toInt()
                               : exportConfiguration()->exportChannels();
    const muse::Val exportCustomChannelMapping = options.count(OptionKey::ExportCustomChannelMapping)
                                                 ? options.at(OptionKey::ExportCustomChannelMapping)
                                                 : exportConfiguration()->exportCustomChannelMapping();

    const auto exportedTracks = ExportUtils::FindExportWaveTracks(TrackList::Get(project), m_selectedOnly);
    if (exportedTracks.empty()) {
        //! NOTE: All selected audio is muted
        return muse::make_ret(muse::Ret::Code::InternalError, muse::trc("export", "All selected audio is muted"));
    }

    int inputChannelsCount = 0;
    for (const auto& exportedTrack : exportedTracks) {
        inputChannelsCount += exportedTrack->NChannels();
    }

    m_downMix = std::make_unique<MixerOptions::Downmix>(inputChannelsCount, exportChannels);
    m_mixerSpec = nullptr;
    if (ExportChannelsPref::ExportChannels(exportChannelsType) == ExportChannelsPref::ExportChannels::MONO) {
        m_numChannels = 1;
    } else if (ExportChannelsPref::ExportChannels(exportChannelsType)
               == ExportChannelsPref::ExportChannels::STEREO) {
        m_numChannels = 2;
    } else {
        //Figure out the final channel mapping: mixer dialog shows
        //all tracks regardless of their mute/solo state, but
        //muted channels should not be present in exported file -
        //apply channel mask to exclude them
        auto& trackList = TrackList::Get(project);
        auto channelMask = prepareChannelMask(trackList, m_selectedOnly);
        m_downMix = std::make_unique<MixerOptions::Downmix>(*m_downMix, channelMask);
        m_mixerSpec = m_downMix.get();

        const std::vector<std::vector<bool> > matrix = utils::valToMatrix(exportCustomChannelMapping);
        m_numChannels = exportChannels;

        for (int in = 0; in < inputChannelsCount; ++in) {
            for (unsigned int out = 0; out < m_numChannels; ++out) {
                m_mixerSpec->mMap[in][out] = false;
            }
        }

        const int rows = std::min(inputChannelsCount, static_cast<int>(matrix.size()));
        for (int in = 0; in < rows; ++in) {
            const int cols = std::min(static_cast<int>(m_numChannels), static_cast<int>(matrix[in].size()));
            for (int out = 0; out < cols; ++out) {
                if (matrix[in][out]) {
                    m_mixerSpec->mMap[in][out] = true;
                }
            }
        }
    }

    return muse::make_ok();
}

muse::Ret Au3Exporter::exportData(const muse::io::path_t& path, const Options& options, muse::ProgressPtr progress,
                                  au::project::IAudacityProjectPtr project)
{
    auto exportProject = project ? project : globalContext()->currentProject();
    IF_ASSERT_FAILED(exportProject) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    Au3Project* au3Project = reinterpret_cast<Au3Project*>(exportProject->au3ProjectPtr());
    IF_ASSERT_FAILED(au3Project) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    const muse::Ret prepared = prepareFormat(options);
    if (!prepared) {
        return prepared;
    }

    const ExportProcessType processType = options.count(OptionKey::ProcessType)
                                          ? options.at(OptionKey::ProcessType).toEnum<ExportProcessType>()
                                          : exportConfiguration()->processType();
    m_selectedOnly = false;
    // TODO: implement other ExportProcessType's selections
    if (processType == ExportProcessType::SELECTED_AUDIO) {
        m_t0
            = !selectionController()->timeSelectionIsEmpty() ? selectionController()->dataSelectedStartTime()
              : selectionController()->leftMostSelectedClipStartTime().value_or(0.0);
        m_t1
            = !selectionController()->timeSelectionIsEmpty() ? selectionController()->dataSelectedEndTime()
              : selectionController()->rightMostSelectedClipEndTime().value_or(0.0);
        m_selectedOnly = true;
    } else if (processType == ExportProcessType::AUDIO_IN_LOOP_REGION) {
        auto region = playbackController()->loopRegion();
        m_t0 = region.start;
        m_t1 = region.end;
    } else {
        auto trackeditProject = exportProject->trackeditProject();

        m_t0 = 0.0;
        m_t1 = trackeditProject->totalTime().to_double();
    }

    m_tags = &Tags::Get(*au3Project);

    const muse::Ret mixed = prepareMix(*au3Project, options);
    if (!mixed) {
        return mixed;
    }

    if (exportConfiguration()->trimBlankSpace()) {
        const auto exportedTracks = ExportUtils::FindExportWaveTracks(TrackList::Get(*au3Project), m_selectedOnly);
        const double firstClipStart = exportedTracks.min(&Track::GetStartTime);
        if (firstClipStart > m_t0 && firstClipStart < m_t1) {
            m_t0 = firstClipStart;
        }
    }

    return runExport(*au3Project, wxFromPath(path), progress);
}

std::vector<Au3Exporter::SeparateFile> Au3Exporter::separateFiles(Au3Project& project, const Options& options) const
{
    const ExportProcessType processType = options.count(OptionKey::ProcessType)
                                          ? options.at(OptionKey::ProcessType).toEnum<ExportProcessType>()
                                          : exportConfiguration()->processType();
    const std::string prefix = options.count(OptionKey::FileNamePrefix)
                               ? options.at(OptionKey::FileNamePrefix).toString()
                               : std::string();
    const bool includeNumbers = options.count(OptionKey::IncludeNumbers)
                                ? options.at(OptionKey::IncludeNumbers).toBool()
                                : exportConfiguration()->includeNumbers();

    if (processType == ExportProcessType::EACH_LABEL_AS_SEPARATE_AUDIO_FILE) {
        const bool includeAudioBeforeFirstLabel = options.count(OptionKey::IncludeAudioBeforeFirstLabel)
                                                  ? options.at(OptionKey::IncludeAudioBeforeFirstLabel).toBool()
                                                  : exportConfiguration()->includeAudioBeforeFirstLabel();
        return labelFiles(project, prefix, includeNumbers, includeAudioBeforeFirstLabel);
    }

    return trackFiles(project, prefix, includeNumbers);
}

std::vector<Au3Exporter::SeparateFile> Au3Exporter::trackFiles(Au3Project& project, const std::string& prefix,
                                                               bool includeNumbers) const
{
    const bool trimBlankSpace = exportConfiguration()->trimBlankSpace();

    auto& tracks = TrackList::Get(project);
    const bool anySolo = !(tracks.Any<const WaveTrack>() + &WaveTrack::GetSolo).empty();
    auto waveTracks = tracks.Any<WaveTrack>() - (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute);

    std::vector<SeparateFile> files;
    std::vector<std::string> usedNames;
    int number = 1;
    for (WaveTrack* track : waveTracks) {
        if (track->IsEmpty()) {
            continue;
        }

        SeparateFile file;
        file.track = track;
        file.number = number;
        file.title = wxToStdString(track->GetName());
        file.t0 = trimBlankSpace ? track->GetStartTime() : 0.0;
        file.t1 = track->GetEndTime();
        file.name = separateFileName(prefix, includeNumbers ? std::optional<int>(number) : std::nullopt, file.title, usedNames);

        files.push_back(std::move(file));
        ++number;
    }

    return files;
}

std::vector<Au3Exporter::SeparateFile> Au3Exporter::labelFiles(Au3Project& project, const std::string& prefix, bool includeNumbers,
                                                               bool includeAudioBeforeFirstLabel) const
{
    const trackedit::ITrackeditProjectPtr trackeditProject = globalContext()->currentTrackeditProject();
    if (!trackeditProject) {
        return {};
    }

    trackedit::Labels labels;
    for (const trackedit::Track& track : trackeditProject->trackList()) {
        if (track.type != trackedit::TrackType::Label) {
            continue;
        }

        const auto trackLabels = trackeditProject->labelList(track.id);
        labels.assign(trackLabels.begin(), trackLabels.end());
        break;
    }

    std::sort(labels.begin(), labels.end(), [](const trackedit::Label& a, const trackedit::Label& b) {
        return a.startTime < b.startTime;
    });

    std::vector<utils::TimeRange> labelRanges;
    labelRanges.reserve(labels.size());
    for (const trackedit::Label& label : labels) {
        labelRanges.push_back({ label.startTime, label.endTime });
    }
    const std::vector<utils::TimeRange> ranges = utils::labelExportRanges(labelRanges, trackeditProject->totalTime().to_double());

    std::vector<SeparateFile> files;
    std::vector<std::string> usedNames;

    if (includeAudioBeforeFirstLabel && !labels.empty()) {
        double start = 0.0;
        if (exportConfiguration()->trimBlankSpace()) {
            const auto exportedTracks = ExportUtils::FindExportWaveTracks(TrackList::Get(project), false);
            if (!exportedTracks.empty()) {
                start = std::max(0.0, exportedTracks.min(&Track::GetStartTime));
            }
        }

        const double end = labels.front().startTime;
        if (start < end) {
            SeparateFile file;
            file.number = 0;
            file.title = muse::io::completeBasename(globalContext()->currentProject()->displayName()).toStdString();
            file.t0 = start;
            file.t1 = end;
            file.name = separateFileName(prefix, includeNumbers ? std::optional<int>(0) : std::nullopt, file.title, usedNames);

            files.push_back(std::move(file));
        }
    }

    int number = 1;
    for (size_t i = 0; i < labels.size(); ++i) {
        if (ranges[i].end <= ranges[i].start) {
            continue;
        }

        SeparateFile file;
        file.number = number;
        file.title = labels[i].title.toStdString();
        file.t0 = ranges[i].start;
        file.t1 = ranges[i].end;
        file.name = separateFileName(prefix, includeNumbers ? std::optional<int>(number) : std::nullopt, file.title, usedNames);

        files.push_back(std::move(file));
        ++number;
    }

    return files;
}

std::vector<std::string> Au3Exporter::separateFileNames(const Options& options) const
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return {};
    }

    Au3Project* au3Project = reinterpret_cast<Au3Project*>(project->au3ProjectPtr());
    if (!au3Project) {
        return {};
    }

    const std::string extension = formatExtension(options);
    std::vector<std::string> names;
    for (const SeparateFile& file : separateFiles(*au3Project, options)) {
        names.push_back(extension.empty() ? file.name : file.name + "." + extension);
    }

    return names;
}

muse::Ret Au3Exporter::exportSeparateFiles(const muse::io::path_t& directory, const Options& options, muse::ProgressPtr progress)
{
    const auto exportProject = globalContext()->currentProject();
    IF_ASSERT_FAILED(exportProject) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    Au3Project* au3Project = reinterpret_cast<Au3Project*>(exportProject->au3ProjectPtr());
    IF_ASSERT_FAILED(au3Project) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    const muse::Ret prepared = prepareFormat(options);
    if (!prepared) {
        return prepared;
    }

    const ExportProcessType processType = options.count(OptionKey::ProcessType)
                                          ? options.at(OptionKey::ProcessType).toEnum<ExportProcessType>()
                                          : exportConfiguration()->processType();
    const bool byLabels = processType == ExportProcessType::EACH_LABEL_AS_SEPARATE_AUDIO_FILE;

    const std::vector<SeparateFile> files = separateFiles(*au3Project, options);
    if (files.empty()) {
        return muse::make_ret(muse::Ret::Code::InternalError,
                              byLabels ? muse::trc("export", "No labels to export.")
                              : muse::trc("export", "There are no tracks to export"));
    }

    const auto exportChannelsType = ExportChannelsPref::ExportChannels(options.count(OptionKey::ExportChannelsType)
                                                                       ? options.at(OptionKey::ExportChannelsType).toInt()
                                                                       : exportConfiguration()->exportChannelsType());
    const std::string extension = formatExtension(options);
    const Tags& projectTags = Tags::Get(*au3Project);
    auto& tracks = TrackList::Get(*au3Project);

    if (byLabels) {
        m_selectedOnly = false;
        const muse::Ret mixed = prepareMix(*au3Project, options);
        if (!mixed) {
            return mixed;
        }
    } else {
        m_selectedOnly = true;
        m_mixerSpec = nullptr;
    }

    for (const SeparateFile& file : files) {
        std::optional<ExclusiveTrackSelection> selection;
        if (file.track) {
            selection.emplace(tracks, file.track);

            switch (exportChannelsType) {
            case ExportChannelsPref::ExportChannels::MONO:
                m_numChannels = 1;
                break;
            case ExportChannelsPref::ExportChannels::STEREO:
                m_numChannels = 2;
                break;
            default:
                m_numChannels = file.track->NChannels();
                break;
            }
        }

        m_t0 = file.t0;
        m_t1 = file.t1;

        Tags tags = projectTags;
        tags.SetTag(TAG_TITLE, wxFromStdString(file.title));
        tags.SetTag(TAG_TRACK, wxString::Format(wxT("%d"), file.number));
        m_tags = &tags;

        muse::io::path_t path = directory.appendingComponent(file.name);
        if (!extension.empty()) {
            path = path.appendingSuffix(extension);
        }

        const muse::Ret ret = runExport(*au3Project, wxFromPath(path), progress);
        m_tags = nullptr;
        if (!ret) {
            return ret;
        }
    }

    return muse::make_ok();
}

std::vector<std::string> Au3Exporter::formatsList() const
{
    std::vector<std::string> formatsList;
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        formatsList.push_back(plugin->GetFormatInfo(formatIndex).description.msgid().toStdString());
    }

    return formatsList;
}

int Au3Exporter::formatIndex(const std::string& format) const
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            return formatIndex;
        }
    }

    return -1;
}

std::vector<std::string> Au3Exporter::formatExtensions(const std::string& format) const
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            auto extensions = plugin->GetFormatInfo(formatIndex).extensions;
            if (!extensions.empty()) {
                std::vector<std::string> result;
                for (const auto& ext : extensions) {
                    result.push_back(ext.ToStdString());
                }
                return result;
            }
        }
    }

    return {};
}

std::vector<std::string> Au3Exporter::cloudPreferredAudioFormats(bool preferLossless) const
{
    const auto& registry = ExportPluginRegistry::Get();

    std::vector<std::string> result;
    for (const auto& mimeType : cloudConfiguration()->preferredAudioFormats(preferLossless)) {
        for (auto [plugin, formatIndex] : registry) {
            for (const auto& mime : plugin->GetMimeTypes(formatIndex)) {
                if (mime == mimeType) {
                    result.push_back(plugin->GetFormatInfo(formatIndex).description.msgid().toStdString());
                    break;
                }
            }
        }
    }

    if (result.empty()) {
        // Fallback to all formats if no preferred formats are found
        for (auto [plugin, formatIndex] : registry) {
            result.push_back(plugin->GetFormatInfo(formatIndex).description.msgid().toStdString());
        }
    }

    return result;
}

ExportParameters Au3Exporter::cloudExportParameters(const std::string& format) const
{
    const ExportPlugin* plugin = nullptr;
    int fmt = -1;

    for (auto [p, formatIndex] : ExportPluginRegistry::Get()) {
        if (p->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            plugin = p;
            fmt = formatIndex;
            break;
        }
    }

    if (!plugin) {
        return {};
    }

    for (const auto& mimeType : plugin->GetMimeTypes(fmt)) {
        auto config = cloudConfiguration()->exportConfig(mimeType);
        ExportProcessor::Parameters au3Params;
        if (plugin->ParseConfig(fmt, config, au3Params)) {
            ExportParameters result;
            for (const auto& [id, val] : au3Params) {
                result.emplace_back(id, std::visit([](auto v) -> OptionValue { return v; }, val));
            }
            return result;
        }
    }

    return {};
}

bool Au3Exporter::isCustomFFmpegExportFormat() const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return false;
    }

    return editor->GetName() == "custom_ffmpeg";
}

bool Au3Exporter::isOggExportFormat() const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return false;
    }

    return editor->GetName() == "ogg";
}

bool Au3Exporter::hasMetadata() const
{
    std::string format = exportConfiguration()->currentFormat();

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            return plugin->GetFormatInfo(formatIndex).canMetaData;
        }
    }

    return false;
}

int Au3Exporter::maxChannels() const
{
    std::string format = exportConfiguration()->currentFormat();

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            return plugin->GetFormatInfo(formatIndex).maxChannels;
        }
    }

    return 1;
}

std::vector<int> Au3Exporter::sampleRateList() const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return {};
    }

    return editor->GetSampleRateList();
}

int Au3Exporter::optionsCount() const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return 0;
    }

    return editor->GetOptionsCount();
}

std::optional<au::importexport::ExportOption> Au3Exporter::option(int i) const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return std::nullopt;
    }

    ::ExportOption opt;

    if (editor->GetOption(i, opt)) {
        std::string title = opt.title.translated().toStdString();
        std::vector<std::string> names;
        for (const auto& name : opt.names) {
            names.push_back(name.translated().toStdString());
        }

        return ExportOption { opt.id,
                              title,
                              opt.flags,
                              opt.values,
                              names };
    }

    return std::nullopt;
}

std::optional<au::importexport::OptionValue> Au3Exporter::value(int id) const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return std::nullopt;
    }

    ::ExportValue val;

    if (editor->GetValue(id, val)) {
        return val;
    }

    return std::nullopt;
}

void Au3Exporter::setValue(int id, const OptionValue& value)
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return;
    }

    ::ExportValue val = value;

    editor->SetValue(id, val);
    editor->Store(*gPrefs);
}

OptionsEditorUPtr Au3Exporter::optionsEditor() const
{
    std::string format = exportConfiguration()->currentFormat();

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            auto editor = plugin->CreateOptionsEditor(formatIndex, nullptr);
            if (!editor) {
                LOGE() << "error: failed to create options editor";
                return nullptr;
            }

            editor->Load(*gPrefs);

            return editor;
        }
    }

    return nullptr;
}
