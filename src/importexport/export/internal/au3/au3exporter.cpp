/*
* Audacity: A Digital Audio Editor
*/

#include "au3exporter.h"

#include "libraries/lib-basic-ui/BasicUI.h"
#include "libraries/lib-mixer/MixerOptions.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-import-export/ExportPluginRegistry.h"
#include "libraries/lib-import-export/ExportUtils.h"
#include "modules/import-export/RegisterExportPlugins.h"

#include "au3wrap/au3types.h"

#include "translation.h"

using namespace au::au3;
using namespace au::importexport;

namespace {
ExportPlugin* formatPlugin(const std::string& format)
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.Translation().ToStdString() == format) {
            return plugin;
        }
    }

    return nullptr;
}
}

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

    void SetStatusString(const TranslatableString& str) override
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
            mProgressDialog = BasicUI::MakeProgress(XO("Export"), mStatus);
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
}

muse::Ret Au3Exporter::exportData(std::string filename)
{
    muse::io::path_t directoryPath = exportConfiguration()->directoryPath();
    muse::io::path_t filePath = directoryPath.appendingComponent(filename)
                                .appendingSuffix(formatExtension(exportConfiguration()->currentFormat()));

    wxFileName wxfilename = wxString(filePath.toStdString());

    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    IF_ASSERT_FAILED(project) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    int format = formatIndex(exportConfiguration()->currentFormat());
    if (format == -1) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }
    m_format = format;

    m_plugin = formatPlugin(exportConfiguration()->currentFormat());
    if (!m_plugin) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    auto editor = m_plugin->CreateOptionsEditor(m_format, nullptr);
    editor->Load(*gPrefs);
    m_parameters = ExportUtils::ParametersFromEditor(*editor);

    // TODO: implement other ExportProcessType's selections
    if (exportConfiguration()->processType() == ExportProcessType::SELECTED_AUDIO) {
        m_t0
            = selectionController()->timeSelectionIsNotEmpty() ? selectionController()->dataSelectedStartTime() : static_cast<trackedit::
                                                                                                                              secs_t>(
                  selectionController()->leftMostSelectedClipStartTime());
        m_t1
            = selectionController()->timeSelectionIsNotEmpty() ? selectionController()->dataSelectedEndTime() : static_cast<trackedit::
                                                                                                                            secs_t>(
                  selectionController()->rightMostSelectedClipEndTime());
    } else {
        auto trackeditProject = globalContext()->currentProject()->trackeditProject();

        m_t0 = 0.0;
        m_t1 = trackeditProject->totalTime().to_double();
    }

    m_selectedOnly = false;

    auto exportedTracks = ExportUtils::FindExportWaveTracks(TrackList::Get(*project), m_selectedOnly);
    if (exportedTracks.empty()) {
        //! NOTE: All selected audio is muted
        return muse::make_ret(muse::Ret::Code::InternalError, muse::trc("export", "All selected audio is muted"));
    }

    // TODO: update when custom mapping is implemented
    if (ExportChannelsPref::ExportChannels(exportConfiguration()->exportChannels()) == ExportChannelsPref::ExportChannels::MONO) {
        m_numChannels = 1;
    } else {
        m_numChannels = 2;
    }

    m_mixerSpec = std::make_unique<MixerOptions::Downmix>(exportedTracks.size(), m_numChannels).get();
    m_sampleRate = exportConfiguration()->exportSampleRate();

    try {
        auto processor = m_plugin->CreateProcessor(m_format);
        if (!processor->Initialize(*project,
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
        DialogExportProgressDelegate delegate;
        std::thread(std::move(exportTask), std::ref(delegate)).detach();
        auto result = ExportResult::Error;
        while (f.wait_for(std::chrono::milliseconds(50)) != std::future_status::ready) {
            delegate.UpdateUI();
        }

        if (result == ExportResult::Error) {
            return muse::make_ret(muse::Ret::Code::InternalError);
        }
    } catch (const ExportException& e) {
        return muse::make_ret(muse::Ret::Code::InternalError, e.What().ToStdString());
    }

    return muse::make_ret(muse::Ret::Code::Ok);
}

std::vector<std::string> Au3Exporter::formatsList() const
{
    std::vector<std::string> formatsList;
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        formatsList.push_back(plugin->GetFormatInfo(formatIndex).description.MSGID().GET().ToStdString());
    }

    return formatsList;
}

int Au3Exporter::formatIndex(const std::string& format) const
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.MSGID().GET().ToStdString() == format) {
            return formatIndex;
        }
    }

    return -1;
}

std::string Au3Exporter::formatExtension(const std::string& format) const
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.Translation().ToStdString() == format) {
            auto extensions = plugin->GetFormatInfo(formatIndex).extensions;
            if (!extensions.empty()) {
                return extensions.front().ToStdString();
            }
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

int Au3Exporter::maxChannels() const
{
    std::string format = exportConfiguration()->currentFormat();

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.Translation().ToStdString() == format) {
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
        std::string title = opt.title.Translation().ToStdString();
        std::vector<std::string> names;
        for (const auto& name : opt.names) {
            names.push_back(name.Translation().ToStdString());
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
        if (plugin->GetFormatInfo(formatIndex).description.Translation().ToStdString() == format) {
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
