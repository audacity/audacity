/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "au3cloud/iau3cloudconfiguration.h"

#include "au3-import-export/Export.h"
#include "au3-mixer/MixerOptions.h"
#include "au3wrap/au3types.h"

#include "../../iexporter.h"
#include "internal/exportconfiguration.h"
#include "trackedit/iselectioncontroller.h"
#include "playback/iplaybackcontroller.h"

namespace au::importexport {
using OptionsEditorUPtr = std::unique_ptr<ExportOptionsEditor>;

class Au3Exporter : public IExporter, public muse::Contextable
{
    muse::GlobalInject<au::importexport::ExportConfiguration> exportConfiguration;
    muse::GlobalInject<au::au3cloud::IAu3CloudConfiguration> cloudConfiguration;

    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<au::trackedit::ISelectionController> selectionController{ this };
    muse::ContextInject<au::playback::IPlaybackController> playbackController{ this };

public:
    Au3Exporter(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init() override;
    muse::Ret exportData(const muse::io::path_t& path, const Options& options = {}, muse::ProgressPtr progress = nullptr,
                         au::project::IAudacityProjectPtr project = nullptr) override;

    std::vector<std::string> separateFileNames(const Options& options = {}) const override;
    muse::Ret exportSeparateFiles(const muse::io::path_t& directory, const Options& options = {},
                                  muse::ProgressPtr progress = nullptr) override;

    std::vector<std::string> formatsList() const override;
    int formatIndex(const std::string& format) const override;
    std::vector<std::string> formatExtensions(const std::string& format) const override;
    std::vector<std::string> cloudPreferredAudioFormats(bool preferLossless) const override;
    ExportParameters cloudExportParameters(const std::string& format) const override;
    bool isCustomFFmpegExportFormat() const override;
    bool isOggExportFormat() const override;
    bool hasMetadata() const override;

    int maxChannels() const override;
    std::vector<int> sampleRateList() const override;
    int optionsCount() const override;

    std::optional<ExportOption> option(int i) const override;
    std::optional<OptionValue> value(int id) const override;
    void setValue(int id, const OptionValue&) override;

    OptionsEditorUPtr optionsEditor() const;

private:
    struct SeparateFile {
        std::string name;
        std::string title;
        int number = 0;
        ::WaveTrack* track = nullptr;
        double t0 = 0.0;
        double t1 = 0.0;
    };

    muse::Ret prepareFormat(const Options& options);
    muse::Ret prepareMix(au::au3::Au3Project& project, const Options& options);
    std::string formatExtension(const Options& options) const;
    std::vector<SeparateFile> separateFiles(au::au3::Au3Project& project, const Options& options) const;
    std::vector<SeparateFile> trackFiles(au::au3::Au3Project& project, const std::string& prefix, bool includeNumbers) const;
    std::vector<SeparateFile> labelFiles(const std::string& prefix, bool includeNumbers, bool includeAudioBeforeFirstLabel) const;
    muse::Ret runExport(au::au3::Au3Project& project, const wxFileName& filename, muse::ProgressPtr progress);

    double m_t0 {};
    double m_t1 {};
    bool m_selectedOnly{};
    unsigned m_numChannels{ 1 };
    double m_sampleRate{ 44100 };
    ExportProcessor::Parameters m_parameters;
    const ExportPlugin* m_plugin{};
    int m_format{};
    std::unique_ptr<MixerOptions::Downmix> m_downMix;
    MixerOptions::Downmix* m_mixerSpec{};
    const Tags* m_tags{};
};
}
