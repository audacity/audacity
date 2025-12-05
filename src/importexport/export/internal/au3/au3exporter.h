/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3-import-export/Export.h"

#include "../../iexporter.h"
#include "internal/exportconfiguration.h"
#include "trackedit/iselectioncontroller.h"
#include "playback/iplaybackcontroller.h"

namespace au::importexport {
using OptionsEditorUPtr = std::unique_ptr<ExportOptionsEditor>;

class Au3Exporter : public IExporter
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::importexport::ExportConfiguration> exportConfiguration;
    muse::Inject<au::trackedit::ISelectionController> selectionController;
    muse::Inject<au::playback::IPlaybackController> playbackController;

public:
    Au3Exporter() = default;

    void init() override;
    muse::Ret exportData(std::string filename) override;

    std::vector<std::string> formatsList() const override;
    int formatIndex(const std::string& format) const override;
    std::vector<std::string> formatExtensions(const std::string& format) const override;
    bool isCustomFFmpegExportFormat() const override;
    bool hasMetadata() const override;

    int maxChannels() const override;
    std::vector<int> sampleRateList() const override;
    int optionsCount() const override;

    std::optional<ExportOption> option(int i) const override;
    std::optional<OptionValue> value(int id) const override;
    void setValue(int id, const OptionValue&) override;

    OptionsEditorUPtr optionsEditor() const;

private:
    double m_t0 {};
    double m_t1 {};
    bool m_selectedOnly{};
    unsigned m_numChannels{ 1 };
    double m_sampleRate{ 44100 };
    ExportProcessor::Parameters m_parameters;
    const ExportPlugin* m_plugin{};
    int m_format{};
    MixerOptions::Downmix* m_mixerSpec{};
    const Tags* m_tags{};
};
}
