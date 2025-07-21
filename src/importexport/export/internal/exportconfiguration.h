/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "global/iglobalconfiguration.h"

#include "../iexporter.h"

#include "../iexportconfiguration.h"

namespace au::importexport {
class ExportConfiguration : public IExportConfiguration
{
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;
    muse::Inject<au::importexport::IExporter> exporter;

public:
    ExportConfiguration() = default;

    void init();

    ExportProcessType processType() const override;
    void setProcessType(ExportProcessType process) override;
    muse::async::Notification processTypeChanged() const override;

    muse::io::path_t directoryPath() const override;
    void setDirectoryPath(const muse::io::path_t& path) override;
    muse::async::Notification directoryPathChanged() const override;

    int exportChannels() const override;
    void setExportChannels(int channels) override;
    muse::async::Notification exportChannelsChanged() const override;

    std::string currentFormat() const override;
    void setCurrentFormat(const std::string& format) override;
    muse::async::Notification currentFormatChanged() const override;

    int exportSampleRate() const override;
    void setExportSampleRate(int newRate) override;
    muse::async::Notification exportSampleRateChanged() const override;

    std::vector<std::string> exportSampleFormatList() const override;
    std::string exportSampleFormat() const override;
    void setExportSampleFormat(const std::string& format) override;
    muse::async::Notification exportSampleFormatChanged() const override;

private:

    muse::async::Notification m_processChanged;
    muse::async::Notification m_filenameChanged;
    muse::async::Notification m_directoryPathChanged;
    muse::async::Notification m_currentFormatChanged;
    muse::async::Notification m_exportChannelsChanged;
    muse::async::Notification m_exportSampleRateChanged;
    muse::async::Notification m_defaultSampleFormatChanged;
};
}
