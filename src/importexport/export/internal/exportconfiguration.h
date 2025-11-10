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

    muse::io::path_t labelsDirectoryPath() const override;
    void setLabelsDirectoryPath(const muse::io::path_t& path) override;

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

    std::string ffmpegFormat() const override;
    void setFFmpegFormat(const std::string& format) override;
    muse::async::Notification ffmpegFormatChanged() const override;

    std::string ffmpegCodec() const override;
    void setFFmpegCodec(const std::string& codec) override;
    muse::async::Notification ffmpegCodecChanged() const override;

    std::string ffmpegLanguage() const override;
    void setFFmpegLanguage(const std::string& language) override;
    muse::async::Notification ffmpegLanguageChanged() const override;

    std::string ffmpegTag() const override;
    void setFFmpegTag(const std::string& tag) override;
    muse::async::Notification ffmpegTagChanged() const override;

    int ffmpegQuality() const override;
    void setFFmpegQuality(int quality) override;
    muse::async::Notification ffmpegQualityChanged() const override;

    int ffmpegCutoff() const override;
    void setFFmpegCutoff(int cutoff) override;
    muse::async::Notification ffmpegCutoffChanged() const override;

    int ffmpegBitrate() const override;
    void setFFmpegBitrate(int bitrate) override;
    muse::async::Notification ffmpegBitrateChanged() const override;

    int ffmpegSampleRate() const override;
    void setFFmpegSampleRate(int sampleRate) override;
    muse::async::Notification ffmpegSampleRateChanged() const override;

    std::string ffmpegProfile() const override;
    void setFFmpegProfile(const std::string& profile) override;
    muse::async::Notification ffmpegProfileChanged() const override;

    bool ffmpegBitReservoir() const override;
    void setFFmpegBitReservoir(bool bitReservoir) override;
    muse::async::Notification ffmpegBitReservoirChanged() const override;

    bool ffmpegVbl() const override;
    void setFFmpegVbl(bool vbl) override;
    muse::async::Notification ffmpegVblChanged() const override;

    int ffmpegCompression() const override;
    void setFFmpegCompression(int compression) override;
    muse::async::Notification ffmpegCompressionChanged() const override;

    int ffmpegLpcCoeffPrecision() const override;
    void setFFmpegLpcCoeffPrecision(int lpc) override;
    muse::async::Notification ffmpegLpcChanged() const override;

    int ffmpegFrameSize() const override;
    void setFFmpegFrameSize(int frameSize) override;
    muse::async::Notification ffmpegFrameSizeChanged() const override;

    int ffmpegPredictionOrderMethod() const override;
    void setFFmpegPredictionOrderMethod(int method) override;
    muse::async::Notification ffmpegPredictionOrderMethodChanged() const override;

    int ffmpegMinPredictionOrder() const override;
    void setFFmpegMinPredictionOrder(int minPdO) override;
    muse::async::Notification ffmpegMinPredictionOrderChanged() const override;

    int ffmpegMaxPredictionOrder() const override;
    void setFFmpegMaxPredictionOrder(int maxPdO) override;
    muse::async::Notification ffmpegMaxPredictionOrderChanged() const override;

    int ffmpegMinPartitionOrder() const override;
    void setFFmpegMinPartitionOrder(int minPtO) override;
    muse::async::Notification ffmpegMinPartitionOrderChanged() const override;

    int ffmpegMaxPartitionOrder() const override;
    void setFFmpegMaxPartitionOrder(int maxPtO) override;
    muse::async::Notification ffmpegMaxPartitionOrderChanged() const override;

    bool ffmpegUseLpc() const override;
    void setFFmpegUseLpc(bool useLpc) override;
    muse::async::Notification ffmpegUseLpcChanged() const override;

    int ffmpegMuxRate() const override;
    void setFFmpegMuxRate(int muxRate) override;
    muse::async::Notification ffmpegMuxRateChanged() const override;

    int ffmpegPacketSize() const override;
    void setFFmpegPacketSize(int packetSize) override;
    muse::async::Notification ffmpegPacketSizeChanged() const override;

    std::string defaultMetadata() const override;
    void setDefaultMetadata(const std::string& xmlString) override;
    muse::async::Notification defaultMetadataChanged() const override;

private:

    muse::async::Notification m_processChanged;
    muse::async::Notification m_filenameChanged;
    muse::async::Notification m_directoryPathChanged;
    muse::async::Notification m_currentFormatChanged;
    muse::async::Notification m_exportChannelsChanged;
    muse::async::Notification m_exportSampleRateChanged;
    muse::async::Notification m_defaultSampleFormatChanged;

    muse::async::Notification m_ffmpegFormatChanged;
    muse::async::Notification m_ffmpegCodecChanged;
    muse::async::Notification m_ffmpegLanguageChanged;
    muse::async::Notification m_ffmpegTagChanged;
    muse::async::Notification m_ffmpegQualityChanged;
    muse::async::Notification m_ffmpegCutoffChanged;
    muse::async::Notification m_ffmpegBitrateChanged;
    muse::async::Notification m_ffmpegSampleRateChanged;
    muse::async::Notification m_ffmpegProfileChanged;
    muse::async::Notification m_ffmpegBitReservoirChanged;
    muse::async::Notification m_ffmpegVblChanged;
    muse::async::Notification m_ffmpegCompressionChanged;
    muse::async::Notification m_ffmpegLpcCoeffPrecisionChanged;
    muse::async::Notification m_ffmpegFrameSizeChanged;
    muse::async::Notification m_ffmpegPredictionOrderMethodChanged;
    muse::async::Notification m_ffmpegMinPredictionOrderChanged;
    muse::async::Notification m_ffmpegMaxPredictionOrderChanged;
    muse::async::Notification m_ffmpegMinPartitionOrderChanged;
    muse::async::Notification m_ffmpegMaxPartitionOrderChanged;
    muse::async::Notification m_ffmpegUseLpcChanged;
    muse::async::Notification m_ffmpegMuxRateChanged;
    muse::async::Notification m_ffmpegPacketSizeChanged;

    muse::async::Notification m_defaultMetadataChanged;
};
}
