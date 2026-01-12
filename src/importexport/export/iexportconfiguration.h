/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"
#include "global/async/channel.h"
#include "global/io/path.h"

#include "modularity/imoduleinterface.h"

#include "types/exporttypes.h"

namespace au::importexport {
class IExportConfiguration : MODULE_GLOBAL_EXPORT_INTERFACE
{
    INTERFACE_ID(IExportConfiguration)

public:

    virtual ~IExportConfiguration() = default;

    virtual ExportProcessType processType() const = 0;
    virtual void setProcessType(ExportProcessType process) = 0;
    virtual muse::async::Notification processTypeChanged() const = 0;

    virtual muse::io::path_t directoryPath() const = 0;
    virtual void setDirectoryPath(const muse::io::path_t& path) = 0;
    virtual muse::async::Notification directoryPathChanged() const = 0;

    virtual std::string currentFormat() const = 0;
    virtual void setCurrentFormat(const std::string& format) = 0;
    virtual muse::async::Notification currentFormatChanged() const = 0;

    virtual int exportChannels() const = 0;
    virtual void setExportChannels(int channels) = 0;
    virtual muse::async::Notification exportChannelsChanged() const = 0;

    virtual int exportSampleRate() const = 0;
    virtual void setExportSampleRate(int newRate) = 0;
    virtual muse::async::Notification exportSampleRateChanged() const = 0;

    virtual std::vector<std::string> exportSampleFormatList() const = 0;
    virtual std::string exportSampleFormat() const = 0;
    virtual void setExportSampleFormat(const std::string& format) = 0;
    virtual muse::async::Notification exportSampleFormatChanged() const = 0;

    virtual std::string ffmpegFormat() const = 0;
    virtual void setFFmpegFormat(const std::string& format) = 0;
    virtual muse::async::Notification ffmpegFormatChanged() const = 0;

    virtual std::string ffmpegCodec() const = 0;
    virtual void setFFmpegCodec(const std::string& codec) = 0;
    virtual muse::async::Notification ffmpegCodecChanged() const = 0;

    virtual std::string ffmpegLanguage() const = 0;
    virtual void setFFmpegLanguage(const std::string& language) = 0;
    virtual muse::async::Notification ffmpegLanguageChanged() const = 0;

    virtual std::string ffmpegTag() const = 0;
    virtual void setFFmpegTag(const std::string& tag) = 0;
    virtual muse::async::Notification ffmpegTagChanged() const = 0;

    virtual int ffmpegQuality() const = 0;
    virtual void setFFmpegQuality(int quality) = 0;
    virtual muse::async::Notification ffmpegQualityChanged() const = 0;

    virtual int ffmpegCutoff() const = 0;
    virtual void setFFmpegCutoff(int cutoff) = 0;
    virtual muse::async::Notification ffmpegCutoffChanged() const = 0;

    virtual int ffmpegBitrate() const = 0;
    virtual void setFFmpegBitrate(int bitrate) = 0;
    virtual muse::async::Notification ffmpegBitrateChanged() const = 0;

    virtual int ffmpegSampleRate() const = 0;
    virtual void setFFmpegSampleRate(int sampleRate) = 0;
    virtual muse::async::Notification ffmpegSampleRateChanged() const = 0;

    virtual std::string ffmpegProfile() const = 0;
    virtual void setFFmpegProfile(const std::string& profile) = 0;
    virtual muse::async::Notification ffmpegProfileChanged() const = 0;

    virtual bool ffmpegBitReservoir() const = 0;
    virtual void setFFmpegBitReservoir(bool bitReservoir) = 0;
    virtual muse::async::Notification ffmpegBitReservoirChanged() const = 0;

    virtual bool ffmpegVbl() const = 0;
    virtual void setFFmpegVbl(bool vbl) = 0;
    virtual muse::async::Notification ffmpegVblChanged() const = 0;

    virtual int ffmpegCompression() const = 0;
    virtual void setFFmpegCompression(int compression) = 0;
    virtual muse::async::Notification ffmpegCompressionChanged() const = 0;

    virtual int ffmpegLpcCoeffPrecision() const = 0;
    virtual void setFFmpegLpcCoeffPrecision(int lpc) = 0;
    virtual muse::async::Notification ffmpegLpcChanged() const = 0;

    virtual int ffmpegFrameSize() const = 0;
    virtual void setFFmpegFrameSize(int frame) = 0;
    virtual muse::async::Notification ffmpegFrameSizeChanged() const = 0;

    virtual int ffmpegPredictionOrderMethod() const = 0;
    virtual void setFFmpegPredictionOrderMethod(int method) = 0;
    virtual muse::async::Notification ffmpegPredictionOrderMethodChanged() const = 0;

    virtual int ffmpegMinPredictionOrder() const = 0;
    virtual void setFFmpegMinPredictionOrder(int minPdO) = 0;
    virtual muse::async::Notification ffmpegMinPredictionOrderChanged() const = 0;

    virtual int ffmpegMaxPredictionOrder() const = 0;
    virtual void setFFmpegMaxPredictionOrder(int maxPdO) = 0;
    virtual muse::async::Notification ffmpegMaxPredictionOrderChanged() const = 0;

    virtual int ffmpegMinPartitionOrder() const = 0;
    virtual void setFFmpegMinPartitionOrder(int minPtO) = 0;
    virtual muse::async::Notification ffmpegMinPartitionOrderChanged() const = 0;

    virtual int ffmpegMaxPartitionOrder() const = 0;
    virtual void setFFmpegMaxPartitionOrder(int maxPtO) = 0;
    virtual muse::async::Notification ffmpegMaxPartitionOrderChanged() const = 0;

    virtual bool ffmpegUseLpc() const = 0;
    virtual void setFFmpegUseLpc(bool useLpc) = 0;
    virtual muse::async::Notification ffmpegUseLpcChanged() const = 0;

    virtual int ffmpegMuxRate() const = 0;
    virtual void setFFmpegMuxRate(int muxRate) = 0;
    virtual muse::async::Notification ffmpegMuxRateChanged() const = 0;

    virtual int ffmpegPacketSize() const = 0;
    virtual void setFFmpegPacketSize(int packetSize) = 0;
    virtual muse::async::Notification ffmpegPacketSizeChanged() const = 0;

    virtual std::string defaultMetadata() const = 0;
    virtual void setDefaultMetadata(const std::string& xmlString) = 0;
    virtual muse::async::Notification defaultMetadataChanged() const = 0;
};
}
