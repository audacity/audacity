/*
* Audacity: A Digital Audio Editor
*/

#include "exportconfiguration.h"

#include "global/settings.h"

#include "log.h"

using namespace au::importexport;

static const std::string module_name("export");

static const muse::Settings::Key EXPORT_PROCESS(module_name, "importexport/process");
static const muse::Settings::Key EXPORT_DIRECTORY_PATH(module_name, "importexport/directoryPath");
static const muse::Settings::Key EXPORT_FORMAT(module_name, "importexport/format");
static const muse::Settings::Key EXPORT_CHANNELS(module_name, "importexport/channels");
static const muse::Settings::Key EXPORT_SAMPLE_RATE(module_name, "importexport/defaultProjectSampleRate");
static const muse::Settings::Key EXPORT_ENCODING(module_name, "importexport/encoding");

// custom FFmpeg keys
static const muse::Settings::Key FFMPEG_LANGUAGE("au3wrap", "FileFormats/FFmpegLanguage");
static const muse::Settings::Key FFMPEG_SAMPLE_RATE("au3wrap", "FileFormats/FFmpegSampleRate");
static const muse::Settings::Key FFMPEG_BIT_RATE("au3wrap", "FileFormats/FFmpegBitRate");
static const muse::Settings::Key FFMPEG_TAG("au3wrap", "FileFormats/FFmpegTag");
static const muse::Settings::Key FFMPEG_QUALITY("au3wrap", "FileFormats/FFmpegQuality");
static const muse::Settings::Key FFMPEG_PROFILE(module_name, "importexport/profile");
static const muse::Settings::Key FFMPEG_BIT_RESERVOIR("au3wrap", "FileFormats/FFmpegBitReservoir");
static const muse::Settings::Key FFMPEG_CUTOFF("au3wrap", "FileFormats/FFmpegCutOff");
static const muse::Settings::Key FFMPEG_VARIABLE_BLOCK_LEN("au3wrap", "FileFormats/FFmpegVariableBlockLen");
static const muse::Settings::Key FFMPEG_COMP_LEVEL("au3wrap", "FileFormats/FFmpegCompLevel");
static const muse::Settings::Key FFMPEG_FRAME_SIZE("au3wrap", "FileFormats/FFmpegFrameSize");
static const muse::Settings::Key FFMPEG_LPC_COEFF_PRECISION("au3wrap", "FileFormats/FFmpegLPCCoefPrec");
static const muse::Settings::Key FFMPEG_USE_LPC(module_name, "importexport/useLPC");
static const muse::Settings::Key FFMPEG_MIN_PREDICTION_ORDER("au3wrap", "FileFormats/FFmpegMinPredOrder");
static const muse::Settings::Key FFMPEG_MAX_PREDICTION_ORDER("au3wrap", "FileFormats/FFmpegMaxPredOrder");
static const muse::Settings::Key FFMPEG_MIN_PARTITION_ORDER("au3wrap", "FileFormats/FFmpegMinPartOrder");
static const muse::Settings::Key FFMPEG_MAX_PARTITION_ORDER("au3wrap", "FileFormats/FFmpegMaxPartOrder");
static const muse::Settings::Key FFMPEG_PRED_ORDER_METHOD("au3wrap", "FileFormats/FFmpegPredOrderMethod");
static const muse::Settings::Key FFMPEG_MUX_RATE("au3wrap", "FileFormats/FFmpegMuxRate");
static const muse::Settings::Key FFMPEG_PACKET_SIZE("au3wrap", "FileFormats/FFmpegPacketSize");
static const muse::Settings::Key FFMPEG_CODEC("au3wrap", "FileFormats/FFmpegCodec");
static const muse::Settings::Key FFMPEG_FORMAT("au3wrap", "FileFormats/FFmpegFormat");

void ExportConfiguration::init()
{
    muse::settings()->setDefaultValue(EXPORT_PROCESS, muse::Val(ExportProcessType::FULL_PROJECT_AUDIO));
    muse::settings()->valueChanged(EXPORT_PROCESS).onReceive(nullptr, [this] (const muse::Val& val) {
        m_processChanged.notify();
    });

    muse::settings()->setDefaultValue(EXPORT_DIRECTORY_PATH, muse::Val(globalConfiguration()->userDataPath()));
    muse::settings()->valueChanged(EXPORT_DIRECTORY_PATH).onReceive(nullptr, [this] (const muse::Val& val) {
        m_directoryPathChanged.notify();
    });

    muse::settings()->valueChanged(EXPORT_FORMAT).onReceive(nullptr, [this] (const muse::Val& val) {
        m_currentFormatChanged.notify();
    });

    muse::settings()->setDefaultValue(EXPORT_CHANNELS, muse::Val(ExportChannelsPref::ExportChannels::STEREO));
    muse::settings()->valueChanged(EXPORT_CHANNELS).onReceive(nullptr, [this] (const muse::Val& val) {
        m_exportChannelsChanged.notify();
    });

    muse::settings()->setDefaultValue(EXPORT_SAMPLE_RATE, muse::Val("44100"));
    muse::settings()->valueChanged(EXPORT_SAMPLE_RATE).onReceive(nullptr, [this] (const muse::Val& val) {
        m_exportSampleRateChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_FORMAT, muse::Val(""));
    muse::settings()->valueChanged(FFMPEG_FORMAT).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegFormatChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_CODEC, muse::Val(""));
    muse::settings()->valueChanged(FFMPEG_CODEC).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegCodecChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_LANGUAGE, muse::Val(""));
    muse::settings()->valueChanged(FFMPEG_LANGUAGE).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegLanguageChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_TAG, muse::Val(""));
    muse::settings()->valueChanged(FFMPEG_TAG).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegTagChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_QUALITY, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_QUALITY).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegQualityChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_CUTOFF, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_CUTOFF).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegCutoffChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_BIT_RATE, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_BIT_RATE).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegBitrateChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_SAMPLE_RATE, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_SAMPLE_RATE).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegSampleRateChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_PROFILE, muse::Val(""));
    muse::settings()->valueChanged(FFMPEG_PROFILE).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegProfileChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_BIT_RESERVOIR, muse::Val(true));
    muse::settings()->valueChanged(FFMPEG_BIT_RESERVOIR).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegBitReservoirChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_VARIABLE_BLOCK_LEN, muse::Val(true));
    muse::settings()->valueChanged(FFMPEG_VARIABLE_BLOCK_LEN).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegVblChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_COMP_LEVEL, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_COMP_LEVEL).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegCompressionChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_LPC_COEFF_PRECISION, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_LPC_COEFF_PRECISION).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegLpcCoeffPrecisionChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_FRAME_SIZE, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_FRAME_SIZE).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegFrameSizeChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_PRED_ORDER_METHOD, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_PRED_ORDER_METHOD).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegPredictionOrderMethodChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_MIN_PREDICTION_ORDER, muse::Val(-1));
    muse::settings()->valueChanged(FFMPEG_MIN_PREDICTION_ORDER).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegMinPredictionOrderChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_MAX_PREDICTION_ORDER, muse::Val(-1));
    muse::settings()->valueChanged(FFMPEG_MAX_PREDICTION_ORDER).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegMaxPredictionOrderChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_MIN_PARTITION_ORDER, muse::Val(-1));
    muse::settings()->valueChanged(FFMPEG_MIN_PARTITION_ORDER).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegMinPartitionOrderChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_MAX_PARTITION_ORDER, muse::Val(-1));
    muse::settings()->valueChanged(FFMPEG_MAX_PARTITION_ORDER).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegMaxPartitionOrderChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_USE_LPC, muse::Val(true));
    muse::settings()->valueChanged(FFMPEG_USE_LPC).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegUseLpcChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_MUX_RATE, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_MUX_RATE).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegMuxRateChanged.notify();
    });

    muse::settings()->setDefaultValue(FFMPEG_PACKET_SIZE, muse::Val(0));
    muse::settings()->valueChanged(FFMPEG_PACKET_SIZE).onReceive(nullptr, [this] (const muse::Val& val) {
        m_ffmpegPacketSizeChanged.notify();
    });
}

ExportProcessType ExportConfiguration::processType() const
{
    return muse::settings()->value(EXPORT_PROCESS).toEnum<ExportProcessType>();
}

void ExportConfiguration::setProcessType(ExportProcessType process)
{
    muse::settings()->setSharedValue(EXPORT_PROCESS, muse::Val(process));
}

muse::async::Notification ExportConfiguration::processTypeChanged() const
{
    return m_processChanged;
}

muse::io::path_t ExportConfiguration::directoryPath() const
{
    return muse::settings()->value(EXPORT_DIRECTORY_PATH).toString();
}

void ExportConfiguration::setDirectoryPath(const muse::io::path_t& path)
{
    muse::settings()->setSharedValue(EXPORT_DIRECTORY_PATH, muse::Val(path));
}

muse::async::Notification ExportConfiguration::directoryPathChanged() const
{
    return m_directoryPathChanged;
}

int ExportConfiguration::exportChannels() const
{
    return muse::settings()->value(EXPORT_CHANNELS).toInt();
}

void ExportConfiguration::setExportChannels(int channels)
{
    muse::settings()->setSharedValue(EXPORT_CHANNELS, muse::Val(channels));
}

muse::async::Notification ExportConfiguration::exportChannelsChanged() const
{
    return m_exportChannelsChanged;
}

std::string ExportConfiguration::currentFormat() const
{
    return muse::settings()->value(EXPORT_FORMAT).toString();
}

void ExportConfiguration::setCurrentFormat(const std::string& format)
{
    muse::settings()->setSharedValue(EXPORT_FORMAT, muse::Val(format));
}

muse::async::Notification ExportConfiguration::currentFormatChanged() const
{
    return m_currentFormatChanged;
}

int ExportConfiguration::exportSampleRate() const
{
    return muse::settings()->value(EXPORT_SAMPLE_RATE).toInt();
}

void ExportConfiguration::setExportSampleRate(int newRate)
{
    muse::settings()->setSharedValue(EXPORT_SAMPLE_RATE, muse::Val(static_cast<int>(newRate)));
}

muse::async::Notification ExportConfiguration::exportSampleRateChanged() const
{
    return m_exportSampleRateChanged;
}

std::vector<std::string> ExportConfiguration::exportSampleFormatList() const
{
    NOT_IMPLEMENTED;
    return {};
}

std::string ExportConfiguration::exportSampleFormat() const
{
    NOT_IMPLEMENTED;
    return {};
}

void ExportConfiguration::setExportSampleFormat(const std::string& format)
{
    Q_UNUSED(format);
    NOT_IMPLEMENTED;
}

muse::async::Notification ExportConfiguration::exportSampleFormatChanged() const
{
    NOT_IMPLEMENTED;
    return {};
}

std::string ExportConfiguration::ffmpegFormat() const
{
    return muse::settings()->value(FFMPEG_FORMAT).toString();
}

void ExportConfiguration::setFFmpegFormat(const std::string& format)
{
    muse::settings()->setSharedValue(FFMPEG_FORMAT, muse::Val(format));
}

muse::async::Notification ExportConfiguration::ffmpegFormatChanged() const
{
    return m_ffmpegFormatChanged;
}

std::string ExportConfiguration::ffmpegCodec() const
{
    return muse::settings()->value(FFMPEG_CODEC).toString();
}

void ExportConfiguration::setFFmpegCodec(const std::string& codec)
{
    muse::settings()->setSharedValue(FFMPEG_CODEC, muse::Val(codec));
}

muse::async::Notification ExportConfiguration::ffmpegCodecChanged() const
{
    return m_ffmpegCodecChanged;
}

std::string ExportConfiguration::ffmpegLanguage() const
{
    return muse::settings()->value(FFMPEG_LANGUAGE).toString();
}

void ExportConfiguration::setFFmpegLanguage(const std::string& language)
{
    muse::settings()->setSharedValue(FFMPEG_LANGUAGE, muse::Val(language));
}

muse::async::Notification ExportConfiguration::ffmpegLanguageChanged() const
{
    return m_ffmpegLanguageChanged;
}

std::string ExportConfiguration::ffmpegTag() const
{
    return muse::settings()->value(FFMPEG_TAG).toString();
}

void ExportConfiguration::setFFmpegTag(const std::string& tag)
{
    muse::settings()->setSharedValue(FFMPEG_TAG, muse::Val(tag));
}

muse::async::Notification ExportConfiguration::ffmpegTagChanged() const
{
    return m_ffmpegTagChanged;
}

int ExportConfiguration::ffmpegQuality() const
{
    return muse::settings()->value(FFMPEG_QUALITY).toInt();
}

void ExportConfiguration::setFFmpegQuality(int quality)
{
    muse::settings()->setSharedValue(FFMPEG_QUALITY, muse::Val(static_cast<int>(quality)));
}

muse::async::Notification ExportConfiguration::ffmpegQualityChanged() const
{
    return m_ffmpegQualityChanged;
}

int ExportConfiguration::ffmpegCutoff() const
{
    return muse::settings()->value(FFMPEG_CUTOFF).toInt();
}

void ExportConfiguration::setFFmpegCutoff(int cutoff)
{
    muse::settings()->setSharedValue(FFMPEG_CUTOFF, muse::Val(static_cast<int>(cutoff)));
}

muse::async::Notification ExportConfiguration::ffmpegCutoffChanged() const
{
    return m_ffmpegCutoffChanged;
}

int ExportConfiguration::ffmpegBitrate() const
{
    return muse::settings()->value(FFMPEG_BIT_RATE).toInt();
}

void ExportConfiguration::setFFmpegBitrate(int bitrate)
{
    muse::settings()->setSharedValue(FFMPEG_BIT_RATE, muse::Val(static_cast<int>(bitrate)));
}

muse::async::Notification ExportConfiguration::ffmpegBitrateChanged() const
{
    return m_ffmpegBitrateChanged;
}

int ExportConfiguration::ffmpegSampleRate() const
{
    return muse::settings()->value(FFMPEG_SAMPLE_RATE).toInt();
}

void ExportConfiguration::setFFmpegSampleRate(int sampleRate)
{
    muse::settings()->setSharedValue(FFMPEG_SAMPLE_RATE, muse::Val(static_cast<int>(sampleRate)));
}

muse::async::Notification ExportConfiguration::ffmpegSampleRateChanged() const
{
    return m_ffmpegSampleRateChanged;
}

std::string ExportConfiguration::ffmpegProfile() const
{
    return muse::settings()->value(FFMPEG_PROFILE).toString();
}

void ExportConfiguration::setFFmpegProfile(const std::string& profile)
{
    muse::settings()->setSharedValue(FFMPEG_PROFILE, muse::Val(profile));
}

muse::async::Notification ExportConfiguration::ffmpegProfileChanged() const
{
    return m_ffmpegProfileChanged;
}

bool ExportConfiguration::ffmpegBitReservoir() const
{
    return muse::settings()->value(FFMPEG_BIT_RESERVOIR).toBool();
}

void ExportConfiguration::setFFmpegBitReservoir(bool bitReservoir)
{
    muse::settings()->setSharedValue(FFMPEG_BIT_RESERVOIR, muse::Val(bitReservoir));
}

muse::async::Notification ExportConfiguration::ffmpegBitReservoirChanged() const
{
    return m_ffmpegBitReservoirChanged;
}

bool ExportConfiguration::ffmpegVbl() const
{
    return muse::settings()->value(FFMPEG_VARIABLE_BLOCK_LEN).toBool();
}

void ExportConfiguration::setFFmpegVbl(bool vbl)
{
    muse::settings()->setSharedValue(FFMPEG_VARIABLE_BLOCK_LEN, muse::Val(vbl));
}

muse::async::Notification ExportConfiguration::ffmpegVblChanged() const
{
    return m_ffmpegVblChanged;
}

int ExportConfiguration::ffmpegCompression() const
{
    return muse::settings()->value(FFMPEG_COMP_LEVEL).toInt();
}

void ExportConfiguration::setFFmpegCompression(int compression)
{
    muse::settings()->setSharedValue(FFMPEG_COMP_LEVEL, muse::Val(compression));
}

muse::async::Notification ExportConfiguration::ffmpegCompressionChanged() const
{
    return m_ffmpegCompressionChanged;
}

int ExportConfiguration::ffmpegLpcCoeffPrecision() const
{
    return muse::settings()->value(FFMPEG_LPC_COEFF_PRECISION).toInt();
}

void ExportConfiguration::setFFmpegLpcCoeffPrecision(int lpc)
{
    muse::settings()->setSharedValue(FFMPEG_LPC_COEFF_PRECISION, muse::Val(lpc));
}

muse::async::Notification ExportConfiguration::ffmpegLpcChanged() const
{
    return m_ffmpegLpcCoeffPrecisionChanged;
}

int ExportConfiguration::ffmpegFrameSize() const
{
    return muse::settings()->value(FFMPEG_FRAME_SIZE).toInt();
}

void ExportConfiguration::setFFmpegFrameSize(int frameSize)
{
    muse::settings()->setSharedValue(FFMPEG_FRAME_SIZE, muse::Val(frameSize));
}

muse::async::Notification ExportConfiguration::ffmpegFrameSizeChanged() const
{
    return m_ffmpegFrameSizeChanged;
}

int ExportConfiguration::ffmpegPredictionOrderMethod() const
{
    return muse::settings()->value(FFMPEG_PRED_ORDER_METHOD).toInt();
}

void ExportConfiguration::setFFmpegPredictionOrderMethod(int method)
{
    muse::settings()->setSharedValue(FFMPEG_PRED_ORDER_METHOD, muse::Val(method));
}

muse::async::Notification ExportConfiguration::ffmpegPredictionOrderMethodChanged() const
{
    return m_ffmpegPredictionOrderMethodChanged;
}

int ExportConfiguration::ffmpegMinPredictionOrder() const
{
    return muse::settings()->value(FFMPEG_MIN_PREDICTION_ORDER).toInt();
}

void ExportConfiguration::setFFmpegMinPredictionOrder(int minPdO)
{
    muse::settings()->setSharedValue(FFMPEG_MIN_PREDICTION_ORDER, muse::Val(minPdO));
}

muse::async::Notification ExportConfiguration::ffmpegMinPredictionOrderChanged() const
{
    return m_ffmpegMinPredictionOrderChanged;
}

int ExportConfiguration::ffmpegMaxPredictionOrder() const
{
    return muse::settings()->value(FFMPEG_MAX_PREDICTION_ORDER).toInt();
}

void ExportConfiguration::setFFmpegMaxPredictionOrder(int maxPdO)
{
    muse::settings()->setSharedValue(FFMPEG_MAX_PREDICTION_ORDER, muse::Val(maxPdO));
}

muse::async::Notification ExportConfiguration::ffmpegMaxPredictionOrderChanged() const
{
    return m_ffmpegMaxPredictionOrderChanged;
}

int ExportConfiguration::ffmpegMinPartitionOrder() const
{
    return muse::settings()->value(FFMPEG_MIN_PARTITION_ORDER).toInt();
}

void ExportConfiguration::setFFmpegMinPartitionOrder(int minPtO)
{
    muse::settings()->setSharedValue(FFMPEG_MIN_PARTITION_ORDER, muse::Val(minPtO));
}

muse::async::Notification ExportConfiguration::ffmpegMinPartitionOrderChanged() const
{
    return m_ffmpegMinPartitionOrderChanged;
}

int ExportConfiguration::ffmpegMaxPartitionOrder() const
{
    return muse::settings()->value(FFMPEG_MAX_PARTITION_ORDER).toInt();
}

void ExportConfiguration::setFFmpegMaxPartitionOrder(int maxPtO)
{
    muse::settings()->setSharedValue(FFMPEG_MAX_PARTITION_ORDER, muse::Val(maxPtO));
}

muse::async::Notification ExportConfiguration::ffmpegMaxPartitionOrderChanged() const
{
    return m_ffmpegMaxPartitionOrderChanged;
}

bool ExportConfiguration::ffmpegUseLpc() const
{
    return muse::settings()->value(FFMPEG_USE_LPC).toBool();
}

void ExportConfiguration::setFFmpegUseLpc(bool useLpc)
{
    muse::settings()->setSharedValue(FFMPEG_USE_LPC, muse::Val(useLpc));
}

muse::async::Notification ExportConfiguration::ffmpegUseLpcChanged() const
{
    return m_ffmpegUseLpcChanged;
}

int ExportConfiguration::ffmpegMuxRate() const
{
    return muse::settings()->value(FFMPEG_MUX_RATE).toInt();
}

void ExportConfiguration::setFFmpegMuxRate(int muxRate)
{
    muse::settings()->setSharedValue(FFMPEG_MUX_RATE, muse::Val(muxRate));
}

muse::async::Notification ExportConfiguration::ffmpegMuxRateChanged() const
{
    return m_ffmpegMuxRateChanged;
}

int ExportConfiguration::ffmpegPacketSize() const
{
    return muse::settings()->value(FFMPEG_PACKET_SIZE).toInt();
}

void ExportConfiguration::setFFmpegPacketSize(int packetSize)
{
    muse::settings()->setSharedValue(FFMPEG_PACKET_SIZE, muse::Val(packetSize));
}

muse::async::Notification ExportConfiguration::ffmpegPacketSizeChanged() const
{
    return m_ffmpegPacketSizeChanged;
}
