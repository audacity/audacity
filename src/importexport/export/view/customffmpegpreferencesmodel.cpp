/*
 * Audacity: A Digital Audio Editor
 */
#include <QtCore/QFileInfo>

#include "customffmpegpreferencesmodel.h"

#include "translation.h"

using namespace au::importexport;

CustomFFmpegPreferencesModel::CustomFFmpegPreferencesModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void CustomFFmpegPreferencesModel::init()
{
    exportConfiguration()->ffmpegFormatChanged().onNotify(this, [this] {
        emit ffmpegFormatChanged();
    });

    exportConfiguration()->ffmpegCodecChanged().onNotify(this, [this] {
        emit ffmpegCodecChanged();
    });

    exportConfiguration()->ffmpegLanguageChanged().onNotify(this, [this] {
        emit languageChanged();
    });

    exportConfiguration()->ffmpegTagChanged().onNotify(this, [this] {
        emit tagChanged();
    });

    exportConfiguration()->ffmpegQualityChanged().onNotify(this, [this] {
        emit qualityChanged();
    });

    exportConfiguration()->ffmpegCutoffChanged().onNotify(this, [this] {
        emit cutoffChanged();
    });

    exportConfiguration()->ffmpegBitrateChanged().onNotify(this, [this] {
        emit bitrateChanged();
    });

    exportConfiguration()->ffmpegSampleRateChanged().onNotify(this, [this] {
        emit sampleRateChanged();
    });

    exportConfiguration()->ffmpegProfileChanged().onNotify(this, [this] {
        emit profileChanged();
    });

    exportConfiguration()->ffmpegBitReservoirChanged().onNotify(this, [this] {
        emit bitReservoirChanged();
    });

    exportConfiguration()->ffmpegVblChanged().onNotify(this, [this] {
        emit vblChanged();
    });

    exportConfiguration()->ffmpegCompressionChanged().onNotify(this, [this] {
        emit compressionChanged();
    });

    exportConfiguration()->ffmpegLpcChanged().onNotify(this, [this] {
        emit lpcChanged();
    });

    exportConfiguration()->ffmpegFrameSizeChanged().onNotify(this, [this] {
        emit frameSizeChanged();
    });

    exportConfiguration()->ffmpegPredictionOrderMethodChanged().onNotify(this, [this] {
        emit pdOMethodChanged();
    });

    exportConfiguration()->ffmpegMinPredictionOrderChanged().onNotify(this, [this] {
        emit minPdOChanged();
    });

    exportConfiguration()->ffmpegMaxPredictionOrderChanged().onNotify(this, [this] {
        emit maxPdOChanged();
    });

    exportConfiguration()->ffmpegMinPartitionOrderChanged().onNotify(this, [this] {
        emit minPtOChanged();
    });

    exportConfiguration()->ffmpegMaxPartitionOrderChanged().onNotify(this, [this] {
        emit maxPtOChanged();
    });

    exportConfiguration()->ffmpegUseLpcChanged().onNotify(this, [this] {
        emit useLpcChanged();
    });

    exportConfiguration()->ffmpegMuxRateChanged().onNotify(this, [this] {
        emit muxRateChanged();
    });

    exportConfiguration()->ffmpegPacketSizeChanged().onNotify(this, [this] {
        emit packetSizeChanged();
    });
}

QString CustomFFmpegPreferencesModel::ffmpegVersion() const
{
    return QString::fromStdString(ffmpegOptionsAccessor()->ffmpegVersion());
}

QString CustomFFmpegPreferencesModel::ffmpegLibraryPath() const
{
    std::string path = ffmpegOptionsAccessor()->ffmpegLibraryPath();

    if (path.empty()) {
        return QString{};
    }
    return QString::fromStdString(path);
}

void CustomFFmpegPreferencesModel::setFFmpegLibraryPath(const QString& path)
{
    const QFileInfo fi(path);

    if (!fi.exists()) {
        interactive()->error(
            muse::trc("preferences", "Error"),
            muse::trc("preferences", "The selected path does not exist.")
            );
        return;
    }

    if (!path.contains("avformat")) {
        interactive()->error(
            muse::trc("preferences", "Error"),
            muse::qtrc("preferences", "Please select a path that contains %1").arg(avformatString()).toStdString()
            );
        return;
    }

    ffmpegOptionsAccessor()->setFFmpegLibraryPath(path);

    interactive()->info(muse::trc("preferences", "Success"),
                        muse::trc("preferences", "Please restart the application for the changes to take effect."));
}

void CustomFFmpegPreferencesModel::locateFFmpegLibrary()
{
    QString libraryPath = ffmpegLibraryPath();
    if (!libraryPath.isEmpty()) {
        auto ret = interactive()->questionSync(muse::trc("preferences", "Success"),
                                               muse::trc(
                                                   "preferences",
                                                   "Audacity already has detected a valid FFmpeg version. Do you want to choose another FFmpeg installation instead?"),
        {
            muse::IInteractive::ButtonData(
                muse::IInteractive::Button::Cancel, muse::trc("preferences", "Cancel"),
                false),
            muse::IInteractive::ButtonData(
                muse::IInteractive::Button::Apply, muse::trc("preferences", "Change FFmpeg"), true)
        }
                                               );

        if (ret.standardButton() != muse::IInteractive::Button::Apply) {
            return;
        }
    }

    muse::io::path_t initialPath;
    if (!libraryPath.isEmpty()) {
        initialPath = libraryPath;
    }

    if (initialPath.empty()) {
        initialPath = globalConfiguration()->userDataPath();
    }

    std::string libFileName = avformatString().toStdString();
    const std::string filter
        =libFileName.replace(libFileName.find('.'), 0, "*");
    muse::io::path_t directory = interactive()->selectOpeningFileSync(muse::qtrc("preferences",
                                                                                 "Locate %1").arg(
                                                                          libFileName).toStdString(), initialPath, { filter });

    if (directory.empty()) {
        return;
    }

    ffmpegOptionsAccessor()->setFFmpegLibraryPath(directory);

    interactive()->info(muse::trc("preferences", "Success"),
                        muse::trc("preferences", "Please restart the application for the changes to take effect."));
}

QString CustomFFmpegPreferencesModel::avformatString() const
{
#if defined(__WXMSW__)
    return "avformat.dll";
#elif defined(__WXMAC__)
    return "libavformat.dylib";
#else
    return "libavformat.so";
#endif
}

int CustomFFmpegPreferencesModel::ffmpegFormatIndex() const
{
    const auto& formats = ffmpegOptionsAccessor()->formatList();
    for (size_t i = 0; i < formats.size(); ++i) {
        if (formats[i] == exportConfiguration()->ffmpegFormat()) {
            return static_cast<int>(i);
        }
    }

    return -1;
}

QString CustomFFmpegPreferencesModel::ffmpegFormat() const
{
    QString format = QString::fromStdString(exportConfiguration()->ffmpegFormat());
    if (format.isEmpty()
        || !muse::contains(ffmpegOptionsAccessor()->formatList(), exportConfiguration()->ffmpegFormat())) {
        QVariantList formatList = ffmpegFormatList();
        if (formatList.empty()) {
            return "";
        }
        format = formatList.front().toString();
    }

    return format;
}

QVariantList CustomFFmpegPreferencesModel::ffmpegFormatList() const
{
    QVariantList result;
    for (const auto& format : ffmpegOptionsAccessor()->formatList()) {
        result << QString::fromStdString(format);
    }

    return result;
}

void CustomFFmpegPreferencesModel::setFFmpegFormat(const QString& val)
{
    if (val != QString::fromStdString(exportConfiguration()->ffmpegFormat())) {
        exportConfiguration()->setFFmpegFormat(val.toStdString());
        emit ffmpegFormatIndexChanged();

        // TODO: it should change codec as well if it doesn't match (AU3 doesn't do that, bug?)
    }

    //! NOTE: we want to filter codec list anyway
    ffmpegOptionsAccessor()->fetchCompatibleCodecList(exportConfiguration()->ffmpegFormat(), exportConfiguration()->ffmpegCodec());
    emit ffmpegCodecListChanged();
    emit ffmpegCodecIndexChanged();
}

void CustomFFmpegPreferencesModel::fetchAllFormats()
{
    ffmpegOptionsAccessor()->fetchAllFormats();
    emit ffmpegFormatListChanged();
    emit ffmpegFormatIndexChanged();
}

int CustomFFmpegPreferencesModel::ffmpegCodecIndex() const
{
    const auto codecs = ffmpegOptionsAccessor()->codecList();
    for (size_t i = 0; i < codecs.size(); ++i) {
        if (codecs[i] == exportConfiguration()->ffmpegCodec()) {
            return static_cast<int>(i);
        }
    }

    return -1;
}

QString CustomFFmpegPreferencesModel::ffmpegCodec() const
{
    QString codec = QString::fromStdString(exportConfiguration()->ffmpegCodec());
    if (codec.isEmpty()
        || !muse::contains(ffmpegOptionsAccessor()->codecList(), exportConfiguration()->ffmpegCodec())) {
        QVariantList codecList = ffmpegCodecList();
        if (codecList.empty()) {
            return "";
        }
        codec = codecList.front().toString();
    }

    return codec;
}

QVariantList CustomFFmpegPreferencesModel::ffmpegCodecList() const
{
    QVariantList result;
    for (const auto& codec : ffmpegOptionsAccessor()->codecList()) {
        result << QString::fromStdString(codec);
    }

    return result;
}

void CustomFFmpegPreferencesModel::setFFmpegCodec(const QString& val)
{
    if (val != QString::fromStdString(exportConfiguration()->ffmpegCodec())) {
        exportConfiguration()->setFFmpegCodec(val.toStdString());
        emit ffmpegCodecIndexChanged();

        // TODO: it should change format as well if it doesn't match (AU3 doesn't do that, bug?)
    }

    //! NOTE: we want to filter format list anyway
    ffmpegOptionsAccessor()->fetchCompatibleFormatList(exportConfiguration()->ffmpegFormat(), exportConfiguration()->ffmpegCodec());
    emit ffmpegFormatListChanged();
    emit ffmpegFormatIndexChanged();
}

void CustomFFmpegPreferencesModel::fetchAllCodecs()
{
    ffmpegOptionsAccessor()->fetchAllCodecs();
    emit ffmpegCodecListChanged();
    emit ffmpegCodecIndexChanged();
}

QString CustomFFmpegPreferencesModel::language() const
{
    return QString::fromStdString(exportConfiguration()->ffmpegLanguage());
}

void CustomFFmpegPreferencesModel::setLanguage(const QString& val)
{
    if (val == language()) {
        return;
    }

    exportConfiguration()->setFFmpegLanguage(val.toStdString());
}

QString CustomFFmpegPreferencesModel::tag() const
{
    return QString::fromStdString(exportConfiguration()->ffmpegTag());
}

void CustomFFmpegPreferencesModel::setTag(const QString& val)
{
    if (val == tag()) {
        return;
    }

    exportConfiguration()->setFFmpegTag(val.toStdString());
}

int CustomFFmpegPreferencesModel::quality() const
{
    return exportConfiguration()->ffmpegQuality();
}

void CustomFFmpegPreferencesModel::setQuality(int val)
{
    if (val == quality()) {
        return;
    }

    exportConfiguration()->setFFmpegQuality(val);
}

int CustomFFmpegPreferencesModel::cutoff() const
{
    return exportConfiguration()->ffmpegCutoff();
}

void CustomFFmpegPreferencesModel::setCutoff(int val)
{
    if (val == cutoff()) {
        return;
    }

    exportConfiguration()->setFFmpegCutoff(val);
}

int CustomFFmpegPreferencesModel::bitrate() const
{
    return exportConfiguration()->ffmpegBitrate();
}

void CustomFFmpegPreferencesModel::setBitrate(int val)
{
    if (val == bitrate()) {
        return;
    }

    exportConfiguration()->setFFmpegBitrate(val);
}

int CustomFFmpegPreferencesModel::sampleRate() const
{
    return exportConfiguration()->ffmpegSampleRate();
}

void CustomFFmpegPreferencesModel::setSampleRate(int val)
{
    if (val == sampleRate()) {
        return;
    }

    exportConfiguration()->setFFmpegSampleRate(val);
}

QString CustomFFmpegPreferencesModel::profile() const
{
    QString profile = QString::fromStdString(exportConfiguration()->ffmpegProfile());
    if (profile.isEmpty()
        || !muse::contains(ffmpegOptionsAccessor()->profileList(), exportConfiguration()->ffmpegProfile())) {
        profile = profileList().front().toString();
    }

    return profile;
}

QVariantList CustomFFmpegPreferencesModel::profileList() const
{
    QVariantList result;
    for (const auto& format : ffmpegOptionsAccessor()->profileList()) {
        result << QString::fromStdString(format);
    }

    return result;
}

void CustomFFmpegPreferencesModel::setProfile(const QString& val)
{
    if (val == profile()) {
        return;
    }

    exportConfiguration()->setFFmpegProfile(val.toStdString());
}

bool CustomFFmpegPreferencesModel::bitReservoir() const
{
    return exportConfiguration()->ffmpegBitReservoir();
}

void CustomFFmpegPreferencesModel::setBitReservoir(bool val)
{
    if (val == bitReservoir()) {
        return;
    }

    exportConfiguration()->setFFmpegBitReservoir(val);
}

bool CustomFFmpegPreferencesModel::vbl() const
{
    return exportConfiguration()->ffmpegVbl();
}

void CustomFFmpegPreferencesModel::setVbl(bool val)
{
    if (val == vbl()) {
        return;
    }

    exportConfiguration()->setFFmpegVbl(val);
}

int CustomFFmpegPreferencesModel::compression() const
{
    return exportConfiguration()->ffmpegCompression();
}

void CustomFFmpegPreferencesModel::setCompression(int val)
{
    if (val == compression()) {
        return;
    }

    exportConfiguration()->setFFmpegCompression(val);
}

int CustomFFmpegPreferencesModel::lpc() const
{
    return exportConfiguration()->ffmpegLpcCoeffPrecision();
}

void CustomFFmpegPreferencesModel::setLpc(int val)
{
    if (val == lpc()) {
        return;
    }

    exportConfiguration()->setFFmpegLpcCoeffPrecision(val);
}

int CustomFFmpegPreferencesModel::minPdO() const
{
    return exportConfiguration()->ffmpegMinPredictionOrder();
}

void CustomFFmpegPreferencesModel::setMinPdO(int val)
{
    if (val == minPdO()) {
        return;
    }

    exportConfiguration()->setFFmpegMinPredictionOrder(val);
}

int CustomFFmpegPreferencesModel::maxPdO() const
{
    return exportConfiguration()->ffmpegMaxPredictionOrder();
}

void CustomFFmpegPreferencesModel::setMaxPdO(int val)
{
    if (val == maxPdO()) {
        return;
    }

    exportConfiguration()->setFFmpegMaxPredictionOrder(val);
}

int CustomFFmpegPreferencesModel::minPtO() const
{
    return exportConfiguration()->ffmpegMinPartitionOrder();
}

void CustomFFmpegPreferencesModel::setMinPtO(int val)
{
    if (val == minPtO()) {
        return;
    }

    exportConfiguration()->setFFmpegMinPartitionOrder(val);
}

int CustomFFmpegPreferencesModel::maxPtO() const
{
    return exportConfiguration()->ffmpegMaxPartitionOrder();
}

void CustomFFmpegPreferencesModel::setMaxPtO(int val)
{
    if (val == maxPtO()) {
        return;
    }

    exportConfiguration()->setFFmpegMaxPartitionOrder(val);
}

int CustomFFmpegPreferencesModel::frameSize() const
{
    return exportConfiguration()->ffmpegFrameSize();
}

void CustomFFmpegPreferencesModel::setFrameSize(int val)
{
    if (val == frameSize()) {
        return;
    }

    exportConfiguration()->setFFmpegFrameSize(val);
}

QString CustomFFmpegPreferencesModel::pdOMethod() const
{
    int methodIndex = exportConfiguration()->ffmpegPredictionOrderMethod();
    if (methodIndex >= static_cast<int>(ffmpegOptionsAccessor()->predictionOrderMethodList().size())) {
        //! NOTE: index of default "Full search" method
        methodIndex = 4;
    }

    return QString::fromStdString(ffmpegOptionsAccessor()->predictionOrderMethodList().at(methodIndex));
}

QVariantList CustomFFmpegPreferencesModel::pdOMethodList() const
{
    QVariantList result;
    for (const auto& format : ffmpegOptionsAccessor()->predictionOrderMethodList()) {
        result << QString::fromStdString(format);
    }

    return result;
}

void CustomFFmpegPreferencesModel::setPdOMethod(int pdOMethodIdx)
{
    exportConfiguration()->setFFmpegPredictionOrderMethod(pdOMethodIdx);
}

bool CustomFFmpegPreferencesModel::useLpc() const
{
    return exportConfiguration()->ffmpegUseLpc();
}

void CustomFFmpegPreferencesModel::setUseLpc(bool val)
{
    if (val == useLpc()) {
        return;
    }

    exportConfiguration()->setFFmpegUseLpc(val);
}

int CustomFFmpegPreferencesModel::muxRate() const
{
    return exportConfiguration()->ffmpegMuxRate();
}

void CustomFFmpegPreferencesModel::setMuxRate(int val)
{
    if (val == muxRate()) {
        return;
    }

    exportConfiguration()->setFFmpegMuxRate(val);
}

int CustomFFmpegPreferencesModel::packetSize() const
{
    return exportConfiguration()->ffmpegPacketSize();
}

void CustomFFmpegPreferencesModel::setPacketSize(int val)
{
    if (val == packetSize()) {
        return;
    }

    exportConfiguration()->setFFmpegPacketSize(val);
}
