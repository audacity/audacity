/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iexportconfiguration.h"
#include "iexporter.h"
#include "iffmpegoptionsaccessor.h"

namespace au::importexport {
class CustomFFmpegPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::GlobalInject<IExportConfiguration> exportConfiguration;
    muse::Inject<IExporter> exporter;
    muse::Inject<IFFmpegOptionsAccessor> ffmpegOptionsAccessor;

    // formats
    Q_PROPERTY(int ffmpegFormatIndex READ ffmpegFormatIndex NOTIFY ffmpegFormatIndexChanged)
    Q_PROPERTY(QString ffmpegFormat READ ffmpegFormat NOTIFY ffmpegFormatChanged)
    Q_PROPERTY(QVariantList ffmpegFormatList READ ffmpegFormatList NOTIFY ffmpegFormatListChanged)

    // codecs
    Q_PROPERTY(int ffmpegCodecIndex READ ffmpegCodecIndex NOTIFY ffmpegCodecIndexChanged)
    Q_PROPERTY(QString ffmpegCodec READ ffmpegCodec NOTIFY ffmpegCodecChanged)
    Q_PROPERTY(QVariantList ffmpegCodecList READ ffmpegCodecList NOTIFY ffmpegCodecListChanged)

    // general options
    Q_PROPERTY(QString language READ language NOTIFY languageChanged)
    Q_PROPERTY(QString tag READ tag NOTIFY tagChanged)
    Q_PROPERTY(int quality READ quality NOTIFY qualityChanged)
    Q_PROPERTY(int cutoff READ cutoff NOTIFY cutoffChanged)
    Q_PROPERTY(int bitrate READ bitrate NOTIFY bitrateChanged)
    Q_PROPERTY(int sampleRate READ sampleRate NOTIFY sampleRateChanged)
    Q_PROPERTY(QString profile READ profile NOTIFY profileChanged);
    Q_PROPERTY(QVariantList profileList READ profileList NOTIFY profileListChanged);
    Q_PROPERTY(bool bitReservoir READ bitReservoir NOTIFY bitReservoirChanged);
    Q_PROPERTY(bool vbl READ vbl NOTIFY vblChanged);

    // FLAC options
    Q_PROPERTY(int compression READ compression NOTIFY compressionChanged);
    Q_PROPERTY(int lpc READ lpc NOTIFY lpcChanged);
    Q_PROPERTY(int minPdO READ minPdO NOTIFY minPdOChanged);
    Q_PROPERTY(int maxPdO READ maxPdO NOTIFY maxPdOChanged);
    Q_PROPERTY(int minPtO READ minPtO NOTIFY minPtOChanged);
    Q_PROPERTY(int maxPtO READ maxPtO NOTIFY maxPtOChanged);
    Q_PROPERTY(int frameSize READ frameSize NOTIFY frameSizeChanged);
    Q_PROPERTY(QString pdOMethod READ pdOMethod NOTIFY pdOMethodChanged);
    Q_PROPERTY(QVariantList pdOMethodList READ pdOMethodList NOTIFY pdOMethodListChanged);
    Q_PROPERTY(bool useLpc READ useLpc NOTIFY useLpcChanged);

    // MPEG container options
    Q_PROPERTY(int muxRate READ muxRate NOTIFY muxRateChanged);
    Q_PROPERTY(int packetSize READ packetSize NOTIFY packetSizeChanged);

public:
    explicit CustomFFmpegPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    int ffmpegFormatIndex() const;
    QString ffmpegFormat() const;
    QVariantList ffmpegFormatList() const;
    Q_INVOKABLE void setFFmpegFormat(const QString& format);
    Q_INVOKABLE void fetchAllFormats();

    int ffmpegCodecIndex() const;
    QString ffmpegCodec() const;
    QVariantList ffmpegCodecList() const;
    Q_INVOKABLE void setFFmpegCodec(const QString& codec);
    Q_INVOKABLE void fetchAllCodecs();

    QString language() const;
    Q_INVOKABLE void setLanguage(const QString& lang);

    QString tag() const;
    Q_INVOKABLE void setTag(const QString& tag);

    int quality() const;
    Q_INVOKABLE void setQuality(int quality);

    int cutoff() const;
    Q_INVOKABLE void setCutoff(int cutoff);

    int bitrate() const;
    Q_INVOKABLE void setBitrate(int bitrate);

    int sampleRate() const;
    Q_INVOKABLE void setSampleRate(int sampleRate);

    QString profile() const;
    QVariantList profileList() const;
    Q_INVOKABLE void setProfile(const QString& profile);

    bool bitReservoir() const;
    Q_INVOKABLE void setBitReservoir(bool bitReservoir);

    bool vbl() const;
    Q_INVOKABLE void setVbl(bool vbl);

    int compression() const;
    Q_INVOKABLE void setCompression(int compression);

    int lpc() const;
    Q_INVOKABLE void setLpc(int lpc);

    int minPdO() const;
    Q_INVOKABLE void setMinPdO(int minPdO);

    int maxPdO() const;
    Q_INVOKABLE void setMaxPdO(int maxPdO);

    int minPtO() const;
    Q_INVOKABLE void setMinPtO(int minPtO);

    int maxPtO() const;
    Q_INVOKABLE void setMaxPtO(int maxPtO);

    int frameSize() const;
    Q_INVOKABLE void setFrameSize(int frameSize);

    QString pdOMethod() const;
    QVariantList pdOMethodList() const;
    Q_INVOKABLE void setPdOMethod(int pdOMethodIdx);

    bool useLpc() const;
    Q_INVOKABLE void setUseLpc(bool useLpc);

    int muxRate() const;
    Q_INVOKABLE void setMuxRate(int muxRate);

    int packetSize() const;
    Q_INVOKABLE void setPacketSize(int packetSize);

signals:
    void ffmpegFormatIndexChanged();
    void ffmpegFormatChanged();
    void ffmpegFormatListChanged();
    void ffmpegCodecIndexChanged();
    void ffmpegCodecChanged();
    void ffmpegCodecListChanged();
    void languageChanged();
    void tagChanged();
    void qualityChanged();
    void cutoffChanged();
    void bitrateChanged();
    void sampleRateChanged();
    void profileChanged();
    void profileListChanged();
    void bitReservoirChanged();
    void vblChanged();
    void compressionChanged();
    void lpcChanged();
    void minPdOChanged();
    void minPtOChanged();
    void frameSizeChanged();
    void pdOMethodChanged();
    void pdOMethodListChanged();
    void maxPdOChanged();
    void maxPtOChanged();
    void useLpcChanged();
    void muxRateChanged();
    void packetSizeChanged();
};
}
