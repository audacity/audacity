/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/view/abstractspectrogramsettingsmodel.h"
#include "spectrogram/iglobalspectrogramconfiguration.h"
#include "spectrogram/ispectrogramservice.h"
#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

namespace au::trackedit {
class TrackSpectrogramSettingsModel : public spectrogram::AbstractSpectrogramSettingsModel, public QQmlParserStatus,
    public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged)
    Q_PROPERTY(QString trackTitle READ trackTitle NOTIFY trackIdChanged)
    Q_PROPERTY(bool useGlobalSettings READ useGlobalSettings WRITE setUseGlobalSettings NOTIFY useGlobalSettingsChanged)

    muse::GlobalInject<spectrogram::IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;

    muse::Inject<au::context::IGlobalContext> globalContext { this };
    muse::Inject<spectrogram::ISpectrogramService> spectrogramService { this };

public:
    TrackSpectrogramSettingsModel(QObject* parent = nullptr);
    ~TrackSpectrogramSettingsModel() override;

    Q_INVOKABLE void accept();

    int trackId() const { return m_trackId; }
    void setTrackId(int value);

    QString trackTitle() const;

    bool useGlobalSettings() const;
    void setUseGlobalSettings(bool value);

    int minFreq() const override;
    void doSetMinFreq(int value) override;

    int maxFreq() const override;
    void doSetMaxFreq(int value) override;

    int colorGainDb() const override;
    void setColorGainDb(int value) override;

    int colorRangeDb() const override;
    void setColorRangeDb(int value) override;

    int colorHighBoostDbPerDec() const override;
    void setColorHighBoostDbPerDec(int value) override;

    int colorScheme() const override;
    void setColorScheme(int value) override;

    int scale() const override;
    void setScale(int value) override;

    int algorithm() const override;
    void setAlgorithm(int value) override;

    int windowType() const override;
    void setWindowType(int value) override;

    int windowSize() const override;
    void setWindowSize(int value) override;

    int zeroPaddingFactor() const override;
    void setZeroPaddingFactor(int value) override;

signals:
    void trackIdChanged();
    void useGlobalSettingsChanged();

private:
    void classBegin() override {}
    void componentComplete() override;

    void onSettingChanged();
    void sendRepaintRequest();

    int m_trackId = -1;

    std::shared_ptr<spectrogram::ITrackSpectrogramConfiguration> m_trackConfig;
    std::unique_ptr<spectrogram::ITrackSpectrogramConfiguration> m_initialTrackConfig;
};
}
