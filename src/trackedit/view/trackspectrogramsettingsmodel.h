/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/view/abstractspectrogramsettingsmodel.h"
#include "spectrogram/iglobalspectrogramconfiguration.h"
#include "spectrogram/itrackspectrogramconfigurationprovider.h"
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

    muse::Inject<spectrogram::IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<spectrogram::ITrackSpectrogramConfigurationProvider> trackSpectrogramConfigurationProvider;

public:
    TrackSpectrogramSettingsModel(QObject* parent = nullptr);
    ~TrackSpectrogramSettingsModel() override;

    Q_INVOKABLE void accept();

    int trackId() const { return m_trackId; }
    void setTrackId(int value);

    QString trackTitle() const;

    bool useGlobalSettings() const;
    void setUseGlobalSettings(bool value);

    bool spectralSelectionEnabled_1() const override;
    void setSpectralSelectionEnabled_1(bool value) override;

    int colorGainDb_2() const override;
    void setColorGainDb_2(int value) override;

    int colorRangeDb_3() const override;
    void setColorRangeDb_3(int value) override;

    int colorHighBoostDbPerDec_4() const override;
    void setColorHighBoostDbPerDec_4(int value) override;

    int colorScheme_5() const override;
    void setColorScheme_5(int value) override;

    int scale_6() const override;
    void setScale_6(int value) override;

    int algorithm_7() const override;
    void setAlgorithm_7(int value) override;

    int windowType_8() const override;
    void setWindowType_8(int value) override;

    int windowSize_9() const override;
    void setWindowSize_9(int value) override;

    int zeroPaddingFactor_10() const override;
    void setZeroPaddingFactor_10(int value) override;

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
