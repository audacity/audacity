/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QQmlParserStatus>

#include "spectrogram/view/abstractspectrogramsettingsmodel.h"
#include "spectrogram/iglobalspectrogramconfiguration.h"
#include "spectrogram/ispectrogramservice.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

namespace au::spectrogram {
class TrackSpectrogramSettingsModel : public AbstractSpectrogramSettingsModel,  public QQmlParserStatus, public muse::Contextable,
    public muse::async::Asyncable
{
    Q_OBJECT
    Q_INTERFACES(QQmlParserStatus)

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged)
    Q_PROPERTY(bool useGlobalSettings READ useGlobalSettings WRITE setUseGlobalSettings NOTIFY useGlobalSettingsChanged)

    muse::GlobalInject<IGlobalSpectrogramConfiguration> globalSpectrogramConfiguration;

    muse::ContextInject<ISpectrogramService> spectrogramService { this };

public:
    TrackSpectrogramSettingsModel(QObject* parent = nullptr);
    ~TrackSpectrogramSettingsModel() override = default;

    Q_INVOKABLE void accept();
    Q_INVOKABLE void aboutToDestroy();

    int trackId() const { return m_trackId; }
    void setTrackId(int value);

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

    int m_trackId = -1;

    std::shared_ptr<ITrackSpectrogramConfiguration> m_trackConfig;
    std::unique_ptr<ITrackSpectrogramConfiguration> m_initialTrackConfig;
};
}
