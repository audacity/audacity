/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogram/view/abstractspectrogramsettingsmodel.h"
#include "spectrogram/ispectrogramconfiguration.h"
#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

#include <optional>

namespace au::trackedit {
class Au3TrackSpectrogramConfiguration;
class SpectrogramConfigurationSnapshot;

class TrackSpectrogramSettingsModel : public spectrogram::AbstractSpectrogramSettingsModel, public QQmlParserStatus,
    public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged)
    Q_PROPERTY(QString trackTitle READ trackTitle NOTIFY trackIdChanged)
    Q_PROPERTY(bool useGlobalSettings READ useGlobalSettings WRITE setUseGlobalSettings NOTIFY useGlobalSettingsChanged)

    muse::Inject<spectrogram::ISpectrogramConfiguration> globalSpectrogramConfiguration;
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    TrackSpectrogramSettingsModel(QObject* parent = nullptr);
    ~TrackSpectrogramSettingsModel() override;

    Q_INVOKABLE void preview();
    Q_INVOKABLE void apply();

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
    void doSetUseGlobalSettings(bool value);
    void doSetSpectralSelectionEnabled_1(bool value, bool resetUseGlobalSettings);
    void doSetColorGainDb_2(int value, bool resetUseGlobalSettings);
    void doSetColorRangeDb_3(int value, bool resetUseGlobalSettings);
    void doSetColorHighBoostDbPerDec_4(int value, bool resetUseGlobalSettings);
    void doSetColorScheme_5(int value, bool resetUseGlobalSettings);
    void doSetScale_6(int value, bool resetUseGlobalSettings);
    void doSetAlgorithm_7(int value, bool resetUseGlobalSettings);
    void doSetWindowType_8(int value, bool resetUseGlobalSettings);
    void doSetWindowSize_9(int value, bool resetUseGlobalSettings);
    void doSetZeroPaddingFactor_10(int value, bool resetUseGlobalSettings);

    void classBegin() override {}
    void componentComplete() override;

    void readFromConfig(const spectrogram::ISpectrogramConfiguration&);
    void writeToConfig(spectrogram::ISpectrogramConfiguration&);
    void sendRepaintRequest();

    int m_trackId = -1;
    bool m_useGlobalSettings = false;
    bool m_spectralSelectionEnabled_1 = false;
    int m_colorGainDb_2 = 0;
    int m_colorRangeDb_3 = 0;
    int m_colorHighBoostDbPerDec_4 = 0;
    spectrogram::SpectrogramColorScheme m_colorScheme_5 = static_cast<spectrogram::SpectrogramColorScheme>(0);
    spectrogram::SpectrogramScale m_scale_6 = static_cast<spectrogram::SpectrogramScale>(0);
    spectrogram::SpectrogramAlgorithm m_algorithm_7 = static_cast<spectrogram::SpectrogramAlgorithm>(0);
    spectrogram::SpectrogramWindowType m_windowType_8 = static_cast<spectrogram::SpectrogramWindowType>(0);
    int m_windowSize_9 = 0;
    int m_zeroPaddingFactor_10 = 0;

    std::unique_ptr<spectrogram::ISpectrogramConfiguration> m_configToRevertTo;
};
}
