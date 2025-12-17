/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractspectrogramsettingsmodel.h"
#include "spectrogram/iglobalspectrogramconfiguration.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include <QQmlParserStatus>

namespace au::spectrogram {
class GlobalSpectrogramSettingsModel : public AbstractSpectrogramSettingsModel, public muse::async::Asyncable, public QQmlParserStatus
{
    Q_OBJECT
    Q_PROPERTY(
        bool spectralSelectionEnabled READ spectralSelectionEnabled WRITE setSpectralSelectionEnabled NOTIFY spectralSelectionEnabledChanged)

    muse::Inject<IGlobalSpectrogramConfiguration> configuration;

public:
    GlobalSpectrogramSettingsModel(QObject* parent = nullptr);
    ~GlobalSpectrogramSettingsModel() override = default;

    bool spectralSelectionEnabled() const;
    void setSpectralSelectionEnabled(bool value);

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
    void spectralSelectionEnabledChanged();

private:
    void classBegin() override {}
    void componentComplete() override;
};
}
