/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractspectrogramsettingsmodel.h"
#include "spectrogram/ispectrogramconfiguration.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include <QQmlParserStatus>

namespace au::spectrogram {
class GlobalSpectrogramSettingsModel : public AbstractSpectrogramSettingsModel, public muse::async::Asyncable, public QQmlParserStatus
{
    Q_OBJECT

    muse::Inject<ISpectrogramConfiguration> configuration;

public:
    GlobalSpectrogramSettingsModel(QObject* parent = nullptr);
    ~GlobalSpectrogramSettingsModel() override = default;

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

private:
    void classBegin() override {}
    void componentComplete() override;
};
}
