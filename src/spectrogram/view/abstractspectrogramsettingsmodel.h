/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QAbstractListModel>
#include <QObject>

namespace au::spectrogram {
class AbstractSpectrogramSettingsModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(
        bool spectralSelectionEnabled READ spectralSelectionEnabled_1 WRITE setSpectralSelectionEnabled_1 NOTIFY spectralSelectionEnabledChanged_1)

    Q_PROPERTY(int colorGainDb READ colorGainDb_2 WRITE setColorGainDb_2 NOTIFY colorGainDbChanged_2)

    Q_PROPERTY(int colorRangeDb READ colorRangeDb_3 WRITE setColorRangeDb_3 NOTIFY colorRangeDbChanged_3)
    Q_PROPERTY(int colorRangeDbMin READ colorRangeDbMin CONSTANT)

    Q_PROPERTY(
        int colorHighBoostDbPerDec READ colorHighBoostDbPerDec_4 WRITE setColorHighBoostDbPerDec_4 NOTIFY colorHighBoostDbPerDecChanged_4)
    Q_PROPERTY(int colorHighBoostDbPerDecMin READ colorHighBoostDbPerDecMin CONSTANT)
    Q_PROPERTY(int colorHighBoostDbPerDecMax READ colorHighBoostDbPerDecMax CONSTANT)

    Q_PROPERTY(int colorScheme READ colorScheme_5 WRITE setColorScheme_5 NOTIFY colorSchemeChanged_5)
    Q_PROPERTY(QList<QString> colorSchemeNames READ colorSchemeNames CONSTANT)

    Q_PROPERTY(int scale READ scale_6 WRITE setScale_6 NOTIFY scaleChanged_6)
    Q_PROPERTY(QList<QString> scaleNames READ scaleNames CONSTANT)

    Q_PROPERTY(int algorithm READ algorithm_7 WRITE setAlgorithm_7 NOTIFY algorithmChanged_7)
    Q_PROPERTY(int windowType READ windowType_8 WRITE setWindowType_8 NOTIFY windowTypeChanged_8)
    Q_PROPERTY(int windowSize READ windowSize_9 WRITE setWindowSize_9 NOTIFY windowSizeChanged_9)
    Q_PROPERTY(int zeroPaddingFactor READ zeroPaddingFactor_10 WRITE setZeroPaddingFactor_10 NOTIFY zeroPaddingFactorChanged_10)

public:
    explicit AbstractSpectrogramSettingsModel(QObject* parent = nullptr);
    virtual ~AbstractSpectrogramSettingsModel() = default;

    virtual bool spectralSelectionEnabled_1() const = 0;
    virtual void setSpectralSelectionEnabled_1(bool value) = 0;

    virtual int colorGainDb_2() const = 0;
    virtual void setColorGainDb_2(int value) = 0;

    virtual int colorRangeDb_3() const = 0;
    virtual void setColorRangeDb_3(int value) = 0;
    int colorRangeDbMin() const { return 1; }

    virtual int colorHighBoostDbPerDec_4() const = 0;
    virtual void setColorHighBoostDbPerDec_4(int value) = 0;
    int colorHighBoostDbPerDecMin() const { return 0; }
    int colorHighBoostDbPerDecMax() const { return 60; }

    virtual int colorScheme_5() const = 0;
    virtual void setColorScheme_5(int value) = 0;
    QList<QString> colorSchemeNames() const;

    virtual int scale_6() const = 0;
    virtual void setScale_6(int value) = 0;
    QList<QString> scaleNames() const;

    virtual int algorithm_7() const = 0;
    virtual void setAlgorithm_7(int value) = 0;

    virtual int windowType_8() const = 0;
    virtual void setWindowType_8(int value) = 0;

    virtual int windowSize_9() const = 0;
    virtual void setWindowSize_9(int value) = 0;

    virtual int zeroPaddingFactor_10() const = 0;
    virtual void setZeroPaddingFactor_10(int value) = 0;

signals:
    void spectralSelectionEnabledChanged_1();
    void colorGainDbChanged_2();
    void colorRangeDbChanged_3();
    void colorHighBoostDbPerDecChanged_4();
    void colorSchemeChanged_5();
    void scaleChanged_6();
    void algorithmChanged_7();
    void windowTypeChanged_8();
    void windowSizeChanged_9();
    void zeroPaddingFactorChanged_10();

private:
    QString colorSchemeName(int) const;
    QString scaleName(int) const;
};
}
