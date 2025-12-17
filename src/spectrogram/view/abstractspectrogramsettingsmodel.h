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

    Q_PROPERTY(int minFreq READ minFreq WRITE setMinFreq NOTIFY minFreqChanged)
    Q_PROPERTY(int maxFreq READ maxFreq WRITE setMaxFreq NOTIFY maxFreqChanged)

    Q_PROPERTY(int colorGainDb READ colorGainDb WRITE setColorGainDb NOTIFY colorGainDbChanged)

    Q_PROPERTY(int colorRangeDb READ colorRangeDb WRITE setColorRangeDb NOTIFY colorRangeDbChanged)
    Q_PROPERTY(int colorRangeDbMin READ colorRangeDbMin CONSTANT)

    Q_PROPERTY(
        int colorHighBoostDbPerDec READ colorHighBoostDbPerDec WRITE setColorHighBoostDbPerDec NOTIFY colorHighBoostDbPerDecChanged)
    Q_PROPERTY(int colorHighBoostDbPerDecMin READ colorHighBoostDbPerDecMin CONSTANT)
    Q_PROPERTY(int colorHighBoostDbPerDecMax READ colorHighBoostDbPerDecMax CONSTANT)

    Q_PROPERTY(int colorScheme READ colorScheme WRITE setColorScheme NOTIFY colorSchemeChanged)
    Q_PROPERTY(QList<QString> colorSchemeNames READ colorSchemeNames CONSTANT)

    Q_PROPERTY(int scale READ scale WRITE setScale NOTIFY scaleChanged)
    Q_PROPERTY(QList<QString> scaleNames READ scaleNames CONSTANT)

    Q_PROPERTY(int algorithm READ algorithm WRITE setAlgorithm NOTIFY algorithmChanged)
    Q_PROPERTY(int windowType READ windowType WRITE setWindowType NOTIFY windowTypeChanged)
    Q_PROPERTY(int windowSize READ windowSize WRITE setWindowSize NOTIFY windowSizeChanged)
    Q_PROPERTY(int zeroPaddingFactor READ zeroPaddingFactor WRITE setZeroPaddingFactor NOTIFY zeroPaddingFactorChanged)

public:
    explicit AbstractSpectrogramSettingsModel(QObject* parent = nullptr);
    virtual ~AbstractSpectrogramSettingsModel() = default;

    virtual int minFreq() const = 0;
    void setMinFreq(int value);
    int frequencyHardMinimum() const { return 0; }

    virtual int maxFreq() const = 0;
    void setMaxFreq(int value);
    int frequencyHardMaximum() const
    {
        // TODO make dynamic based on sample rate
        return 22050;
    }

    virtual int colorGainDb() const = 0;
    virtual void setColorGainDb(int value) = 0;

    virtual int colorRangeDb() const = 0;
    virtual void setColorRangeDb(int value) = 0;
    int colorRangeDbMin() const { return 1; }

    virtual int colorHighBoostDbPerDec() const = 0;
    virtual void setColorHighBoostDbPerDec(int value) = 0;
    int colorHighBoostDbPerDecMin() const { return 0; }
    int colorHighBoostDbPerDecMax() const { return 60; }

    virtual int colorScheme() const = 0;
    virtual void setColorScheme(int value) = 0;
    QList<QString> colorSchemeNames() const;

    virtual int scale() const = 0;
    virtual void setScale(int value) = 0;
    QList<QString> scaleNames() const;

    virtual int algorithm() const = 0;
    virtual void setAlgorithm(int value) = 0;

    virtual int windowType() const = 0;
    virtual void setWindowType(int value) = 0;

    virtual int windowSize() const = 0;
    virtual void setWindowSize(int value) = 0;

    virtual int zeroPaddingFactor() const = 0;
    virtual void setZeroPaddingFactor(int value) = 0;

signals:
    void minFreqChanged();
    void maxFreqChanged();
    void colorGainDbChanged();
    void colorRangeDbChanged();
    void colorHighBoostDbPerDecChanged();
    void colorSchemeChanged();
    void scaleChanged();
    void algorithmChanged();
    void windowTypeChanged();
    void windowSizeChanged();
    void zeroPaddingFactorChanged();

private:
    virtual void doSetMinFreq(int value) = 0;
    virtual void doSetMaxFreq(int value) = 0;

    QString colorSchemeName(int) const;
    QString scaleName(int) const;
};
}
