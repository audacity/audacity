/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/builtineffectmodel.h"

namespace au::effects {
class TruncateSilenceEffect;
class TruncateSilenceViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(QString effectTitle READ effectTitle CONSTANT FINAL)

    // Detect silence section
    Q_PROPERTY(QString detectSilenceLabel READ detectSilenceLabel CONSTANT FINAL)

    // Threshold parameter
    Q_PROPERTY(QString thresholdLabel READ thresholdLabel CONSTANT FINAL)
    Q_PROPERTY(double thresholdValue READ thresholdValue WRITE setThresholdValue NOTIFY thresholdValueChanged FINAL)
    Q_PROPERTY(double thresholdMin READ thresholdMin CONSTANT FINAL)
    Q_PROPERTY(double thresholdMax READ thresholdMax CONSTANT FINAL)
    Q_PROPERTY(double thresholdStep READ thresholdStep CONSTANT FINAL)
    Q_PROPERTY(int thresholdDecimals READ thresholdDecimals CONSTANT FINAL)
    Q_PROPERTY(QString thresholdUnit READ thresholdUnit CONSTANT FINAL)

    // Minimum parameter
    Q_PROPERTY(QString minimumLabel READ minimumLabel CONSTANT FINAL)
    Q_PROPERTY(double minimumValue READ minimumValue WRITE setMinimumValue NOTIFY minimumValueChanged FINAL)
    Q_PROPERTY(double minimumMin READ minimumMin CONSTANT FINAL)
    Q_PROPERTY(double minimumMax READ minimumMax CONSTANT FINAL)
    Q_PROPERTY(double minimumStep READ minimumStep CONSTANT FINAL)
    Q_PROPERTY(int minimumDecimals READ minimumDecimals CONSTANT FINAL)
    Q_PROPERTY(QString minimumUnit READ minimumUnit CONSTANT FINAL)

    // Action parameter (Truncate or Compress)
    Q_PROPERTY(QString actionLabel READ actionLabel CONSTANT FINAL)
    Q_PROPERTY(int actionIndex READ actionIndex WRITE setActionIndex NOTIFY actionIndexChanged FINAL)
    Q_PROPERTY(QStringList actionChoices READ actionChoices CONSTANT FINAL)

    // Truncate parameter
    Q_PROPERTY(QString truncateToLabel READ truncateToLabel CONSTANT FINAL)
    Q_PROPERTY(QString truncateActionLabel READ truncateActionLabel CONSTANT FINAL)
    Q_PROPERTY(double truncateValue READ truncateValue WRITE setTruncateValue NOTIFY truncateValueChanged FINAL)
    Q_PROPERTY(double truncateMin READ truncateMin CONSTANT FINAL)
    Q_PROPERTY(double truncateMax READ truncateMax CONSTANT FINAL)
    Q_PROPERTY(double truncateStep READ truncateStep CONSTANT FINAL)
    Q_PROPERTY(int truncateDecimals READ truncateDecimals CONSTANT FINAL)
    Q_PROPERTY(QString truncateUnit READ truncateUnit CONSTANT FINAL)

    // Compress parameter
    Q_PROPERTY(QString compressToLabel READ compressToLabel CONSTANT FINAL)
    Q_PROPERTY(QString compressActionLabel READ compressActionLabel CONSTANT FINAL)
    Q_PROPERTY(double compressValue READ compressValue WRITE setCompressValue NOTIFY compressValueChanged FINAL)
    Q_PROPERTY(double compressMin READ compressMin CONSTANT FINAL)
    Q_PROPERTY(double compressMax READ compressMax CONSTANT FINAL)
    Q_PROPERTY(double compressStep READ compressStep CONSTANT FINAL)
    Q_PROPERTY(int compressDecimals READ compressDecimals CONSTANT FINAL)
    Q_PROPERTY(QString compressUnit READ compressUnit CONSTANT FINAL)

    // Independent parameter
    Q_PROPERTY(QString independentTruncateLabel READ independentTruncateLabel CONSTANT FINAL)
    Q_PROPERTY(QString independentCompressLabel READ independentCompressLabel CONSTANT FINAL)
    Q_PROPERTY(bool independentValue READ independentValue WRITE setIndependentValue NOTIFY independentValueChanged FINAL)

public:
    TruncateSilenceViewModel() = default;

    QString effectTitle() const;

    // Detect silence section
    QString detectSilenceLabel() const;

    // Threshold
    QString thresholdLabel() const;
    double thresholdValue() const;
    void setThresholdValue(double newThreshold);
    double thresholdMin() const;
    double thresholdMax() const;
    double thresholdStep() const;
    int thresholdDecimals() const;
    QString thresholdUnit() const;

    // Minimum
    QString minimumLabel() const;
    double minimumValue() const;
    void setMinimumValue(double newMinimum);
    double minimumMin() const;
    double minimumMax() const;
    double minimumStep() const;
    int minimumDecimals() const;
    QString minimumUnit() const;

    // Action
    QString actionLabel() const;
    int actionIndex() const;
    void setActionIndex(int newActionIndex);
    QStringList actionChoices() const;

    // Truncate
    QString truncateToLabel() const;
    QString truncateActionLabel() const;
    double truncateValue() const;
    void setTruncateValue(double newTruncate);
    double truncateMin() const;
    double truncateMax() const;
    double truncateStep() const;
    int truncateDecimals() const;
    QString truncateUnit() const;

    // Compress
    QString compressToLabel() const;
    QString compressActionLabel() const;
    double compressValue() const;
    void setCompressValue(double newCompress);
    double compressMin() const;
    double compressMax() const;
    double compressStep() const;
    int compressDecimals() const;
    QString compressUnit() const;

    // Independent
    QString independentTruncateLabel() const;
    QString independentCompressLabel() const;
    bool independentValue() const;
    void setIndependentValue(bool newIndependent);

signals:
    void thresholdValueChanged();
    void actionIndexChanged();
    void minimumValueChanged();
    void truncateValueChanged();
    void compressValueChanged();
    void independentValueChanged();

private:
    void doReload() override;

    // Internal unit helpers
    QString dbUnit() const;
    QString secondsUnit() const;
    QString percentUnit() const;
};
}
