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

    Q_PROPERTY(double thresholdValue READ thresholdValue WRITE setThresholdValue NOTIFY thresholdValueChanged FINAL)
    Q_PROPERTY(double minimumValue READ minimumValue WRITE setMinimumValue NOTIFY minimumValueChanged FINAL)
    Q_PROPERTY(int actionIndex READ actionIndex WRITE setActionIndex NOTIFY actionIndexChanged FINAL)
    Q_PROPERTY(QVariantMap currentActionConfig READ currentActionConfig NOTIFY actionIndexChanged FINAL)
    Q_PROPERTY(double truncateValue READ truncateValue WRITE setTruncateValue NOTIFY truncateValueChanged FINAL)
    Q_PROPERTY(double compressValue READ compressValue WRITE setCompressValue NOTIFY compressValueChanged FINAL)
    Q_PROPERTY(bool independentValue READ independentValue WRITE setIndependentValue NOTIFY independentValueChanged FINAL)

public:
    TruncateSilenceViewModel(QObject* parent, int instanceId);
    ~TruncateSilenceViewModel() override = default;

    double thresholdValue() const;
    void setThresholdValue(double newThreshold);

    double minimumValue() const;
    void setMinimumValue(double newMinimum);

    int actionIndex() const;
    void setActionIndex(int newActionIndex);

    QVariantMap currentActionConfig() const;

    double truncateValue() const;
    void setTruncateValue(double newTruncate);

    double compressValue() const;
    void setCompressValue(double newCompress);

    bool independentValue() const;
    void setIndependentValue(bool newIndependent);

    Q_INVOKABLE QString effectTitle() const;

    Q_INVOKABLE QString detectSilenceLabel() const;

    Q_INVOKABLE QString thresholdLabel() const;
    Q_INVOKABLE double thresholdMin() const;
    Q_INVOKABLE double thresholdMax() const;
    Q_INVOKABLE double thresholdStep() const;
    Q_INVOKABLE int thresholdDecimals() const;
    Q_INVOKABLE QString thresholdUnitSymbol() const;

    Q_INVOKABLE QString minimumLabel() const;
    Q_INVOKABLE double minimumMin() const;
    Q_INVOKABLE double minimumMax() const;
    Q_INVOKABLE double minimumStep() const;
    Q_INVOKABLE int minimumDecimals() const;
    Q_INVOKABLE QString minimumUnitSymbol() const;

    Q_INVOKABLE QString actionLabel() const;
    Q_INVOKABLE QVariantList actionModel() const;

    Q_INVOKABLE QString truncateToLabel() const;
    Q_INVOKABLE QString truncateActionLabel() const;
    Q_INVOKABLE double truncateMin() const;
    Q_INVOKABLE double truncateMax() const;
    Q_INVOKABLE double truncateStep() const;
    Q_INVOKABLE int truncateDecimals() const;
    Q_INVOKABLE QString truncateUnitSymbol() const;

    Q_INVOKABLE QString compressToLabel() const;
    Q_INVOKABLE QString compressActionLabel() const;
    Q_INVOKABLE double compressMin() const;
    Q_INVOKABLE double compressMax() const;
    Q_INVOKABLE double compressStep() const;
    Q_INVOKABLE int compressDecimals() const;
    Q_INVOKABLE QString compressUnitSymbol() const;

    Q_INVOKABLE QString independentTruncateLabel() const;
    Q_INVOKABLE QString independentCompressLabel() const;

signals:
    void thresholdValueChanged();
    void actionIndexChanged();
    void minimumValueChanged();
    void truncateValueChanged();
    void compressValueChanged();
    void independentValueChanged();

private:
    void doReload() override;
};

class TruncateSilenceViewModelFactory : public EffectViewModelFactory<TruncateSilenceViewModel>
{
};
}
