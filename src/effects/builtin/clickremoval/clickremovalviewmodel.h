/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/builtineffectmodel.h"

namespace au::effects {
class ClickRemovalEffect;
class ClickRemovalViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(QString effectTitle READ effectTitle CONSTANT FINAL)

    Q_PROPERTY(QString thresholdLabel READ thresholdLabel CONSTANT FINAL)
    Q_PROPERTY(int thresholdValue READ thresholdValue WRITE setThresholdValue NOTIFY thresholdValueChanged FINAL)
    Q_PROPERTY(int thresholdMin READ thresholdMin CONSTANT FINAL)
    Q_PROPERTY(int thresholdMax READ thresholdMax CONSTANT FINAL)
    Q_PROPERTY(int thresholdStep READ thresholdStep CONSTANT FINAL)
    Q_PROPERTY(int thresholdDecimals READ thresholdDecimals CONSTANT FINAL)

    Q_PROPERTY(QString widthLabel READ widthLabel CONSTANT FINAL)
    Q_PROPERTY(int widthValue READ widthValue WRITE setWidthValue NOTIFY widthValueChanged FINAL)
    Q_PROPERTY(int widthMin READ widthMin CONSTANT FINAL)
    Q_PROPERTY(int widthMax READ widthMax CONSTANT FINAL)
    Q_PROPERTY(int widthStep READ widthStep CONSTANT FINAL)
    Q_PROPERTY(int widthDecimals READ widthDecimals CONSTANT FINAL)

public:
    ClickRemovalViewModel() = default;

    QString effectTitle() const;

    QString thresholdLabel() const;
    int thresholdValue() const;
    void setThresholdValue(int newThreshold);
    int thresholdMin() const;
    int thresholdMax() const;
    int thresholdStep() const;
    int thresholdDecimals() const;

    QString widthLabel() const;
    int widthValue() const;
    void setWidthValue(int newWidth);
    int widthMin() const;
    int widthMax() const;
    int widthStep() const;
    int widthDecimals() const;

signals:
    void thresholdValueChanged();
    void widthValueChanged();

private:
    void doReload() override;
};
}
