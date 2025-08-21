/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "builtineffectmodel.h"

namespace au::effects {
class BuiltinEffectSettingModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(QString paramId READ paramId WRITE setParamId NOTIFY paramIdChanged FINAL)
    Q_PROPERTY(double value READ value WRITE setValue NOTIFY valueChanged FINAL)

    Q_PROPERTY(double min READ min CONSTANT FINAL)
    Q_PROPERTY(double max READ max CONSTANT FINAL)
    Q_PROPERTY(double step READ step CONSTANT FINAL)

public:
    BuiltinEffectSettingModel(QObject* parent);
    ~BuiltinEffectSettingModel() override = default;

    QString paramId() const;
    void setParamId(const QString& newParamId);

    virtual double value() const = 0;
    virtual void setValue(double newValue) = 0;

    virtual double min() const = 0;
    virtual double max() const = 0;
    virtual double step() const = 0;

signals:
    void paramIdChanged();
    void valueChanged();

protected:
    QString m_paramId;

private:
    void doReload() override;
    void doUpdateSettings() override;
};
}
