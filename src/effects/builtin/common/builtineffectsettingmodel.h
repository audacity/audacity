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
    Q_PROPERTY(double defaultValue READ defaultValue CONSTANT FINAL)
    Q_PROPERTY(QString title READ title CONSTANT FINAL)
    Q_PROPERTY(QString unit READ unit CONSTANT FINAL)

public:
    BuiltinEffectSettingModel(QObject* parent, int instanceId);
    ~BuiltinEffectSettingModel() override = default;

    QString paramId() const;
    void setParamId(const QString& newParamId);

    virtual double value() const = 0;
    virtual void setValue(double newValue) = 0;

    virtual double min() const = 0;
    virtual double max() const = 0;
    virtual double step() const = 0;
    virtual double defaultValue() const = 0;
    virtual QString title() const = 0;
    virtual QString unit() const = 0;

signals:
    void paramIdChanged();
    void valueChanged();

protected:
    QString m_paramId;

private:
    void doReload() override;
    void doUpdateSettings() override;
};

class AbstractBuiltinEffectSettingModelFactory : public QObject
{
    Q_OBJECT
public:
    virtual ~AbstractBuiltinEffectSettingModelFactory() = default;

    Q_INVOKABLE BuiltinEffectSettingModel* createModel(QObject* parent, int instanceId, const QString& paramId) const
    {
        BuiltinEffectSettingModel* model = doCreateModel(parent, instanceId);
        model->setParamId(paramId);
        return model;
    }

private:
    virtual BuiltinEffectSettingModel* doCreateModel(QObject* parent, int instanceId) const = 0;
};

template<typename T>
class BuiltinEffectSettingModelFactory : public AbstractBuiltinEffectSettingModelFactory
{
public:
    ~BuiltinEffectSettingModelFactory() override = default;

    BuiltinEffectSettingModel* doCreateModel(QObject* parent, int instanceId) const override
    {
        return new T(parent, instanceId);
    }
};
}
