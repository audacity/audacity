/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstracteffectmodel.h"
#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class AbstractEffectSettingModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(QString paramId READ paramId WRITE setParamId NOTIFY paramIdChanged FINAL)
    Q_PROPERTY(double value READ value WRITE setValue NOTIFY valueChanged FINAL)

    Q_PROPERTY(double min READ min NOTIFY minChanged FINAL)
    Q_PROPERTY(double max READ max NOTIFY maxChanged FINAL)
    Q_PROPERTY(double step READ step NOTIFY stepChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    AbstractEffectSettingModel(QObject* parent);
    ~AbstractEffectSettingModel() override = default;

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
    void labelChanged();
    void minChanged();
    void maxChanged();
    void stepChanged();

protected:
    const Effect* effect() const;
    QString m_paramId;

private:
    void doReload() override;
    void doUpdateSettings() override;
};
}
