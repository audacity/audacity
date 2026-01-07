/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectinstancesregister.h"
#include "ieffectparametersprovider.h"
#include "ieffectsprovider.h"
#include "effectstypes.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include <QObject>
#include <QVariantList>
#include <QVariantMap>

namespace au::effects {
class GeneratedEffectViewerModel : public QObject, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(QString effectName READ effectName NOTIFY effectNameChanged FINAL)
    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)
    Q_PROPERTY(QString noParametersMessage READ noParametersMessage NOTIFY noParametersMessageChanged FINAL)
    Q_PROPERTY(QVariantList parameters READ parameters NOTIFY parametersChanged FINAL)
    Q_PROPERTY(bool hasParameters READ hasParameters NOTIFY parametersChanged FINAL)

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectParametersProvider> parametersProvider;
    muse::Inject<IEffectsProvider> effectsProvider;

public:
    explicit GeneratedEffectViewerModel(QObject* parent = nullptr);
    ~GeneratedEffectViewerModel() override = default;

    Q_INVOKABLE void load();
    Q_INVOKABLE void setParameterValue(const QString& parameterId, double plainValue);
    Q_INVOKABLE QString getParameterValueString(const QString& parameterId, double normalizedValue) const;

    int instanceId() const;
    void setInstanceId(int newInstanceId);

    QString effectName() const;
    QString title() const;
    QString noParametersMessage() const;
    QVariantList parameters() const;
    bool hasParameters() const;

signals:
    void instanceIdChanged();
    void effectNameChanged();
    void titleChanged();
    void noParametersMessageChanged();
    void parametersChanged();

private:
    QVariantMap parameterInfoToVariant(const ParameterInfo& info) const;
    void updateParameters();
    void updateEffectName();

    int m_instanceId = -1;
    QString m_effectName;
    QVariantList m_parameters;
};
}
