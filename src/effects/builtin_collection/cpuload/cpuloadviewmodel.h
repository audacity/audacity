/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

namespace au::effects {
struct CpuLoadSettings;

class CpuLoadViewModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(QVariantMap paramsList READ paramsList NOTIFY paramsListChanged FINAL)

public:
    CpuLoadViewModel(QObject* parent, int instanceId);

    QVariantMap paramsList() const;

    Q_INVOKABLE void setParam(const QString& key, double val);

signals:
    void paramsListChanged();

private:
    void doReload() override;
    void doUpdateSettings() override {}

    using Setter = std::function<void (CpuLoadSettings&, double)>;

    QVariantMap m_paramsList;
    QMap<QString, Setter> m_setters;
};

class CpuLoadViewModelFactory : public EffectViewModelFactory<CpuLoadViewModel>
{
};
}
