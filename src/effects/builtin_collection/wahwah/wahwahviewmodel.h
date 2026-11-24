#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

struct EffectWahwahSettings;

namespace au::effects {
class WahWahViewModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(QVariantMap paramsList READ paramsList NOTIFY paramsListChanged FINAL)

public:
    WahWahViewModel(QObject* parent, int instanceId);

    QVariantMap paramsList() const;

    Q_INVOKABLE void setParam(const QString& key, double val);

signals:
    void paramsListChanged();

private:
    void doReload() override;
    void doUpdateSettings() override;

    using Setter = std::function<void (EffectWahwahSettings&, double)>;

    QVariantMap m_paramsList;
    QMap<QString, Setter> m_setters;
};

class WahWahViewModelFactory : public EffectViewModelFactory<WahWahViewModel>
{
};
}
