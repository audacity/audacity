#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

struct EffectPhaserSettings;

namespace au::effects {
class PhaserViewModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(QVariantMap paramsList READ paramsList NOTIFY paramsListChanged FINAL)

public:
    PhaserViewModel(QObject* parent, int instanceId);

    QVariantMap paramsList() const;

    Q_INVOKABLE void setParam(const QString& key, double val);

signals:
    void paramsListChanged();

private:
    void doReload() override;
    void doUpdateSettings() override;

    using Setter = std::function<void (EffectPhaserSettings&, double)>;

    QVariantMap m_paramsList;
    QMap<QString, Setter> m_setters;
};

class PhaserViewModelFactory : public EffectViewModelFactory<PhaserViewModel>
{
};
}
