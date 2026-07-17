#pragma once

#include "effects/builtin/view/builtineffectmodel.h"

struct BassTrebleSettings;

namespace au::effects {
class BassTrebleEffect;
class BassTrebleViewModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(QVariantMap paramsList READ paramsList NOTIFY paramsListChanged FINAL)

    Q_PROPERTY(bool link READ link WRITE setLink NOTIFY linkChanged FINAL)

public:
    BassTrebleViewModel(QObject* parent, int instanceId);

    QVariantMap paramsList() const;

    Q_INVOKABLE void setParam(const QString& key, double val);

    bool link() const;
    void setLink(bool newLink);

signals:
    void paramsListChanged();
    void linkChanged();

private:

    void doReload() override;
    void doUpdateSettings() override
    {
    }

    using Setter = std::function<void (BassTrebleSettings&, double)>;

    QVariantMap m_paramsList;
    QMap<QString, Setter> m_setters;
};

class BassTrebleViewModelFactory : public EffectViewModelFactory<BassTrebleViewModel>
{
};
}
