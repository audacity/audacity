#pragma once

#include "../common/builtineffectmodel.h"

namespace au::effects {
class ReverbEffect;
struct ReverbSettings;
class ReverbViewModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(QVariantMap paramsList READ paramsList NOTIFY paramsListChanged FINAL)

    Q_PROPERTY(bool wetOnly READ wetOnly WRITE setWetOnly NOTIFY wetOnlyChanged FINAL)

public:
    ReverbViewModel();

    QVariantMap paramsList() const;

    Q_INVOKABLE void setParam(const QString& key, double val);

    bool wetOnly() const;
    void setWetOnly(bool newWetOnly);

signals:
    void paramsListChanged();
    void wetOnlyChanged();

private:

    void doReload() override;
    void doUpdateSettings() override
    {
        // TODO: let https://github.com/audacity/audacity/pull/8565 get merged and then implement this.
    }

    using Setter = std::function<void (ReverbSettings&, double)>;

    QVariantMap m_paramsList;
    QMap<QString, Setter> m_setters;
};
}
