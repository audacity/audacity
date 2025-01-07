#pragma once

#include "../common/abstracteffectmodel.h"

namespace au::effects {
class ReverbEffect;
struct ReverbSettings;
class ReverbViewModel : public AbstractEffectModel
{
    Q_OBJECT
    Q_PROPERTY(QVariantList paramsList READ paramsList NOTIFY paramsListChanged FINAL)
    Q_PROPERTY(bool wetOnly READ wetOnly WRITE setWetOnly NOTIFY wetOnlyChanged FINAL)

public:
    ReverbViewModel();

    QVariantList paramsList() const;

    Q_INVOKABLE void setParam(const QString& key, double val);

    bool wetOnly() const;
    void setWetOnly(bool newWetOnly);

signals:
    void paramsListChanged();
    void wetOnlyChanged();

private:

    void doReload() override;

    using Setter = std::function<void (ReverbSettings&, double)>;

    QVariantList m_paramsList;
    QMap<QString, Setter> m_setters;
};
}
