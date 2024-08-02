/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "effects/ieffectsprovider.h"

namespace au::effects {
class AbstractEffectModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString id READ id WRITE setId NOTIFY idChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    AbstractEffectModel(QObject* parent = nullptr);

    QString id() const;
    void setId(const QString& newId);

signals:
    void idChanged();

protected:
    double linearToDB(double value) const;
    double dBToLinear(double value) const;

private:
    QString m_id;
};
}
