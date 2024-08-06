/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "effects/ieffectsprovider.h"
#include "effects/ieffectsprocessing.h"

namespace au::effects {
class AbstractEffectModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(QString id READ id WRITE setId NOTIFY idChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

protected: // todo
    muse::Inject<IEffectsProcessing> effectsProcessing;

public:
    AbstractEffectModel(QObject* parent = nullptr);

    Q_INVOKABLE void apply();

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
