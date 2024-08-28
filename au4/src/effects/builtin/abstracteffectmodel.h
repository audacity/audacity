/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "../ieffectsprovider.h"

class Effect;
namespace au::effects {
class AbstractEffectModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(QString effectId READ effectId WRITE setEffectId NOTIFY effectIdChanged FINAL)

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    AbstractEffectModel(QObject* parent = nullptr);

    QString effectId() const;
    void setEffectId(const QString& newEffectId);

signals:
    void effectIdChanged();

protected:

    Effect* effect() const;

private:
    QString m_effectId;
};
}
