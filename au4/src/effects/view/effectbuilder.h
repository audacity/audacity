/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QQmlComponent>
#include <QQuickItem>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "effects/ieffectsprovider.h"
#include "effects/ieffectsuiengine.h"

namespace au::effects {
class EffectBuilder : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)
    Q_PROPERTY(QQuickItem * contentItem READ contentItem NOTIFY contentItemChanged FINAL)

    muse::Inject<IEffectsProvider> provider;
    muse::Inject<IEffectsUiEngine> engine;

public:
    EffectBuilder(QObject* parent = nullptr);

    QString title() const;
    QQuickItem* contentItem() const;

    Q_INVOKABLE void load(const QString& id, QObject* itemParent);

signals:
    void titleChanged();
    void contentItemChanged();

    void closeRequested();

private:

    void setTitle(QString title);

    QString m_title;
    QQuickItem* m_contentItem = nullptr;
};
}
