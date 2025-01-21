/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QQmlComponent>
#include <QQuickItem>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "../ieffectsviewregister.h"
#include "../ieffectsuiengine.h"

namespace au::effects {
//! TODO Move to builtin module
class EffectViewLoader : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QQuickItem * contentItem READ contentItem NOTIFY contentItemChanged FINAL)

    muse::Inject<IEffectsViewRegister> viewRegister;
    muse::Inject<IEffectsUiEngine> engine;

public:
    EffectViewLoader(QObject* parent = nullptr);

    QQuickItem* contentItem() const;

    Q_INVOKABLE void load(const QString& type, const QString& instanceId, const QString& effectState, QObject* itemParent);

signals:
    void titleChanged();
    void contentItemChanged();

    void closeRequested();

private:
    QQuickItem* m_contentItem = nullptr;
};
}
