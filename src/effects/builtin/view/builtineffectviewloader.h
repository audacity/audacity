/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QQmlComponent>
#include <QQuickItem>

#include "global/async/asyncable.h"
#include "modularity/ioc.h"

#include "ieffectsviewregister.h"

#include "effects/effects_base/ieffectsuiengine.h"
#include "effects/effects_base/ieffectinstancesregister.h"

namespace au::effects {
//! TODO Move to builtin module
class BuiltinEffectViewLoader : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(int instanceId READ instanceId CONSTANT FINAL)
    Q_PROPERTY(QQuickItem * contentItem READ contentItem NOTIFY contentItemChanged FINAL)

    muse::Inject<IEffectsViewRegister> viewRegister;
    muse::Inject<IEffectsUiEngine> engine;
    muse::Inject<IEffectInstancesRegister> instancesRegister;

public:
    BuiltinEffectViewLoader(QObject* parent = nullptr);
    ~BuiltinEffectViewLoader() override;

    QQuickItem* contentItem() const;

    Q_INVOKABLE void load(QObject* itemParent, QObject* dialogView, bool usedDestructively);

    int instanceId() const { return m_instanceId; }

signals:
    void titleChanged();
    void contentItemChanged();

    void closeRequested();

private:
    const int m_instanceId;
    QQuickItem* m_contentItem = nullptr;
};
}
