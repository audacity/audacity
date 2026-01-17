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
class BuiltinEffectViewLoader : public QObject, public muse::async::Asyncable, muse::Injectable
{
    Q_OBJECT

    Q_PROPERTY(QQuickItem * contentItem READ contentItem NOTIFY contentItemChanged FINAL)

    muse::Inject<IEffectsViewRegister> viewRegister { this };
    muse::Inject<IEffectsUiEngine> engine { this };
    muse::Inject<IEffectInstancesRegister> instancesRegister { this };

public:
    BuiltinEffectViewLoader(QObject* parent = nullptr);
    ~BuiltinEffectViewLoader() override;

    QQuickItem* contentItem() const;

    Q_INVOKABLE void load(int instanceId, QObject* itemParent, QObject* dialogView, bool usedDestructively);

signals:
    void titleChanged();
    void contentItemChanged();

    void closeRequested();

private:
    QQuickItem* m_contentItem = nullptr;
};
}
