/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QQmlComponent>
#include <QQuickItem>

#include "global/async/asyncable.h"
#include "modularity/ioc.h"

#include "effects/builtin/ibuiltineffectsviewregister.h"
#include "effects/effects_base/ieffectsuiengine.h"
#include "effects/effects_base/ieffectinstancesregister.h"

namespace au::effects {
class BuiltinEffectViewLoader : public QObject, public muse::async::Asyncable, muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(QQuickItem * contentItem READ contentItem NOTIFY contentItemChanged FINAL)

    muse::GlobalInject<IBuiltinEffectsViewRegister> viewRegister;
    muse::GlobalInject<IEffectInstancesRegister> instancesRegister;

    muse::ContextInject<IEffectsUiEngine> engine { this };

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
