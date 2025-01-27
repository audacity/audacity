/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iprojectsceneconfiguration.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"
#include "async/asyncable.h"
#include "modularity/ioc.h"
#include <QObject>
#include <map>

namespace au::projectscene {
class RealtimeEffectSectionModel : public QObject, public muse::actions::Actionable, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(bool showEffectsSection READ prop_showEffectsSection WRITE prop_setShowEffectsSection NOTIFY showEffectsSectionChanged)

    muse::Inject<IProjectSceneConfiguration> configuration;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    explicit RealtimeEffectSectionModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    bool prop_showEffectsSection() const;
    void prop_setShowEffectsSection(bool show);

signals:
    void showEffectsSectionChanged();
};
}
