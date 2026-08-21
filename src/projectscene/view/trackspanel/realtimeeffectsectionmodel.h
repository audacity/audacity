/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "iprojectsceneconfiguration.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"
#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "ui/inavigationcontroller.h"
#include <QObject>
#include <map>

namespace au::projectscene {
class RealtimeEffectSectionModel : public QObject, public muse::actions::Actionable, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(bool showEffectsSection READ prop_showEffectsSection WRITE prop_setShowEffectsSection NOTIFY showEffectsSectionChanged)
    Q_PROPERTY(
        bool navigationFocusInsideEffectsPanel READ prop_navigationFocusInsideEffectsPanel WRITE prop_setNavigationFocusInsideEffectsPanel NOTIFY navigationFocusInsideEffectsPanelChanged)

    muse::GlobalInject<IProjectSceneConfiguration> configuration;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::ContextInject<muse::ui::INavigationController> navigationController{ this };

public:
    explicit RealtimeEffectSectionModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    bool prop_showEffectsSection() const;
    void prop_setShowEffectsSection(bool show);

    bool prop_navigationFocusInsideEffectsPanel() const;
    void prop_setNavigationFocusInsideEffectsPanel(bool inside);

signals:
    void showEffectsSectionChanged();
    void focusEffectsPanelRequested();
    void navigationFocusInsideEffectsPanelChanged();

private:
    void savePreviouslyFocusedControl();
    void restorePreviouslyFocusedControl();

    muse::ui::INavigationControl* m_previouslyFocusedControl = nullptr;
    bool m_navigationFocusInsideEffectsPanel = false;
};
}
