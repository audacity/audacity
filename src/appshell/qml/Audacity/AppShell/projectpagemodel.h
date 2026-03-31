/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_APPSHELL_PROJECTPAGEMODEL_H
#define AU_APPSHELL_PROJECTPAGEMODEL_H

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"
#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/dockwindow/idockwindowprovider.h"
#include "framework/ui/iuiconfiguration.h"
#include "framework/ui/iuistate.h"

#include "context/iglobalcontext.h"
#include "playback/iplaybackconfiguration.h"

#include <QtQml/qqmlregistration.h>

namespace au::appshell {
class ProjectPageModel : public QObject, public muse::async::Asyncable, public muse::actions::Actionable, public muse::Contextable
{
    Q_OBJECT
    QML_ELEMENT

    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfiguration;
    muse::GlobalInject<playback::IPlaybackConfiguration> playbackConfiguration;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<muse::dock::IDockWindowProvider> dockWindowProvider { this };
    muse::ContextInject<muse::ui::IUiState> uiState { this };

public:
    explicit ProjectPageModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    Q_INVOKABLE QString projectToolBarName() const;
    Q_INVOKABLE QString playbackToolBarName() const;
    Q_INVOKABLE QString undoRedoToolBarName() const;
    Q_INVOKABLE QString workspacesToolBarName() const;

    Q_INVOKABLE QString tracksPanelName() const;
    Q_INVOKABLE QString historyPanelName() const;
    Q_INVOKABLE QString playbackMeterPanelName() const;

    Q_INVOKABLE QString statusBarName() const;

private:
    void toggleDock(const QString& name);

    void updatePlaybackMeterVisibility();

    bool m_inited = false;
};
}

#endif // AU_APPSHELL_PROJECTPAGEMODEL_H
