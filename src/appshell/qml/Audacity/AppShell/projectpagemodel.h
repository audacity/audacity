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

#include "context/iglobalcontext.h"
#include "playback/iplaybackconfiguration.h"

#include "iappshellconfiguration.h"

#include <QtQml/qqmlregistration.h>

//! TODO AU4
// #include "braille/ibrailleconfiguration.h"

namespace au::appshell {
class ProjectPageModel : public QObject, public muse::async::Asyncable, public muse::actions::Actionable, public muse::Injectable
{
    Q_OBJECT
    QML_ELEMENT

    muse::GlobalInject<IAppShellConfiguration> configuration;
    muse::GlobalInject<muse::ui::IUiConfiguration> uiConfiguration;
    muse::GlobalInject<playback::IPlaybackConfiguration> playbackConfiguration;

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::Inject<au::context::IGlobalContext> globalContext { this };
    muse::Inject<muse::dock::IDockWindowProvider> dockWindowProvider { this };

//! TODO AU4
//    INJECT(braille::IBrailleConfiguration, brailleConfiguration)

    Q_PROPERTY(bool isNavigatorVisible READ isNavigatorVisible NOTIFY isNavigatorVisibleChanged)
    Q_PROPERTY(bool isBraillePanelVisible READ isBraillePanelVisible NOTIFY isBraillePanelVisibleChanged)

public:
    explicit ProjectPageModel(QObject* parent = nullptr);

    bool isNavigatorVisible() const;
    bool isBraillePanelVisible() const;

    Q_INVOKABLE void init();

    Q_INVOKABLE QString projectToolBarName() const;
    Q_INVOKABLE QString playbackToolBarName() const;
    Q_INVOKABLE QString undoRedoToolBarName() const;
    Q_INVOKABLE QString noteInputBarName() const;
    Q_INVOKABLE QString workspacesToolBarName() const;

    Q_INVOKABLE QString tracksPanelName() const;
    Q_INVOKABLE QString historyPanelName() const;
    Q_INVOKABLE QString playbackMeterPanelName() const;

    Q_INVOKABLE QString instrumentsPanelName() const;
    Q_INVOKABLE QString inspectorPanelName() const;
    Q_INVOKABLE QString selectionFiltersPanelName() const;

    Q_INVOKABLE QString mixerPanelName() const;
    Q_INVOKABLE QString pianoKeyboardPanelName() const;
    Q_INVOKABLE QString timelinePanelName() const;
    Q_INVOKABLE QString drumsetPanelName() const;

    Q_INVOKABLE QString statusBarName() const;

signals:
    void isNavigatorVisibleChanged();
    void isBraillePanelVisibleChanged();

private:
    void onNotationChanged();

    void toggleDock(const QString& name);

    void updateDrumsetPanelVisibility();
    void updatePlaybackMeterVisibility();

    bool m_inited = false;
};
}

#endif // AU_APPSHELL_PROJECTPAGEMODEL_H
