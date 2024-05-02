/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_APPSHELL_PROJECTPAGEMODEL_H
#define AU_APPSHELL_PROJECTPAGEMODEL_H

#include <QQuickItem>

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "actions/actionable.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "iappshellconfiguration.h"
#include "dockwindow/idockwindowprovider.h"

//! TODO AU4
// #include "braille/ibrailleconfiguration.h"

namespace au::appshell {
class ProjectPageModel : public QObject, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<muse::dock::IDockWindowProvider> dockWindowProvider;
    muse::Inject<IAppShellConfiguration> configuration;

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

    Q_INVOKABLE QString tracksPanelName() const;

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
};
}

#endif // AU_APPSHELL_PROJECTPAGEMODEL_H
