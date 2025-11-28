#pragma once

#include "ui/qml/Muse/Ui/navigationsection.h"
#include "ui/qml/Muse/Ui/navigationpanel.h"
#include "global/async/asyncable.h"

#include "global/modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"
#include "ui/inavigationcontroller.h"

namespace au::trackedit {
class TrackNavigationModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<muse::ui::INavigationController> navigationController;

    Q_PROPERTY(QList<muse::ui::NavigationPanel*> trackItemPanels READ trackItemPanels NOTIFY trackItemPanelsChanged)
    Q_PROPERTY(QList<muse::ui::NavigationPanel*> clipItemPanels READ clipItemPanels NOTIFY clipItemPanelsChanged)

public:
    explicit TrackNavigationModel(QObject* parent = nullptr);

    Q_INVOKABLE void init(muse::ui::NavigationSection* section);
    Q_INVOKABLE void requestActivateByIndex(int index);
    Q_INVOKABLE void moveFocusTo(int index);

    QList<muse::ui::NavigationPanel*> trackItemPanels() const;
    QList<muse::ui::NavigationPanel*> clipItemPanels() const;

signals:
    void trackItemPanelsChanged();
    void clipItemPanelsChanged();

private:
    void load();
    void cleanup();
    void clearPanels();

    void addPanels(trackedit::TrackId trackId, int pos);
    void resetPanelOrder();
    void addDefaultNavigation();

    muse::ui::NavigationControl* m_default_control = nullptr;
    muse::ui::NavigationPanel* m_default_panel = nullptr;
    muse::ui::NavigationSection* m_default_section = nullptr;

    muse::ui::INavigationSection* m_section = nullptr;

    QList<muse::ui::NavigationPanel*> m_trackItemPanels;
    QList<muse::ui::NavigationPanel*> m_clipItemPanels;
};
}
