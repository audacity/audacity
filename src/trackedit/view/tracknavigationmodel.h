#pragma once

#include "ui/qml/Muse/Ui/navigationsection.h"
#include "ui/qml/Muse/Ui/navigationpanel.h"
#include "global/async/asyncable.h"

#include "global/modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"
#include "ui/inavigationcontroller.h"
#include "trackedit/internal/itracknavigationcontroller.h"

namespace au::trackedit {
class TrackNavigationModel : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::Inject<muse::ui::INavigationController> navigationController{ this };
    muse::Inject<ITrackNavigationController> tracksNavigationController{ this };

    Q_PROPERTY(QList<muse::ui::NavigationPanel*> trackItemPanels READ trackItemPanels NOTIFY trackItemPanelsChanged)
    Q_PROPERTY(QList<muse::ui::NavigationPanel*> viewItemPanels READ viewItemPanels NOTIFY viewItemPanelsChanged)

public:
    explicit TrackNavigationModel(QObject* parent = nullptr);

    Q_INVOKABLE void init(muse::ui::NavigationSection* section);

    Q_INVOKABLE void moveFocusTo(const QVariant& trackId);

    QList<muse::ui::NavigationPanel*> trackItemPanels() const;
    QList<muse::ui::NavigationPanel*> viewItemPanels() const;

signals:
    void trackItemPanelsChanged();
    void viewItemPanelsChanged();

private:
    void load();
    void cleanup();
    void clearPanels();

    void addPanels(const TrackId& trackId, int pos);
    void resetPanelOrder();
    void addDefaultNavigation();

    void activateNavigation(const TrackId& trackId, bool highlight = false);
    void activateNavigation(const TrackItemKey& itemKey, bool highlight = false);

    muse::ui::NavigationControl* m_default_control = nullptr;
    muse::ui::NavigationPanel* m_default_panel = nullptr;
    muse::ui::NavigationSection* m_default_section = nullptr;

    muse::ui::INavigationSection* m_section = nullptr;

    QList<muse::ui::NavigationPanel*> m_trackItemPanels;
    QList<muse::ui::NavigationPanel*> m_viewItemPanels;
};
}
