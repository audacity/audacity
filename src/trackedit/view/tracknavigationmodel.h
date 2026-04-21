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
class TrackNavigationModel : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::ContextInject<muse::ui::INavigationController> navigationController{ this };
    muse::ContextInject<ITrackNavigationController> tracksNavigationController{ this };

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
    void handleArrowKeyFallback(muse::ui::NavigationEvent* event);

    void activateNavigation(const TrackId& trackId, bool highlight = false);
    void activateNavigation(const TrackItemKey& itemKey, bool highlight = false);

    muse::ui::INavigationSection* m_section = nullptr;

    QList<muse::ui::NavigationPanel*> m_trackItemPanels;
    QList<muse::ui::NavigationPanel*> m_viewItemPanels;
};
}
