#pragma once

#include <QPointer>
#include <QQuickItem>

#include "ui/qml/Muse/Ui/navigationsection.h"
#include "ui/qml/Muse/Ui/navigationpanel.h"
#include "ui/qml/Muse/Ui/navigationcontrol.h"
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

    //! NOTE: three panels per track, in the tab order:
    //! the track itself, the controls of its header, its clips/labels
    Q_PROPERTY(QList<muse::ui::NavigationPanel*> trackItemPanels READ trackItemPanels NOTIFY panelsChanged)
    Q_PROPERTY(QList<muse::ui::NavigationPanel*> trackHeaderPanels READ trackHeaderPanels NOTIFY panelsChanged)
    Q_PROPERTY(QList<muse::ui::NavigationPanel*> viewItemPanels READ viewItemPanels NOTIFY panelsChanged)

    //! NOTE: the control the page provides to become its default navigation control
    //! when the project has no tracks
    Q_PROPERTY(
        muse::ui::NavigationControl
        * fallbackNavigationControl READ fallbackNavigationControl WRITE setFallbackNavigationControl NOTIFY fallbackNavigationControlChanged)

    Q_PROPERTY(QQuickItem * trackViewItem READ trackViewItem WRITE setTrackViewItem NOTIFY trackViewItemChanged)
    Q_PROPERTY(
        muse::ui::NavigationControl * defaultNavigationControl READ defaultNavigationControl NOTIFY defaultNavigationControlChanged)

public:
    explicit TrackNavigationModel(QObject* parent = nullptr);
    ~TrackNavigationModel() override;

    Q_INVOKABLE void init(muse::ui::NavigationSection* section);

    Q_INVOKABLE void moveFocusTo(const QVariant& trackId);

    Q_INVOKABLE void activateDefaultNavigation();

    QList<muse::ui::NavigationPanel*> trackItemPanels() const;
    QList<muse::ui::NavigationPanel*> trackHeaderPanels() const;
    QList<muse::ui::NavigationPanel*> viewItemPanels() const;

    muse::ui::NavigationControl* fallbackNavigationControl() const;
    void setFallbackNavigationControl(muse::ui::NavigationControl* control);

    QQuickItem* trackViewItem() const;
    void setTrackViewItem(QQuickItem* item);

    muse::ui::NavigationControl* defaultNavigationControl() const;

signals:
    void panelsChanged();
    void fallbackNavigationControlChanged();
    void trackViewItemChanged();
    void defaultNavigationControlChanged();

private:
    struct TrackPanels
    {
        TrackId trackId = INVALID_TRACK;

        muse::ui::NavigationPanel* track = nullptr;
        muse::ui::NavigationPanel* header = nullptr;
        muse::ui::NavigationPanel* items = nullptr;
    };

    void load();
    void cleanup();
    void clearPanels();

    muse::ui::NavigationPanel* makePanel(const QString& name, int order);
    void addPanels(const TrackId& trackId, int pos);
    void removePanels(const TrackId& trackId);
    void resetPanelOrder();
    void deletePanels(const TrackPanels& panels);
    int indexOfTrack(const TrackId& trackId) const;
    bool isNavigationOnTrack(const TrackId& trackId) const;
    QList<muse::ui::NavigationPanel*> panelsList(muse::ui::NavigationPanel* TrackPanels::* panel) const;
    void updateNavigationActive(const muse::ui::INavigationPanel* activePanel);
    void syncFocusedItem(const muse::ui::INavigationPanel* activePanel, const muse::ui::INavigationControl* activeControl);

    void addDefaultNavigation();
    void disableDefaultNavigation();

    void updateDefaultNavigationControl();

    void activateNavigation(const TrackId& trackId, bool highlight = false);
    void activateNavigation(const TrackItemKey& itemKey, bool highlight = false);
    void activateNavigation(const muse::ui::INavigationControl* control, bool highlight = false);

    muse::ui::INavigationSection* m_section = nullptr;

    muse::ui::NavigationPanel* m_defaultPanel = nullptr;
    muse::ui::NavigationControl* m_defaultControl = nullptr;

    QList<TrackPanels> m_panels;

    QPointer<muse::ui::NavigationControl> m_fallbackNavigationControl;
    QPointer<QQuickItem> m_trackViewItem;

    bool m_activateDefaultNavigationRequested = false;
    bool m_isFocusSyncing = false;

    int m_lastActivePanelOrder = -1;
};
}
