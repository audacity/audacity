#include "tracknavigationmodel.h"

#include "global/defer.h"
#include "global/translation.h"

#include "log.h"

// #define TRACK_NAVIGATION_LOGGING_ENABLED

#ifdef TRACK_NAVIGATION_LOGGING_ENABLED
#define MYLOG() LOGI()
#else
#define MYLOG() LOGN()
#endif

using namespace au::trackedit;

static const QString makeTrackPanelName(const TrackId& trackId)
{
    return QString("Track %1 Panel").arg(trackId);
}

static const QString makeTrackHeaderPanelName(const TrackId& trackId)
{
    return QString("Track %1 Header Panel").arg(trackId);
}

static const QString makeTrackItemsPanelName(const TrackId& trackId)
{
    return QString("Track %1 Items Panel").arg(trackId);
}

//! NOTE: the default (no tracks) panel takes the order 0, tracks start right after it.
//! Every track takes a block of orders: the track itself, its header controls,
//! its clips/labels and one slot reserved for the per-track vertical ruler
static constexpr int TRACK_PANELS_ORDER_START = 1;
static constexpr int TRACK_PANELS_ORDER_STRIDE = 4;

static int trackPanelsOrderBase(int pos)
{
    return TRACK_PANELS_ORDER_START + TRACK_PANELS_ORDER_STRIDE * pos;
}

static muse::ui::INavigationControl* findFirstEnabledControl(const muse::ui::INavigationPanel* panel)
{
    int minIndex = std::numeric_limits<int>::max();
    muse::ui::INavigationControl* firstControl = nullptr;
    for (muse::ui::INavigationControl* control : panel->controls()) {
        if (!control || !control->enabled()) {
            continue;
        }

        int index = control->index().order();
        if (minIndex > index) {
            firstControl = control;
            minIndex = index;
        }
    }

    return firstControl;
}

static const muse::ui::INavigationControl* findLastEnabledControl(const muse::ui::INavigationPanel* panel)
{
    int maxIndex = std::numeric_limits<int>::min();
    const muse::ui::INavigationControl* lastControl = nullptr;
    for (muse::ui::INavigationControl* control : panel->controls()) {
        if (!control || !control->enabled()) {
            continue;
        }

        int index = control->index().order();
        if (maxIndex < index) {
            lastControl = control;
            maxIndex = index;
        }
    }

    return lastControl;
}

TrackNavigationModel::TrackNavigationModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    connect(this, &TrackNavigationModel::panelsChanged, this, &TrackNavigationModel::updateDefaultNavigationControl);
}

TrackNavigationModel::~TrackNavigationModel()
{
    navigationController()->setDefaultNavigationControl(nullptr);
}

void TrackNavigationModel::init(muse::ui::NavigationSection* section)
{
    if (!section) {
        return;
    }

    m_section = section;

    MYLOG() << "section: " << section->name().toStdString();

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        cleanup();
        load();
    });

    navigationController()->navigationChanged().onNotify(this, [this](){
        const muse::ui::INavigationPanel* activePanel = navigationController()->activePanel();
        const muse::ui::INavigationControl* activeControl = navigationController()->activeControl();

        MYLOG() << "navigation changed, panel: " << (activePanel ? activePanel->name().toStdString() : std::string("null"))
                << ", control: " << (activeControl ? activeControl->name().toStdString() : std::string("null"));

        updateNavigationActive(activePanel);

        syncFocusedItem(activePanel, activeControl);
    });
}

void TrackNavigationModel::load()
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    m_activateDefaultNavigationRequested = true;

    prj->tracksChanged().onReceive(this, [this](const std::vector<au::trackedit::Track> tracks) {
        clearPanels();

        for (size_t pos = 0; pos < tracks.size(); ++pos) {
            addPanels(tracks[pos].id, static_cast<int>(pos));
        }

        if (tracks.empty()) {
            addDefaultNavigation();
        } else {
            disableDefaultNavigation();
        }
    });

    prj->trackAdded().onReceive(this, [this](const Track& track) {
        if (m_panels.isEmpty()) {
            disableDefaultNavigation();
        }
        addPanels(track.id, m_panels.size());
        resetPanelOrder();
    });

    prj->trackRemoved().onReceive(this, [this](const Track& track) {
        removePanels(track.id);

        resetPanelOrder();

        if (m_panels.isEmpty()) {
            addDefaultNavigation();
        }
    });

    prj->trackInserted().onReceive(this, [this](const Track& track, int pos) {
        if (m_panels.isEmpty()) {
            disableDefaultNavigation();
        }
        addPanels(track.id, pos);
        resetPanelOrder();
    });

    prj->trackMoved().onReceive(this, [this](const Track& track, int pos) {
        const int from = indexOfTrack(track.id);
        if (from < 0 || from == pos) {
            return;
        }

        m_panels.move(from, pos);

        resetPanelOrder();
    });

    const auto trackList = prj->trackList();

    MYLOG() << "load, tracks: " << trackList.size();

    for (const auto& track : trackList) {
        addPanels(track.id, m_panels.size());
    }

    if (trackList.empty()) {
        addDefaultNavigation();
    }

    updateDefaultNavigationControl();

    tracksNavigationController()->focusedTrackChanged().onReceive(this, [this](const TrackId& trackId, bool highlight) {
        MYLOG() << "focused track changed: " << trackId << ", highlight: " << highlight;

        if (m_isFocusSyncing) {
            MYLOG() << "skipped, the focus is set from the navigation";
            return;
        }

        if (tracksNavigationController()->focusedItem().itemId != INVALID_TRACK_ITEM) {
            MYLOG() << "skipped, the focused item is set: " << tracksNavigationController()->focusedItem().itemId;
            return;
        }

        QTimer::singleShot(10, [this, trackId, highlight](){
            if (isNavigationOnTrack(trackId)) {
                MYLOG() << "skipped, the navigation is already on the track " << trackId;
                return;
            }

            activateNavigation(trackId, highlight);
        });
    }, muse::async::Asyncable::Mode::SetReplace);

    tracksNavigationController()->focusedItemChanged().onReceive(this, [this](const TrackItemKey& itemKey, bool highlight) {
        MYLOG() << "focused item changed, track: " << itemKey.trackId << ", item: " << itemKey.itemId
                << ", highlight: " << highlight;

        if (m_isFocusSyncing) {
            MYLOG() << "skipped, the focus is set from the navigation";
            return;
        }

        QTimer::singleShot(10, [this, itemKey, highlight](){
            activateNavigation(itemKey, highlight);
        });
    }, muse::async::Asyncable::Mode::SetReplace);
}

void TrackNavigationModel::addPanels(const TrackId& trackId, int pos)
{
    const int orderBase = trackPanelsOrderBase(pos);

    MYLOG() << "track: " << trackId << ", pos: " << pos << ", order base: " << orderBase;

    muse::ui::NavigationPanel* trackPanel = makePanel(makeTrackPanelName(trackId), orderBase);

    connect(trackPanel, &muse::ui::NavigationPanel::navigationEvent, this,
            [this, trackId](muse::ui::NavigationEvent* event) {
        if (event->type() == muse::ui::NavigationEvent::AboutActive) {
            if (tracksNavigationController()->focusedTrack() != trackId) {
                tracksNavigationController()->setFocusedTrack(trackId);
            }
            return;
        }
    });

    trackPanel->controlsListChanged().onNotify(this, [this]() {
        updateDefaultNavigationControl();
    });

    muse::ui::NavigationPanel* headerPanel = makePanel(makeTrackHeaderPanelName(trackId), orderBase + 1);

    connect(headerPanel, &muse::ui::NavigationPanel::navigationEvent, this,
            [this, trackId](muse::ui::NavigationEvent* event) {
        if (event->type() == muse::ui::NavigationEvent::Escape) {
            //! NOTE: Escape from a track header control returns the focus to the track container
            event->setAccepted(true);
            activateNavigation(trackId, true /*highlight*/);
        }
    });

    muse::ui::NavigationPanel* itemsPanel = makePanel(makeTrackItemsPanelName(trackId), orderBase + 2);

    connect(itemsPanel, &muse::ui::NavigationPanel::navigationEvent, this,
            [this, itemsPanel](muse::ui::NavigationEvent* event) {
        if (event->type() == muse::ui::NavigationEvent::AboutActive) {
            //! NOTE: navigation system activates the first control of a panel it enters. When the clips/labels
            //! panel is entered while navigating backwards (Shift+Tab), the navigation should
            //! instead land on the last item, so ask navigation system to activate it explicitly
            const bool backwards = itemsPanel->index().order() < m_lastActivePanelOrder;
            if (backwards) {
                if (const muse::ui::INavigationControl* last = findLastEnabledControl(itemsPanel)) {
                    event->setData("controlName", last->name());
                }
            }
            return;
        }
    });

    m_panels.insert(pos, { trackId, trackPanel, headerPanel, itemsPanel });

    emit panelsChanged();
}

muse::ui::NavigationPanel* TrackNavigationModel::makePanel(const QString& name, int order)
{
    muse::ui::NavigationPanel* panel = new muse::ui::NavigationPanel(this);
    panel->setName(name);
    panel->setIndex({ order, 0 });
    panel->setOrder(order);
    panel->setSection(m_section);
    panel->componentComplete();

    return panel;
}

void TrackNavigationModel::resetPanelOrder()
{
    for (int i = 0; i < m_panels.size(); ++i) {
        const TrackPanels& panels = m_panels.at(i);
        const int orderBase = trackPanelsOrderBase(i);

        panels.track->setOrder(orderBase);
        panels.header->setOrder(orderBase + 1);
        panels.items->setOrder(orderBase + 2);
    }

    emit panelsChanged();
}

bool TrackNavigationModel::isNavigationOnTrack(const TrackId& trackId) const
{
    const muse::ui::INavigationPanel* activePanel = navigationController()->activePanel();
    if (!activePanel) {
        return false;
    }

    const int pos = indexOfTrack(trackId);
    if (pos < 0) {
        return false;
    }

    const TrackPanels& panels = m_panels.at(pos);

    return activePanel == panels.track || activePanel == panels.header || activePanel == panels.items;
}

int TrackNavigationModel::indexOfTrack(const TrackId& trackId) const
{
    for (int i = 0; i < m_panels.size(); ++i) {
        if (m_panels.at(i).trackId == trackId) {
            return i;
        }
    }

    return -1;
}

void TrackNavigationModel::addDefaultNavigation()
{
    if (!m_section) {
        return;
    }

    if (!m_defaultPanel) {
        m_defaultPanel = new muse::ui::NavigationPanel(this);
        m_defaultPanel->setName("Default Track Panel");
        m_defaultPanel->setIndex({ 0, 0 });
        m_defaultPanel->setOrder(0);
        m_defaultPanel->setSection(m_section);
        m_defaultPanel->componentComplete();

        m_defaultControl = new muse::ui::NavigationControl(this);
        m_defaultControl->setName("Default Track Control");
        m_defaultControl->setIndex({ 0, 0 });
        m_defaultControl->setOrder(0);
        m_defaultControl->setPanel(m_defaultPanel);
        m_defaultControl->componentComplete();

        muse::ui::AccessibleItem* accessible = m_defaultControl->accessible();
        accessible->setRole(muse::ui::MUAccessible::Information);
        accessible->setName(muse::qtrc("trackedit", "Tracks: Empty"));
        accessible->setVisualItem(m_trackViewItem);
        accessible->componentComplete();

        emit defaultNavigationControlChanged();
    }

    MYLOG() << "the project has no tracks, the default control is enabled";

    m_defaultPanel->setEnabled(true);
    m_defaultControl->setEnabled(true);

    navigationController()->requestActivateByName(
        m_section->name().toStdString(),
        m_defaultPanel->name().toStdString(),
        m_defaultControl->name().toStdString());
}

void TrackNavigationModel::disableDefaultNavigation()
{
    if (!m_defaultPanel) {
        return;
    }

    MYLOG() << "the default control is disabled";

    m_defaultPanel->setEnabled(false);
    m_defaultControl->setEnabled(false);
}

muse::ui::NavigationControl* TrackNavigationModel::fallbackNavigationControl() const
{
    return m_fallbackNavigationControl;
}

void TrackNavigationModel::setFallbackNavigationControl(muse::ui::NavigationControl* control)
{
    if (m_fallbackNavigationControl == control) {
        return;
    }

    m_fallbackNavigationControl = control;

    if (control) {
        connect(control, &QObject::destroyed, this, &TrackNavigationModel::updateDefaultNavigationControl,
                Qt::UniqueConnection);
    }

    emit fallbackNavigationControlChanged();

    updateDefaultNavigationControl();
}

muse::ui::NavigationControl* TrackNavigationModel::defaultNavigationControl() const
{
    return m_defaultControl;
}

QQuickItem* TrackNavigationModel::trackViewItem() const
{
    return m_trackViewItem;
}

void TrackNavigationModel::setTrackViewItem(QQuickItem* item)
{
    if (m_trackViewItem == item) {
        return;
    }

    m_trackViewItem = item;

    if (m_defaultControl) {
        m_defaultControl->accessible()->setVisualItem(item);
    }

    emit trackViewItemChanged();
}

void TrackNavigationModel::activateDefaultNavigation()
{
    m_activateDefaultNavigationRequested = true;

    updateDefaultNavigationControl();
}

void TrackNavigationModel::updateDefaultNavigationControl()
{
    //! NOTE: the default navigation control is the control the navigation returns to
    //! (Escape, reset of the navigation): the first track if there are tracks,
    //! the fallback control otherwise
    muse::ui::INavigationControl* trackControl = m_panels.isEmpty() ? nullptr : findFirstEnabledControl(m_panels.first().track);
    muse::ui::INavigationControl* control = trackControl ? trackControl : m_fallbackNavigationControl.data();

    MYLOG() << "default control: " << (control ? control->name().toStdString() : std::string("null"));

    navigationController()->setDefaultNavigationControl(control);

    if (!m_activateDefaultNavigationRequested || !control) {
        return;
    }

    //! NOTE: QML creates the control of a track later than the model creates its panels,
    //! so the requested activation waits for the first track instead of landing on the fallback control
    if (!m_panels.isEmpty() && !trackControl) {
        return;
    }

    m_activateDefaultNavigationRequested = false;

    activateNavigation(control);
}

void TrackNavigationModel::updateNavigationActive(const muse::ui::INavigationPanel* activePanel)
{
    //! NOTE: only the header controls panel is navigated as a usual panel (general navigation):
    //! Left/Right move between the controls and the trigger (Space) presses the focused control.
    //! The track panel and the clips/labels panel belong to the project: Left/Right move the play
    //! cursor, Up/Down navigate the tracks, the trigger starts the playback.
    bool navigationActive = false;
    for (const TrackPanels& panels : m_panels) {
        if (panels.header == activePanel) {
            navigationActive = true;
            break;
        }

        if (panels.track == activePanel || panels.items == activePanel) {
            navigationActive = false;
            break;
        }
    }

    MYLOG() << "navigation active: " << navigationActive;

    tracksNavigationController()->setIsNavigationActive(navigationActive);
}

void TrackNavigationModel::syncFocusedItem(const muse::ui::INavigationPanel* activePanel,
                                           const muse::ui::INavigationControl* activeControl)
{
    //! NOTE: the navigation may be moved by the navigation controller itself (Tab, mouse),
    //! so the focused item of the tracks controller is taken from the active panel/control.
    //! Setting an item of the already focused track sends the item notification only,
    //! so this doesn't fight with the activation done on focus changes
    m_isFocusSyncing = true;
    DEFER {
        m_isFocusSyncing = false;
    };

    for (const TrackPanels& panels : m_panels) {
        if (panels.items == activePanel) {
            m_lastActivePanelOrder = activePanel->index().order();

            const TrackItemId itemId = activeControl ? activeControl->name().toLongLong() : INVALID_TRACK_ITEM;

            MYLOG() << "the items panel is active, track: " << panels.trackId << ", item: " << itemId;

            tracksNavigationController()->setFocusedItem({ panels.trackId, itemId });
            return;
        }

        if (panels.track == activePanel || panels.header == activePanel) {
            m_lastActivePanelOrder = activePanel->index().order();

            MYLOG() << "the " << (panels.track == activePanel ? "track" : "header") << " panel is active, track: "
                    << panels.trackId;

            tracksNavigationController()->setFocusedItem({ panels.trackId, INVALID_TRACK_ITEM });
            return;
        }
    }
}

void TrackNavigationModel::moveFocusTo(const QVariant& trackId)
{
    tracksNavigationController()->setFocusedTrack(trackId.toInt(), false /*highlight*/);
}

void TrackNavigationModel::cleanup()
{
    MYLOG() << "====";

    disableDefaultNavigation();

    clearPanels();

    //! NOTE: we disabled default navigation, we deleted all panels, so let's deactivate the section
    if (m_section && m_section->active()) {
        m_section->setActive(false);
    }

    navigationController()->setDefaultNavigationControl(nullptr);
}

void TrackNavigationModel::clearPanels()
{
    MYLOG() << "panels of tracks: " << m_panels.size();

    for (const TrackPanels& panels : m_panels) {
        deletePanels(panels);
    }

    m_panels.clear();

    emit panelsChanged();
}

void TrackNavigationModel::removePanels(const TrackId& trackId)
{
    MYLOG() << "track: " << trackId;

    const int pos = indexOfTrack(trackId);
    if (pos < 0) {
        return;
    }

    deletePanels(m_panels.takeAt(pos));

    emit panelsChanged();
}

void TrackNavigationModel::deletePanels(const TrackPanels& panels)
{
    for (muse::ui::NavigationPanel* panel : { panels.track, panels.header, panels.items }) {
        panel->setSection(nullptr);
        panel->deleteLater();
    }
}

QList<muse::ui::NavigationPanel*> TrackNavigationModel::trackItemPanels() const
{
    return panelsList(&TrackPanels::track);
}

QList<muse::ui::NavigationPanel*> TrackNavigationModel::trackHeaderPanels() const
{
    return panelsList(&TrackPanels::header);
}

QList<muse::ui::NavigationPanel*> TrackNavigationModel::viewItemPanels() const
{
    return panelsList(&TrackPanels::items);
}

QList<muse::ui::NavigationPanel*> TrackNavigationModel::panelsList(muse::ui::NavigationPanel* TrackPanels::* panel) const
{
    QList<muse::ui::NavigationPanel*> result;
    result.reserve(m_panels.size());

    for (const TrackPanels& panels : m_panels) {
        result.append(panels.*panel);
    }

    return result;
}

void TrackNavigationModel::activateNavigation(const TrackId& trackId, bool highlight)
{
    if (!m_section) {
        return;
    }

    if (trackId == INVALID_TRACK) {
        return;
    }

    const int pos = indexOfTrack(trackId);
    if (pos < 0) {
        return;
    }

    const muse::ui::NavigationPanel* targetPanel = m_panels.at(pos).track;

    const muse::ui::INavigationControl* firstControl = findFirstEnabledControl(targetPanel);
    if (!firstControl) {
        return;
    }

    if (firstControl->active()) {
        return;
    }

    MYLOG() << "activate the track " << trackId << ", control: " << firstControl->name().toStdString()
            << ", highlight: " << highlight;

    navigationController()->setIsHighlight(highlight);
    navigationController()->requestActivateByName(
        m_section->name().toStdString(),
        targetPanel->name().toStdString(),
        firstControl->name().toStdString()
        );
}

void TrackNavigationModel::activateNavigation(const TrackItemKey& itemKey, bool highlight)
{
    if (!m_section) {
        return;
    }

    if (!itemKey.isValid()) {
        return;
    }

    const int pos = indexOfTrack(itemKey.trackId);
    if (pos < 0) {
        return;
    }

    const muse::ui::NavigationPanel* targetPanel = m_panels.at(pos).items;

    const auto controls = targetPanel->controls();
    for (auto* control : controls) {
        if (control && control->name() == QString::number(itemKey.itemId)) {
            if (control->active()) {
                return;
            }

            MYLOG() << "activate the item " << itemKey.itemId << " of the track " << itemKey.trackId
                    << ", highlight: " << highlight;

            navigationController()->setIsHighlight(highlight);
            navigationController()->requestActivateByName(
                m_section->name().toStdString(),
                targetPanel->name().toStdString(),
                control->name().toStdString()
                );
            return;
        }
    }
}

void TrackNavigationModel::activateNavigation(const muse::ui::INavigationControl* control, bool highlight)
{
    if (!control || control->active()) {
        return;
    }

    const muse::ui::INavigationPanel* panel = control->panel();
    const muse::ui::INavigationSection* section = panel ? panel->section() : nullptr;
    if (!section) {
        return;
    }

    MYLOG() << "activate the control " << control->name().toStdString() << " of the panel "
            << panel->name().toStdString() << ", highlight: " << highlight;

    navigationController()->setIsHighlight(highlight);
    navigationController()->requestActivateByName(
        section->name().toStdString(),
        panel->name().toStdString(),
        control->name().toStdString()
        );
}
