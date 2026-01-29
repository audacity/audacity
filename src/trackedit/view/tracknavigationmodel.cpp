#include "tracknavigationmodel.h"

using namespace au::trackedit;

static const QString makeTrackPanelName(const TrackId& trackId)
{
    return QString("Track %1 Panel").arg(trackId);
}

static const QString makeTrackItemsPanelName(const TrackId& trackId)
{
    return QString("Track %1 Items Panel").arg(trackId);
}

static const muse::ui::INavigationControl* findFirstEnabledControl(const muse::ui::INavigationPanel* panel)
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

TrackNavigationModel::TrackNavigationModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void TrackNavigationModel::init(muse::ui::NavigationSection* section)
{
    if (!section) {
        return;
    }

    m_section = section;

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        cleanup();
        load();
    });

    navigationController()->navigationChanged().onNotify(this, [this](){
        const muse::ui::INavigationPanel* activePanel = navigationController()->activePanel();
        const muse::ui::INavigationControl* activeControl = navigationController()->activeControl();

        if (m_trackItemPanels.contains(activePanel)) {
            const muse::ui::INavigationControl* firstControl = findFirstEnabledControl(activePanel);
            tracksNavigationController()->setIsNavigationActive(firstControl == activeControl);
        }
    });
}

void TrackNavigationModel::load()
{
    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    prj->tracksChanged().onReceive(this, [this](const std::vector<au::trackedit::Track> tracks) {
        clearPanels();

        for (size_t pos = 0; pos < tracks.size(); ++pos) {
            addPanels(tracks[pos].id, static_cast<int>(pos));
        }
    });

    prj->trackAdded().onReceive(this, [this](const Track& track) {
        const int pos = m_trackItemPanels.size();
        addPanels(track.id, pos);
        resetPanelOrder();
    });

    prj->trackRemoved().onReceive(this, [this](const Track& track) {
        QString trackPanelName = makeTrackPanelName(track.id);
        for (int i = 0; i < m_trackItemPanels.size(); ++i) {
            if (m_trackItemPanels.at(i)->name() == trackPanelName) {
                muse::ui::NavigationPanel* trackPanel = m_trackItemPanels.takeAt(i);
                trackPanel->setSection(nullptr);
                trackPanel->deleteLater();
                break;
            }
        }

        QString trackItemsPanelName = makeTrackItemsPanelName(track.id);
        for (int i = 0; i < m_viewItemPanels.size(); ++i) {
            if (m_viewItemPanels.at(i)->name() == trackItemsPanelName) {
                muse::ui::NavigationPanel* itemsPanel = m_viewItemPanels.takeAt(i);
                itemsPanel->setSection(nullptr);
                itemsPanel->deleteLater();
                break;
            }
        }

        resetPanelOrder();
    });

    prj->trackInserted().onReceive(this, [this](const Track& track, int pos) {
        addPanels(track.id, pos);
        resetPanelOrder();
    });

    prj->trackMoved().onReceive(this, [this](const Track& track, int pos) {
        for (int i = 0; i < m_trackItemPanels.size(); ++i) {
            if (m_trackItemPanels.at(i)->name() == makeTrackPanelName(track.id)) {
                auto trackPanel = m_trackItemPanels.takeAt(i);
                m_trackItemPanels.insert(pos, trackPanel);
                break;
            }
        }

        QString trackItemsPanelName = makeTrackItemsPanelName(track.id);
        for (int i = 0; i < m_viewItemPanels.size(); ++i) {
            if (m_viewItemPanels.at(i)->name() == trackItemsPanelName) {
                auto itemsPanel = m_viewItemPanels.takeAt(i);
                m_viewItemPanels.insert(pos, itemsPanel);
                break;
            }
        }

        resetPanelOrder();
    });

    const auto trackList = prj->trackList();
    for (const auto& track : trackList) {
        const int pos = m_trackItemPanels.size();
        addPanels(track.id, pos);
    }

    addDefaultNavigation();

    navigationController()->requestActivateByName(m_default_section->name().toStdString(),
                                                  m_default_panel->name().toStdString(), m_default_control->name().toStdString());

    tracksNavigationController()->focusedTrackChanged().onReceive(this, [this](const TrackId& trackId, bool highlight) {
        QTimer::singleShot(10, [this, trackId, highlight](){
            activateNavigation(trackId, highlight);
        });
    }, muse::async::Asyncable::Mode::SetReplace);

    tracksNavigationController()->focusedItemChanged().onReceive(this, [this](const TrackItemKey& itemKey, bool highlight) {
        QTimer::singleShot(10, [this, itemKey, highlight](){
            activateNavigation(itemKey, highlight);
        });
    }, muse::async::Asyncable::Mode::SetReplace);
}

void TrackNavigationModel::addPanels(const TrackId& trackId, int pos)
{
    muse::ui::NavigationPanel* trackPanel = new muse::ui::NavigationPanel(this);
    trackPanel->setName(makeTrackPanelName(trackId));
    trackPanel->setIndex({ 2 * pos, 0 });
    trackPanel->setOrder(2 * pos);
    trackPanel->setSection(m_section);
    trackPanel->componentComplete();

    connect(trackPanel, &muse::ui::NavigationPanel::navigationEvent, this,
            [this, trackId](muse::ui::NavigationEvent* event) {
        if (event->type() != muse::ui::NavigationEvent::AboutActive) {
            return;
        }

        if (tracksNavigationController()->focusedTrack() != trackId) {
            tracksNavigationController()->setFocusedTrack(trackId);
        }
    });

    muse::ui::NavigationPanel* itemsPanel = new muse::ui::NavigationPanel(this);
    itemsPanel->setName(makeTrackItemsPanelName(trackId));
    itemsPanel->setIndex({ 2 * pos + 1, 0 });
    itemsPanel->setOrder(2 * pos + 1);
    itemsPanel->setSection(m_section);
    itemsPanel->componentComplete();

    m_trackItemPanels.append(trackPanel);
    m_viewItemPanels.append(itemsPanel);

    emit trackItemPanelsChanged();
    emit viewItemPanelsChanged();
}

void TrackNavigationModel::resetPanelOrder()
{
    for (int i = 0; i < m_trackItemPanels.size(); ++i) {
        m_trackItemPanels.at(i)->setOrder(2 * i);
    }

    for (int i = 0; i < m_viewItemPanels.size(); ++i) {
        m_viewItemPanels.at(i)->setOrder(2 * i + 1);
    }

    emit trackItemPanelsChanged();
    emit viewItemPanelsChanged();
}

void TrackNavigationModel::moveFocusTo(const QVariant& trackId)
{
    tracksNavigationController()->setFocusedTrack(trackId.toInt(), false /*highlight*/);
}

void TrackNavigationModel::addDefaultNavigation()
{
    if (!m_default_section) {
        m_default_section = new muse::ui::NavigationSection(this);
        m_default_section->setName("Main Section");
        m_default_section->setIndex({ 0, 0 });
        m_default_section->setOrder(1000); //! so as not to conflict with other sections
        m_default_section->componentComplete();

        m_default_panel = new muse::ui::NavigationPanel(this);
        m_default_panel->setName("Main Panel");
        m_default_panel->setIndex({ 0, 0 });
        m_default_panel->setOrder(0);
        m_default_panel->setSection(m_default_section);
        m_default_panel->componentComplete();

        m_default_control = new muse::ui::NavigationControl(this);
        m_default_control->setName("Main Control");
        m_default_control->setIndex({ 0, 0 });
        m_default_control->setOrder(0);
        m_default_control->setPanel(m_default_panel);
        m_default_control->componentComplete();

        navigationController()->reg(m_default_section);
    }

    navigationController()->setDefaultNavigationControl(m_default_control);
}

void TrackNavigationModel::cleanup()
{
    if (m_default_section) {
        navigationController()->setDefaultNavigationControl(nullptr);
    }

    clearPanels();

    navigationController()->resetNavigation();
}

void TrackNavigationModel::clearPanels()
{
    for (auto& panel : m_trackItemPanels) {
        panel->setSection(nullptr);
        panel->deleteLater();
    }
    m_trackItemPanels.clear();

    for (auto& panel : m_viewItemPanels) {
        panel->setSection(nullptr);
        panel->deleteLater();
    }
    m_viewItemPanels.clear();
}

QList<muse::ui::NavigationPanel*> TrackNavigationModel::trackItemPanels() const
{
    return m_trackItemPanels;
}

QList<muse::ui::NavigationPanel*> TrackNavigationModel::viewItemPanels() const
{
    return m_viewItemPanels;
}

void TrackNavigationModel::activateNavigation(const TrackId& trackId, bool highlight)
{
    if (!m_section) {
        return;
    }

    if (trackId == INVALID_TRACK) {
        return;
    }

    QString panelName = makeTrackPanelName(trackId);

    muse::ui::NavigationPanel* targetPanel = nullptr;
    for (auto* panel : m_trackItemPanels) {
        if (panel->name() == panelName) {
            targetPanel = panel;
            break;
        }
    }

    if (!targetPanel) {
        return;
    }

    const muse::ui::INavigationControl* firstControl = findFirstEnabledControl(targetPanel);
    if (!firstControl) {
        return;
    }

    if (firstControl->active()) {
        return;
    }

    navigationController()->setIsHighlight(highlight);
    navigationController()->requestActivateByName(
        m_section->name().toStdString(),
        panelName.toStdString(),
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

    QString panelName = makeTrackItemsPanelName(itemKey.trackId);

    muse::ui::NavigationPanel* targetPanel = nullptr;
    for (auto* panel : m_viewItemPanels) {
        if (panel->name() == panelName) {
            targetPanel = panel;
            break;
        }
    }

    if (!targetPanel) {
        return;
    }

    const auto controls = targetPanel->controls();
    for (auto* control : controls) {
        if (control && control->name() == QString::number(itemKey.itemId)) {
            if (control->active()) {
                return;
            }

            navigationController()->setIsHighlight(highlight);
            navigationController()->requestActivateByName(
                m_section->name().toStdString(),
                panelName.toStdString(),
                control->name().toStdString()
                );
            return;
        }
    }
}
