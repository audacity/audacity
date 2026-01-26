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

    tracksNavigationController()->focusedTrackChanged().onNotify(this, [this]() {
        QTimer::singleShot(10, [this](){
            activateNavigationForFocusedTrack();
        });
    }, muse::async::Asyncable::Mode::SetReplace);

    tracksNavigationController()->focusedItemChanged().onNotify(this, [this]() {
        QTimer::singleShot(10, [this](){
            activateNavigationForFocusedItem();
        });
    }, muse::async::Asyncable::Mode::SetReplace);
}

void TrackNavigationModel::addPanels(trackedit::TrackId trackId, int pos)
{
    muse::ui::NavigationPanel* trackPanel = new muse::ui::NavigationPanel(this);
    trackPanel->setName(makeTrackPanelName(trackId));
    trackPanel->setIndex({ 2 * pos, 0 });
    trackPanel->setOrder(2 * pos);
    trackPanel->setSection(m_section);
    trackPanel->componentComplete();

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

void TrackNavigationModel::requestActivateByIndex(int index)
{
    if (index < 0 || index >= m_trackItemPanels.size()) {
        return;
    }

    const auto& panel = m_trackItemPanels.at(index);
    if (!panel) {
        return;
    }

    const auto firstControl = panel->controls().begin();
    if (!(*firstControl)) {
        return;
    }

    navigationController()->setIsResetOnMousePress(false);
    navigationController()->setIsHighlight(true);
    navigationController()->requestActivateByName(m_section->name().toStdString(), panel->name().toStdString(),
                                                  (*firstControl)->name().toStdString());
}

void TrackNavigationModel::moveFocusTo(const QVariant& trackId)
{
    tracksNavigationController()->setFocusedTrack(trackId.toInt());
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

void TrackNavigationModel::activateNavigationForFocusedTrack()
{
    if (!m_section) {
        return;
    }

    TrackId trackId = tracksNavigationController()->focusedTrack();
    if (trackId == INVALID_TRACK) {
        return;
    }

    QString panelName = makeTrackPanelName(trackId);

    muse::ui::INavigationPanel* activePanel = navigationController()->activePanel();
    if (activePanel && activePanel->name() == panelName) {
        return;
    }

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

    const auto controls = targetPanel->controls();
    if (controls.empty()) {
        return;
    }

    const auto firstControl = controls.begin();
    if (!(*firstControl)) {
        return;
    }

    navigationController()->setIsResetOnMousePress(false);
    navigationController()->setIsHighlight(true);
    navigationController()->requestActivateByName(
        m_section->name().toStdString(),
        panelName.toStdString(),
        (*firstControl)->name().toStdString()
        );
}

void TrackNavigationModel::activateNavigationForFocusedItem()
{
    if (!m_section) {
        return;
    }

    TrackItemKey focusedItem = tracksNavigationController()->focusedItem();

    if (!focusedItem.isValid()) {
        return;
    }

    QString panelName = makeTrackItemsPanelName(focusedItem.trackId);

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
        if (control && control->name() == QString::number(focusedItem.itemId)) {
            navigationController()->setIsResetOnMousePress(false);
            navigationController()->setIsHighlight(true);
            navigationController()->requestActivateByName(
                m_section->name().toStdString(),
                panelName.toStdString(),
                control->name().toStdString()
                );
            return;
        }
    }
}
