#include "tracknavigationmodel.h"

using namespace au::trackedit;

TrackNavigationModel::TrackNavigationModel(QObject* parent)
    : QObject(parent)
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
        for (int i = 0; i < m_trackItemPanels.size(); ++i) {
            if (m_trackItemPanels.at(i)->name() == QString("Track %1 Panel").arg(track.id)) {
                muse::ui::NavigationPanel* trackPanel = m_trackItemPanels.takeAt(i);
                trackPanel->setSection(nullptr);
                trackPanel->deleteLater();
                break;
            }
        }

        for (int i = 0; i < m_clipItemPanels.size(); ++i) {
            if (m_clipItemPanels.at(i)->name() == QString("Clip %1 Panel").arg(track.id)) {
                muse::ui::NavigationPanel* clipPanel = m_clipItemPanels.takeAt(i);
                clipPanel->setSection(nullptr);
                clipPanel->deleteLater();
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
            if (m_trackItemPanels.at(i)->name() == QString("Track %1 Panel").arg(track.id)) {
                auto trackPanel = m_trackItemPanels.takeAt(i);
                m_trackItemPanels.insert(pos, trackPanel);
                break;
            }
        }

        for (int i = 0; i < m_clipItemPanels.size(); ++i) {
            if (m_clipItemPanels.at(i)->name() == QString("Clip %1 Panel").arg(track.id)) {
                auto clipPanel = m_clipItemPanels.takeAt(i);
                m_clipItemPanels.insert(pos, clipPanel);
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
}

void TrackNavigationModel::addPanels(trackedit::TrackId trackId, int pos)
{
    muse::ui::NavigationPanel* trackPanel = new muse::ui::NavigationPanel(this);
    trackPanel->setName(QString("Track %1 Panel").arg(trackId));
    trackPanel->setIndex({ 2 * pos, 0 });
    trackPanel->setOrder(2 * pos);
    trackPanel->setSection(m_section);
    trackPanel->componentComplete();

    connect(trackPanel, &muse::ui::NavigationPanel::navigationEvent, this,
            [this, name = trackPanel->name()](muse::ui::NavigationEvent* event) {
        int pos = -1;
        for (int i = 0; i < m_trackItemPanels.size(); ++i) {
            if (m_trackItemPanels.at(i)->name() == name) {
                pos = i;
                break;
            }
        }

        if (pos == -1) {
            return;
        }

        if (event->type() == muse::ui::NavigationEvent::Type::Up) {
            const auto args = muse::actions::ActionData::make_arg1<int>(pos);
            dispatcher()->dispatch("prev-track", args);
            event->setAccepted(true);
        } else if (event->type() == muse::ui::NavigationEvent::Type::Down) {
            const auto args = muse::actions::ActionData::make_arg1<int>(pos);
            dispatcher()->dispatch("next-track", args);
            event->setAccepted(true);
        } else if (event->type() == muse::ui::NavigationEvent::Type::Trigger) {
            dispatcher()->dispatch("track-toggle-focused-selection");
            event->setAccepted(true);
        }
    });

    muse::ui::NavigationPanel* clipPanel = new muse::ui::NavigationPanel(this);
    clipPanel->setName(QString("Clip %1 Panel").arg(trackId));
    clipPanel->setIndex({ 2 * pos + 1, 0 });
    clipPanel->setOrder(2 * pos + 1);
    clipPanel->setSection(m_section);
    clipPanel->componentComplete();

    m_trackItemPanels.append(trackPanel);
    m_clipItemPanels.append(clipPanel);

    emit trackItemPanelsChanged();
    emit clipItemPanelsChanged();
}

void TrackNavigationModel::resetPanelOrder()
{
    for (int i = 0; i < m_trackItemPanels.size(); ++i) {
        m_trackItemPanels.at(i)->setOrder(2 * i);
    }

    for (int i = 0; i < m_clipItemPanels.size(); ++i) {
        m_clipItemPanels.at(i)->setOrder(2 * i + 1);
    }

    emit trackItemPanelsChanged();
    emit clipItemPanelsChanged();
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

void TrackNavigationModel::moveFocusTo(int index)
{
    if (index < 0 || index >= m_trackItemPanels.size()) {
        return;
    }

    dispatcher()->dispatch("focus-track-index", muse::actions::ActionData::make_arg1<int>(index));
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

        connect(m_default_control, &muse::ui::NavigationControl::navigationEvent, this, [this](muse::ui::NavigationEvent* event) {
            if (event->type() == muse::ui::NavigationEvent::Type::Up) {
                dispatcher()->dispatch("focus-prev-track");
                event->setAccepted(true);
            } else if (event->type() == muse::ui::NavigationEvent::Type::Down) {
                dispatcher()->dispatch("focus-next-track");
                event->setAccepted(true);
            } else if (event->type() == muse::ui::NavigationEvent::Type::Trigger) {
                dispatcher()->dispatch("track-toggle-focused-selection");
                event->setAccepted(true);
            }
        });
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

    for (auto& panel : m_clipItemPanels) {
        panel->setSection(nullptr);
        panel->deleteLater();
    }
    m_clipItemPanels.clear();
}

QList<muse::ui::NavigationPanel*> TrackNavigationModel::trackItemPanels() const
{
    return m_trackItemPanels;
}

QList<muse::ui::NavigationPanel*> TrackNavigationModel::clipItemPanels() const
{
    return m_clipItemPanels;
}
