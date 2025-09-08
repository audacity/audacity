#include "tracknavigationmodel.h"

using namespace au::trackedit;
TrackNavigationModel::TrackNavigationModel(QObject* parent)
    : QObject(parent)
{
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        init();
    });
}

void TrackNavigationModel::init(muse::ui::NavigationSection* section)
{
    if (section) {
        m_section = section;
    }

    const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    prj->tracksChanged().onReceive(this, [this](std::vector<au::trackedit::Track>) {
    });

    prj->trackAdded().onReceive(this, [this](const Track& track) {
        const int pos = m_trackItemPanels.size();

        muse::ui::NavigationPanel* trackPanel = new muse::ui::NavigationPanel(this);
        trackPanel->setName(QString("Track %1 Panel").arg(track.id));
        trackPanel->setIndex({ 2 * pos, 0 });
        trackPanel->setOrder(2 * pos);
        trackPanel->setSection(m_section);

        connect(trackPanel, &muse::ui::NavigationPanel::navigationEvent, this, [this, pos](QVariant event) {
            muse::ui::NavigationEvent navEvent = event.value<muse::ui::NavigationEvent>();

            if (navEvent.type() == muse::ui::NavigationEvent::Type::Up) {
                const auto args = muse::actions::ActionData::make_arg1<int>(pos);
                dispatcher()->dispatch("prev-track", args);
                navEvent.setAccepted(true);
            } else if (navEvent.type() == muse::ui::NavigationEvent::Type::Down) {
                const auto args = muse::actions::ActionData::make_arg1<int>(pos);
                dispatcher()->dispatch("next-track", args);
                navEvent.setAccepted(true);
            } else if (navEvent.type() == muse::ui::NavigationEvent::Type::Trigger) {
                dispatcher()->dispatch("track-toggle-focused-selection");
                navEvent.setAccepted(true);
            }
        });

        muse::ui::NavigationPanel* clipPanel = new muse::ui::NavigationPanel(this);
        clipPanel->setName(QString("Clip %1 Panel").arg(track.id));
        clipPanel->setIndex({ 2 * pos + 1, 0 });
        clipPanel->setOrder(2 * pos + 1);
        clipPanel->setSection(m_section);

        m_trackItemPanels.append(trackPanel);
        m_clipItemPanels.append(clipPanel);

        emit trackItemPanelsChanged();
        emit clipItemPanelsChanged();
    });

    prj->trackRemoved().onReceive(this, [this](const Track& track) {
        for (int i = 0; i < m_trackItemPanels.size(); ++i) {
            if (m_trackItemPanels.at(i)->name() == QString("Track %1 Panel").arg(track.id)) {
                muse::ui::NavigationPanel* trackPanel = m_trackItemPanels.takeAt(i);
                trackPanel->deleteLater();
                break;
            }
        }

        for (int i = 0; i < m_clipItemPanels.size(); ++i) {
            if (m_clipItemPanels.at(i)->name() == QString("Clip %1 Panel").arg(track.id)) {
                muse::ui::NavigationPanel* clipPanel = m_clipItemPanels.takeAt(i);
                clipPanel->deleteLater();
                break;
            }
        }

        emit trackItemPanelsChanged();
        emit clipItemPanelsChanged();
    });

    prj->trackChanged().onReceive(this, [this](const Track&) {});

    prj->trackInserted().onReceive(this, [this](const Track& track, int pos) {
        muse::ui::NavigationPanel* trackPanel = new muse::ui::NavigationPanel(this);
        trackPanel->setName(QString("Track %1 Panel").arg(track.id));
        trackPanel->setIndex({ 2 * pos, 0 });
        trackPanel->setOrder(pos);
        trackPanel->setSection(m_section);

        muse::ui::NavigationPanel* clipPanel = new muse::ui::NavigationPanel(this);
        clipPanel->setName(QString("Clip %1 Panel").arg(track.id));
        clipPanel->setIndex({ 2 * pos + 1, 1 });
        clipPanel->setOrder(pos);
        clipPanel->setSection(m_section);

        m_trackItemPanels.append(trackPanel);
        m_clipItemPanels.append(clipPanel);

        emit trackItemPanelsChanged();
        emit clipItemPanelsChanged();
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

        for (int i = 0; i < m_trackItemPanels.size(); ++i) {
            m_trackItemPanels.at(i)->setOrder(i);
        }

        for (int i = 0; i < m_clipItemPanels.size(); ++i) {
            m_clipItemPanels.at(i)->setOrder(i);
        }

        emit trackItemPanelsChanged();
        emit clipItemPanelsChanged();
    });

    const auto trackList = prj->trackList();
    for (const auto& track : trackList) {
        const int pos = m_trackItemPanels.size();
        muse::ui::NavigationPanel* trackPanel = new muse::ui::NavigationPanel(this);
        trackPanel->setName(QString("Track %1 Panel").arg(track.id));
        trackPanel->setIndex({ 2 * pos, 0 });
        trackPanel->setOrder(2 * pos);
        trackPanel->setSection(m_section);

        connect(trackPanel, &muse::ui::NavigationPanel::navigationEvent, this, [this, pos](QVariant event) {
            muse::ui::NavigationEvent navEvent = event.value<muse::ui::NavigationEvent>();

            if (navEvent.type() == muse::ui::NavigationEvent::Type::Up) {
                const auto args = muse::actions::ActionData::make_arg1<int>(pos);
                dispatcher()->dispatch("prev-track", args);
                navEvent.setAccepted(true);
            } else if (navEvent.type() == muse::ui::NavigationEvent::Type::Down) {
                const auto args = muse::actions::ActionData::make_arg1<int>(pos);
                dispatcher()->dispatch("next-track", args);
                navEvent.setAccepted(true);
            } else if (navEvent.type() == muse::ui::NavigationEvent::Type::Trigger) {
                dispatcher()->dispatch("track-toggle-focused-selection");
                navEvent.setAccepted(true);
            }
        });

        m_trackItemPanels.append(trackPanel);
    }

    for (const auto& track : trackList) {
        const int pos = m_clipItemPanels.size();
        muse::ui::NavigationPanel* clipPanel = new muse::ui::NavigationPanel(this);
        clipPanel->setName(QString("Clip %1 Panel").arg(track.id));
        clipPanel->setIndex({ 2 * pos + 1, 0 });
        clipPanel->setOrder(2 * pos + 1);
        clipPanel->setSection(m_section);

        m_clipItemPanels.append(clipPanel);
    }
    emit trackItemPanelsChanged();
    emit clipItemPanelsChanged();

    m_default_section = new muse::ui::NavigationSection(this);
    m_default_section->setName("Main Section");
    m_default_section->setIndex({ 0, 0 });
    m_default_section->setOrder(0);

    m_default_panel = new muse::ui::NavigationPanel(this);
    m_default_panel->setName("Main Panel");
    m_default_panel->setIndex({ 0, 0 });
    m_default_panel->setOrder(0);
    m_default_panel->setSection(m_default_section);

    m_default_control = new muse::ui::NavigationControl(this);
    m_default_control->setName("Main Control");
    m_default_control->setIndex({ 0, 0 });
    m_default_control->setOrder(0);
    m_default_control->setPanel(m_default_panel);

    connect(m_default_control, &muse::ui::NavigationControl::navigationEvent, this, [this](QVariant event) {
        muse::ui::NavigationEvent navEvent = event.value<muse::ui::NavigationEvent>();

        if (navEvent.type() == muse::ui::NavigationEvent::Type::Up) {
            dispatcher()->dispatch("focus-prev-track");
            navEvent.setAccepted(true);
        } else if (navEvent.type() == muse::ui::NavigationEvent::Type::Down) {
            dispatcher()->dispatch("focus-next-track");
            navEvent.setAccepted(true);
        } else if (navEvent.type() == muse::ui::NavigationEvent::Type::Trigger) {
            dispatcher()->dispatch("track-toggle-focused-selection");
            navEvent.setAccepted(true);
        }
    });

    navigationController()->reg(m_default_section);
    navigationController()->requestActivateByName(m_default_section->name().toStdString(),
                                                  m_default_panel->name().toStdString(), m_default_control->name().toStdString());
    navigationController()->setDefaultNavigationControl(m_default_control);
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

QList<muse::ui::NavigationPanel*> TrackNavigationModel::trackItemPanels() const
{
    return m_trackItemPanels;
}

QList<muse::ui::NavigationPanel*> TrackNavigationModel::clipItemPanels() const
{
    return m_clipItemPanels;
}
