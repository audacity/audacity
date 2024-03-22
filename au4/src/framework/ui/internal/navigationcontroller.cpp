/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "navigationcontroller.h"

#include <algorithm>
#include <limits>
#include <utility>

#include <QApplication>
#include <QWindow>
#include <QTextStream>

#include "diagnostics/diagnosticutils.h"
#include "async/async.h"
#include "defer.h"
#include "log.h"

// #define NAVIGATION_LOGGING_ENABLED

#ifdef NAVIGATION_LOGGING_ENABLED
#define MYLOG() LOGI()
#else
#define MYLOG() LOGN()
#endif

using namespace mu::ui;

static const mu::UriQuery DEV_SHOW_CONTROLS_URI("musescore://devtools/keynav/controls?sync=false&modal=false");

using MoveDirection = NavigationController::MoveDirection;
using Event = INavigation::Event;
using ActivationType = INavigation::ActivationType;

// algorithms
template<class T>
static T* findNearestEnabled(const std::set<T*>& set, const INavigation::Index& currentIndex, MoveDirection direction)
{
    TRACEFUNC;
    T* ret = nullptr;
    for (T* v : set) {
        if (!v->enabled()) {
            continue;
        }

        switch (direction) {
        case MoveDirection::First: {
            if (!ret) {
                ret = v;
                continue;
            }

            if (v->index().row <= ret->index().row) {
                if (v->index().column <= ret->index().column) {
                    ret = v;
                    continue;
                }
            }
        } break;
        case MoveDirection::Last: {
            if (!ret) {
                ret = v;
                continue;
            }

            if (v->index().row >= ret->index().row) {
                if (v->index().column >= ret->index().column) {
                    ret = v;
                    continue;
                }
            }
        } break;
        case MoveDirection::Right: {
            if (v->index().row != currentIndex.row) {
                continue;
            }

            if (v->index().column > currentIndex.column) {
                if (!ret) {
                    ret = v;
                    continue;
                }

                if (v->index().column < ret->index().column) {
                    ret = v;
                }
            }
        } break;
        case MoveDirection::Left: {
            if (v->index().row != currentIndex.row) {
                continue;
            }

            if (v->index().column < currentIndex.column) {
                if (!ret) {
                    ret = v;
                    continue;
                }

                if (v->index().column > ret->index().column) {
                    ret = v;
                }
            }
        } break;
        case MoveDirection::Down: {
            if (v->index().column != currentIndex.column) {
                continue;
            }

            if (v->index().row > currentIndex.row) {
                if (!ret) {
                    ret = v;
                    continue;
                }

                if (v->index().row < ret->index().row) {
                    ret = v;
                }
            }
        } break;
        case MoveDirection::Up: {
            if (v->index().column != currentIndex.column) {
                continue;
            }

            if (v->index().row < currentIndex.row) {
                if (!ret) {
                    ret = v;
                    continue;
                }

                if (v->index().row > ret->index().row) {
                    ret = v;
                }
            }
        } break;
        }
    }

    return ret;
}

template<class T>
static T* firstEnabled(const std::set<T*>& set, const INavigation::Index& currentIndex, MoveDirection direction)
{
    if (set.empty()) {
        return nullptr;
    }

    IF_ASSERT_FAILED(direction == MoveDirection::Right || direction == MoveDirection::Down) {
        return nullptr;
    }

    return findNearestEnabled<T>(set, currentIndex, direction);
}

template<class T>
static T* firstEnabled(const std::set<T*>& set)
{
    if (set.empty()) {
        return nullptr;
    }

    return findNearestEnabled<T>(set, INavigation::Index(), MoveDirection::First);
}

template<class T>
static T* lastEnabled(const std::set<T*>& set, const INavigation::Index& currentIndex, MoveDirection direction)
{
    if (set.empty()) {
        return nullptr;
    }

    IF_ASSERT_FAILED(direction == MoveDirection::Left || (direction == MoveDirection::Up)) {
        return nullptr;
    }

    return findNearestEnabled<T>(set, currentIndex, direction);
}

template<class T>
static T* lastEnabled(const std::set<T*>& set)
{
    if (set.empty()) {
        return nullptr;
    }

    return findNearestEnabled<T>(set, INavigation::Index(), MoveDirection::Last);
}

template<class T>
static T* nextEnabled(const std::set<T*>& set, const INavigation::Index& currentIndex,
                      MoveDirection direction = MoveDirection::Right)
{
    if (set.empty()) {
        return nullptr;
    }

    return findNearestEnabled<T>(set, currentIndex, direction);
}

template<class T>
static T* prevEnabled(const std::set<T*>& set, const INavigation::Index& currentIndex, MoveDirection direction = MoveDirection::Left)
{
    if (set.empty()) {
        return nullptr;
    }

    return findNearestEnabled<T>(set, currentIndex, direction);
}

template<class T>
static T* findActive(const std::set<T*>& set)
{
    auto it = std::find_if(set.cbegin(), set.cend(), [](const T* s) {
        return s->active();
    });

    if (it != set.cend()) {
        return *it;
    }

    return nullptr;
}

template<class T>
static T* findByName(const std::set<T*>& set, const QString& name)
{
    auto it = std::find_if(set.cbegin(), set.cend(), [name](const T* s) {
        return s->name() == name;
    });

    if (it != set.cend()) {
        return *it;
    }

    return nullptr;
}

template<class T>
static T* findByIndex(const std::set<T*>& set, const INavigation::Index& idx)
{
    auto it = std::find_if(set.cbegin(), set.cend(), [idx](const T* s) {
        return s->index() == idx;
    });

    if (it != set.cend()) {
        return *it;
    }

    return nullptr;
}

void NavigationController::init()
{
    dispatcher()->reg(this, "nav-next-section", [this]() { navigateTo(NavigationType::NextSection); });
    dispatcher()->reg(this, "nav-prev-section", [this]() { navigateTo(NavigationType::PrevSection); });
    dispatcher()->reg(this, "nav-next-panel", [this]() { navigateTo(NavigationType::NextPanel); });
    dispatcher()->reg(this, "nav-prev-panel", [this]() { navigateTo(NavigationType::PrevPanel); });
    //! NOTE Same as panel at the moment
    dispatcher()->reg(this, "nav-next-tab", [this]() { navigateTo(NavigationType::NextPanel); });
    dispatcher()->reg(this, "nav-prev-tab", [this]() { navigateTo(NavigationType::PrevPanel); });

    dispatcher()->reg(this, "nav-trigger-control", [this]() { doTriggerControl(); });

    dispatcher()->reg(this, "nav-right", [this]() { navigateTo(NavigationType::Right); });
    dispatcher()->reg(this, "nav-left", [this]() { navigateTo(NavigationType::Left); });
    dispatcher()->reg(this, "nav-up", [this]() { navigateTo(NavigationType::Up); });
    dispatcher()->reg(this, "nav-down", [this]() { navigateTo(NavigationType::Down); });
    dispatcher()->reg(this, "nav-escape", [this]() { onEscape(); });

    dispatcher()->reg(this, "nav-first-control", [this]() { navigateTo(NavigationType::FirstControl); });         // typically Home key
    dispatcher()->reg(this, "nav-last-control", [this]() { navigateTo(NavigationType::LastControl); });           // typically End key
    dispatcher()->reg(this, "nav-nextrow-control", [this]() { navigateTo(NavigationType::NextRowControl); });     // typically PageDown key
    dispatcher()->reg(this, "nav-prevrow-control", [this]() { navigateTo(NavigationType::PrevRowControl); });     // typically PageUp key

    qApp->installEventFilter(this);
}

void NavigationController::reg(INavigationSection* section)
{
    //! TODO add check on valid state
    TRACEFUNC;
    m_sections.insert(section);
    section->setOnActiveRequested([this](INavigationSection* section, INavigationPanel* panel, INavigationControl* control,
                                         bool enableHighlight, ActivationType activationType) {
        if (control && activationType == ActivationType::ByMouse) {
            if (section->type() != INavigationSection::Type::Exclusive) {
                return;
            }
        }

        if (enableHighlight) {
            setIsHighlight(true);
        }

        onActiveRequested(section, panel, control);
    });
}

void NavigationController::unreg(INavigationSection* section)
{
    TRACEFUNC;
    m_sections.erase(section);
    section->setOnActiveRequested(nullptr);
}

const std::set<INavigationSection*>& NavigationController::sections() const
{
    return m_sections;
}

bool NavigationController::isHighlight() const
{
    return m_isHighlight;
}

void NavigationController::setIsHighlight(bool isHighlight)
{
    if (m_isHighlight == isHighlight) {
        return;
    }

    m_isHighlight = isHighlight;
    m_highlightChanged.notify();
}

mu::async::Notification NavigationController::highlightChanged() const
{
    return m_highlightChanged;
}

void NavigationController::setIsResetOnMousePress(bool arg)
{
    m_isResetOnMousePress = arg;
}

void NavigationController::resetIfNeed(QObject* watched)
{
    if (!m_isResetOnMousePress) {
        return;
    }

#ifdef MUE_BUILD_DIAGNOSTICS_MODULE
    if (diagnostics::isDiagnosticHierarchy(watched)) {
        return;
    }
#endif

    auto activeCtrl = activeControl();
    if (activeCtrl && activeCtrl != m_defaultNavigationControl && watched == qApp) {
        resetNavigation();
    }

    setIsHighlight(false);
}

bool NavigationController::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() == QEvent::MouseButtonPress) {
        resetIfNeed(watched);
    }

    return QObject::eventFilter(watched, event);
}

void NavigationController::navigateTo(NavigationController::NavigationType type)
{
    switch (type) {
    case NavigationType::NextSection:
        goToNextSection();
        break;
    case NavigationType::PrevSection:
        goToPrevSection(false);
        break;
    case NavigationType::PrevSectionActiveLastPanel:
        goToPrevSection(true);
        break;
    case NavigationType::NextPanel:
        goToNextPanel();
        break;
    case NavigationType::PrevPanel:
        goToPrevPanel();
        break;
    case NavigationType::Left:
        onLeft();
        break;
    case NavigationType::Right:
        onRight();
        break;
    case NavigationType::Up:
        onUp();
        break;
    case NavigationType::Down:
        onDown();
        break;
    case NavigationType::FirstControl:
        goToFirstControl();
        break;
    case NavigationType::LastControl:
        goToLastControl();
        break;
    case NavigationType::NextRowControl:
        goToNextRowControl();
        break;
    case NavigationType::PrevRowControl:
        goToPrevRowControl();
        break;
    }

    setIsHighlight(true);
}

void NavigationController::resetNavigation()
{
    MYLOG() << "===";
    INavigationSection* activeSec = this->activeSection();
    if (!activeSec) {
        return;
    }

    INavigationPanel* activePnl = findActive(activeSec->panels());
    if (activePnl) {
        INavigationControl* activeCtrl = findActive(activePnl->controls());
        if (activeCtrl) {
            activeCtrl->setActive(false);
        }

        activePnl->setActive(false);
    }

    activeSec->setActive(false);

    if (m_defaultNavigationControl) {
        INavigationPanel* panel = m_defaultNavigationControl->panel();
        INavigationSection* section = panel ? panel->section() : nullptr;
        if (section->enabled()) {
            m_defaultNavigationControl->setActive(true);
            m_navigationChanged.notify();
        }
    } else {
        doActivateFirst();
    }
}

void NavigationController::doActivateSection(INavigationSection* sect, bool isActivateLastPanel)
{
    TRACEFUNC;
    IF_ASSERT_FAILED(sect) {
        return;
    }

    for (INavigationPanel* panel : sect->panels()) {
        doDeactivatePanel(panel);
    }

    sect->setActive(true);
    MYLOG() << "activated section: " << sect->name() << ", order: " << sect->index().order();

    INavigationPanel* toActivatePanel = nullptr;
    if (isActivateLastPanel) {
        toActivatePanel = lastEnabled(sect->panels());
    } else {
        toActivatePanel = firstEnabled(sect->panels());
    }

    if (toActivatePanel) {
        doActivatePanel(toActivatePanel);
    }
}

void NavigationController::doDeactivateSection(INavigationSection* sect)
{
    TRACEFUNC;
    IF_ASSERT_FAILED(sect) {
        return;
    }

    for (INavigationPanel* panel : sect->panels()) {
        doDeactivatePanel(panel);
    }

    sect->setActive(false);
}

void NavigationController::doActivatePanel(INavigationPanel* panel)
{
    TRACEFUNC;
    IF_ASSERT_FAILED(panel) {
        return;
    }

    for (INavigationControl* ctrl : panel->controls()) {
        doDeactivateControl(ctrl);
    }

    INavigation::EventPtr event = INavigation::Event::make(INavigation::Event::AboutActive);
    panel->onEvent(event);

    panel->setActive(true);
    MYLOG() << "activated panel: " << panel->name() << ", order: " << panel->index().order();

    INavigationControl* control = nullptr;
    QString ctrlName = event->data.value("controlName").toString();
    if (!ctrlName.isEmpty()) {
        control = findByName(panel->controls(), ctrlName);
        IF_ASSERT_FAILED(control) {
        }
    }

    if (!control) {
        QVariantList idxVal = event->data.value("controlIndex").toList();
        if (idxVal.count() == 2) {
            INavigation::Index ctrlIndex;
            ctrlIndex.row = idxVal.at(0).toInt();
            ctrlIndex.column = idxVal.at(1).toInt();
            control = findByIndex(panel->controls(), ctrlIndex);
            if (!control) {
                bool isOptional = event->data.value("controlOptional").toBool();
                if (!isOptional) {
                    IF_ASSERT_FAILED(control) {
                    }
                }
            }
        }
    }

    if (!control) {
        control = firstEnabled(panel->controls());
    }

    if (control) {
        doActivateControl(control);
    }
}

void NavigationController::doDeactivatePanel(INavigationPanel* panel)
{
    TRACEFUNC;
    IF_ASSERT_FAILED(panel) {
        return;
    }

    for (INavigationControl* ctr : panel->controls()) {
        doDeactivateControl(ctr);
    }

    if (panel->active()) {
        panel->setActive(false);
    }
}

void NavigationController::doActivateControl(INavigationControl* ctrl)
{
    TRACEFUNC;
    IF_ASSERT_FAILED(ctrl) {
        return;
    }

    ctrl->setActive(true);
    MYLOG() << "activated control: " << ctrl->name() << ", row: " << ctrl->index().row << ", column: " << ctrl->index().column;
}

void NavigationController::doDeactivateControl(INavigationControl* ctrl)
{
    TRACEFUNC;
    IF_ASSERT_FAILED(ctrl) {
        return;
    }

    if (ctrl->active()) {
        ctrl->setActive(false);
    }
}

void NavigationController::doActivateFirst()
{
    if (m_sections.empty()) {
        return;
    }

    INavigationSection* first = firstEnabled(m_sections);
    if (first) {
        doActivateSection(first);
    }
}

void NavigationController::doActivateLast()
{
    if (m_sections.empty()) {
        return;
    }

    INavigationSection* last = lastEnabled(m_sections);
    if (last) {
        doActivateSection(last);
    }
}

INavigationSection* NavigationController::activeSection() const
{
    TRACEFUNC;
    if (m_sections.empty()) {
        return nullptr;
    }
    return findActive(m_sections);
}

INavigationPanel* NavigationController::activePanel() const
{
    TRACEFUNC;
    INavigationSection* activeSec = activeSection();
    if (!activeSec) {
        return nullptr;
    }
    return findActive(activeSec->panels());
}

INavigationControl* NavigationController::activeControl() const
{
    TRACEFUNC;
    INavigationPanel* activePanel = this->activePanel();
    if (!activePanel) {
        return nullptr;
    }
    return findActive(activePanel->controls());
}

void NavigationController::setDefaultNavigationControl(INavigationControl* control)
{
    m_defaultNavigationControl = control;
}

mu::async::Notification NavigationController::navigationChanged() const
{
    return m_navigationChanged;
}

void NavigationController::goToNextSection()
{
    TRACEFUNC;
    MYLOG() << "====";
    if (m_sections.empty()) {
        return;
    }

    INavigationSection* activeSec = findActive(m_sections);
    if (!activeSec) { // no any active
        doActivateFirst();
        return;
    }

    if (activeSec->type() == INavigationSection::Type::Exclusive) {
        INavigationPanel* first = firstEnabled(activeSec->panels());
        if (first) {
            doActivatePanel(first);
            m_navigationChanged.notify();
        }
        return;
    }

    doDeactivateSection(activeSec);

    INavigationSection* nextSec = nextEnabled(m_sections, activeSec->index());
    if (!nextSec) { // active is last
        nextSec = firstEnabled(m_sections); // the first to be the next
    }
    if (!nextSec) {
        LOGI() << "no enabled sections!";
        return;
    }

    LOGI() << "nextSec: " << nextSec->name() << ", enabled: " << nextSec->enabled();

    doActivateSection(nextSec);

    m_navigationChanged.notify();
}

void NavigationController::goToPrevSection(bool isActivateLastPanel)
{
    TRACEFUNC;
    MYLOG() << "====";
    if (m_sections.empty()) {
        return;
    }

    INavigationSection* activeSec = findActive(m_sections);
    if (!activeSec) { // no any active
        doActivateLast();
        return;
    }

    if (activeSec->type() == INavigationSection::Type::Exclusive) {
        INavigationPanel* first = firstEnabled(activeSec->panels());
        if (first) {
            doActivatePanel(first);
            m_navigationChanged.notify();
        }
        return;
    }

    doDeactivateSection(activeSec);

    INavigationSection* prevSec = prevEnabled(m_sections, activeSec->index());
    if (!prevSec) { // active is first
        prevSec = lastEnabled(m_sections); // the last to be the prev
    }

    doActivateSection(prevSec, isActivateLastPanel);

    m_navigationChanged.notify();
}

void NavigationController::goToNextPanel()
{
    TRACEFUNC;
    MYLOG() << "====";
    INavigationSection* activeSec = activeSection();
    if (!activeSec) {
        doActivateFirst();
        m_navigationChanged.notify();
        return;
    }

    INavigationPanel* activePanel = findActive(activeSec->panels());
    if (!activePanel) { // no any active
        INavigationPanel* first = firstEnabled(activeSec->panels());
        if (first) {
            doActivatePanel(first);
            m_navigationChanged.notify();
        }
        return;
    }

    doDeactivatePanel(activePanel);

    INavigationPanel* nextPanel = nextEnabled(activeSec->panels(), activePanel->index());
    if (nextPanel) {
        doActivatePanel(nextPanel);
        m_navigationChanged.notify();
        return;
    }

    if (activeSec->type() == INavigationSection::Type::Exclusive) {
        INavigationPanel* first = firstEnabled(activeSec->panels());
        if (first) {
            doActivatePanel(first);
            m_navigationChanged.notify();
        }
        return;
    }

    // active is last panel, go to first panel in next section
    goToNextSection();
}

void NavigationController::goToPrevPanel()
{
    TRACEFUNC;
    MYLOG() << "====";
    INavigationSection* activeSec = activeSection();
    if (!activeSec) {
        doActivateLast();
        m_navigationChanged.notify();
        return;
    }

    INavigationPanel* activePanel = findActive(activeSec->panels());
    if (!activePanel) { // no any active
        INavigationPanel* lastPanel = lastEnabled(activeSec->panels());
        if (lastPanel) {
            doActivatePanel(lastPanel);
            m_navigationChanged.notify();
        }
        return;
    }

    doDeactivatePanel(activePanel);

    INavigationPanel* prevPanel = prevEnabled(activeSec->panels(), activePanel->index());
    if (prevPanel) {
        doActivatePanel(prevPanel);
        m_navigationChanged.notify();
        return;
    }

    if (activeSec->type() == INavigationSection::Type::Exclusive) {
        INavigationPanel* lastPanel = lastEnabled(activeSec->panels());
        if (lastPanel) {
            doActivatePanel(lastPanel);
            m_navigationChanged.notify();
        }
        return;
    }

    // active is first, go to last panel in prev section
    goToPrevSection(true);
}

void NavigationController::onRight()
{
    TRACEFUNC;
    INavigationPanel* activePanel = this->activePanel();
    if (!activePanel) {
        return;
    }

    INavigation::EventPtr e = Event::make(Event::Right);
    activePanel->onEvent(e);
    if (e->accepted) {
        return;
    }

    INavigationControl* activeCtrl = findActive(activePanel->controls());
    if (activeCtrl) {
        activeCtrl->onEvent(e);
        if (e->accepted) {
            return;
        }
    }

    goToControl(MoveDirection::Right, activePanel);
}

void NavigationController::onLeft()
{
    TRACEFUNC;
    INavigationPanel* activePanel = this->activePanel();
    if (!activePanel) {
        return;
    }

    INavigation::EventPtr e = Event::make(Event::Left);
    activePanel->onEvent(e);
    if (e->accepted) {
        return;
    }

    INavigationControl* activeCtrl = findActive(activePanel->controls());
    if (activeCtrl) {
        activeCtrl->onEvent(e);
        if (e->accepted) {
            return;
        }
    }

    goToControl(MoveDirection::Left, activePanel);
}

void NavigationController::onDown()
{
    TRACEFUNC;
    INavigationPanel* activePanel = this->activePanel();
    if (!activePanel) {
        return;
    }

    INavigation::EventPtr e = Event::make(Event::Down);
    activePanel->onEvent(e);
    if (e->accepted) {
        return;
    }

    INavigationControl* activeCtrl = findActive(activePanel->controls());
    if (activeCtrl) {
        activeCtrl->onEvent(e);
        if (e->accepted) {
            return;
        }
    }

    goToControl(MoveDirection::Down, activePanel);
}

void NavigationController::onUp()
{
    TRACEFUNC;
    INavigationPanel* activePanel = this->activePanel();
    if (!activePanel) {
        return;
    }

    INavigation::EventPtr e = Event::make(Event::Up);
    activePanel->onEvent(e);
    if (e->accepted) {
        return;
    }

    INavigationControl* activeCtrl = findActive(activePanel->controls());
    if (activeCtrl) {
        activeCtrl->onEvent(e);
        if (e->accepted) {
            return;
        }
    }

    goToControl(MoveDirection::Up, activePanel);
}

void NavigationController::onEscape()
{
    TRACEFUNC;
    MYLOG() << "====";
    INavigationSection* activeSec = activeSection();
    if (!activeSec) {
        return;
    }

    INavigation::EventPtr e = Event::make(Event::Escape);
    activeSec->onEvent(e);
    if (e->accepted) {
        return;
    }

    INavigationPanel* activePanel = findActive(activeSec->panels());
    if (!activePanel) {
        return;
    }

    activePanel->onEvent(e);
    if (e->accepted) {
        return;
    }

    INavigationControl* activeCtrl = findActive(activePanel->controls());
    if (!activeCtrl) {
        return;
    }

    activeCtrl->onEvent(e);
    if (e->accepted) {
        return;
    }

    activeCtrl->setActive(false);

    if (m_defaultNavigationControl) {
        doActivateControl(m_defaultNavigationControl);
    }
}

void NavigationController::goToFirstControl()
{
    MYLOG() << "====";
    goToControl(MoveDirection::First);
}

void NavigationController::goToLastControl()
{
    MYLOG() << "====";
    goToControl(MoveDirection::Last);
}

void NavigationController::goToNextRowControl()
{
    TRACEFUNC;
    MYLOG() << "====";
    INavigationPanel* activePanel = this->activePanel();
    if (!activePanel) {
        return;
    }

    INavigationControl* activeControl = findActive(activePanel->controls());
    if (activeControl) {
        doDeactivateControl(activeControl);
    }

    INavigationControl* toControl = nullptr;
    if (activeControl) {
        INavigation::Index index = activeControl->index();
        index.column = 0;
        toControl = nextEnabled(activePanel->controls(), index, MoveDirection::Down);
        if (!toControl) { // last row
            toControl = firstEnabled(activePanel->controls());
        }
    } else {
        toControl = firstEnabled(activePanel->controls());
    }

    if (toControl) {
        doActivateControl(toControl);
    }

    m_navigationChanged.notify();
}

void NavigationController::goToPrevRowControl()
{
    TRACEFUNC;
    MYLOG() << "====";
    INavigationPanel* activePanel = this->activePanel();
    if (!activePanel) {
        return;
    }

    INavigationControl* activeControl = findActive(activePanel->controls());
    if (activeControl) {
        doDeactivateControl(activeControl);
    }

    INavigationControl* toControl = nullptr;
    if (activeControl) {
        INavigation::Index index = activeControl->index();
        index.column = 0;
        toControl = prevEnabled(activePanel->controls(), index, MoveDirection::Up);
        if (!toControl) { // first row
            toControl = lastEnabled(activePanel->controls());
        }
    } else {
        toControl = lastEnabled(activePanel->controls());
    }

    if (toControl) {
        doActivateControl(toControl);
    }

    m_navigationChanged.notify();
}

void NavigationController::goToControl(MoveDirection direction, INavigationPanel* activePanel)
{
    TRACEFUNC;
    MYLOG() << "direction: " << direction;
    if (!activePanel) {
        activePanel = this->activePanel();
    }

    if (!activePanel) {
        return;
    }

    INavigationControl* activeControl = findActive(activePanel->controls());
    INavigationControl* toControl = nullptr;

    bool tryOtherOrientation = activePanel->direction() == INavigationPanel::Direction::Horizontal
                               || activePanel->direction() == INavigationPanel::Direction::Vertical;

    switch (direction) {
    case MoveDirection::First: {
        toControl = firstEnabled(activePanel->controls());
    } break;
    case MoveDirection::Last: {
        toControl = lastEnabled(activePanel->controls());
    } break;
    case MoveDirection::Right: {
        if (!activeControl) { // no any active
            toControl = firstEnabled(activePanel->controls(), INavigation::Index(), MoveDirection::Right);
        } else {
            toControl = nextEnabled(activePanel->controls(), activeControl->index(), MoveDirection::Right);
            if (!toControl && tryOtherOrientation) {
                toControl = nextEnabled(activePanel->controls(), activeControl->index(), MoveDirection::Down);
            }
            if (!toControl) { // active is last
                INavigation::Index index = activeControl->index();
                index.column = -1;
                toControl = firstEnabled(activePanel->controls(), index, MoveDirection::Right); // the first to be the next

                if (!toControl && tryOtherOrientation) {
                    index = activeControl->index();
                    index.row = -1;
                    toControl = nextEnabled(activePanel->controls(), index, MoveDirection::Down);
                }
            }
        }
    } break;
    case MoveDirection::Down: {
        if (!activeControl) { // no any active
            toControl = firstEnabled(activePanel->controls(), INavigation::Index(), MoveDirection::Down);
        } else {
            toControl = nextEnabled(activePanel->controls(), activeControl->index(), MoveDirection::Down);
            if (!toControl && tryOtherOrientation) {
                toControl = nextEnabled(activePanel->controls(), activeControl->index(), MoveDirection::Right);
            }
            if (!toControl) { // active is last
                INavigation::Index index = activeControl->index();
                index.row = -1;
                toControl = firstEnabled(activePanel->controls(), index, MoveDirection::Down); // the first to be the next

                if (!toControl && tryOtherOrientation) {
                    index = activeControl->index();
                    index.column = -1;
                    toControl = nextEnabled(activePanel->controls(), index, MoveDirection::Right);
                }
            }
        }
    } break;
    case MoveDirection::Left: {
        if (!activeControl) { // no any active
            toControl = lastEnabled(activePanel->controls(), INavigation::Index(), MoveDirection::Left);
        } else {
            toControl = nextEnabled(activePanel->controls(), activeControl->index(), MoveDirection::Left);
            if (!toControl && tryOtherOrientation) {
                toControl = nextEnabled(activePanel->controls(), activeControl->index(), MoveDirection::Up);
            }
            if (!toControl) { // active is first
                INavigation::Index index = activeControl->index();
                index.column = std::numeric_limits<int>::max();
                toControl = lastEnabled(activePanel->controls(), index, MoveDirection::Left); // the last to be the next

                if (!toControl && tryOtherOrientation) {
                    index = activeControl->index();
                    index.row = std::numeric_limits<int>::max();
                    toControl = nextEnabled(activePanel->controls(), index, MoveDirection::Up);
                }
            }
        }
    } break;
    case MoveDirection::Up: {
        if (!activeControl) { // no any active
            toControl = lastEnabled(activePanel->controls(), INavigation::Index(), MoveDirection::Up);
        } else {
            toControl = nextEnabled(activePanel->controls(), activeControl->index(), MoveDirection::Up);
            if (!toControl && tryOtherOrientation) {
                toControl = nextEnabled(activePanel->controls(), activeControl->index(), MoveDirection::Left);
            }
            if (!toControl) { // active is first
                INavigation::Index index = activeControl->index();
                index.row = std::numeric_limits<int>::max();
                toControl = lastEnabled(activePanel->controls(), index, MoveDirection::Up); // the last to be the next

                if (!toControl && tryOtherOrientation) {
                    index = activeControl->index();
                    index.column = std::numeric_limits<int>::max();
                    toControl = nextEnabled(activePanel->controls(), index, MoveDirection::Left);
                }
            }
        }
    } break;
    }

    if (!toControl) {
        return;
    }

    //! NOTE Maybe just one control (or just one enabled control)
    if (toControl == activeControl) {
        return;
    }

    if (activeControl) {
        MYLOG() << "current activated control: " << activeControl->name()
                << ", row: " << activeControl->index().row
                << ", column: " << activeControl->index().column;

        doDeactivateControl(activeControl);
    }

    doActivateControl(toControl);

    m_navigationChanged.notify();
}

void NavigationController::doTriggerControl()
{
    TRACEFUNC;
    MYLOG() << "====";
    INavigationPanel* activePanel = this->activePanel();
    if (!activePanel) {
        return;
    }

    INavigation::EventPtr e = Event::make(Event::Trigger);
    activePanel->onEvent(e);
    if (e->accepted) {
        return;
    }

    INavigationControl* activeCtrl = findActive(activePanel->controls());
    if (!activeCtrl) {
        return;
    }

    MYLOG() << "triggered control: " << activeCtrl->name();
    activeCtrl->onEvent(e);
    if (e->accepted) {
        return;
    }

    activeCtrl->trigger();
}

bool NavigationController::requestActivateByName(const std::string& sectName, const std::string& panelName, const std::string& controlName)
{
    INavigationSection* section = findByName(m_sections, QString::fromStdString(sectName));
    if (!section) {
        LOGE() << "not found section with name: " << sectName;
        return false;
    }

    INavigationPanel* panel = findByName(section->panels(), QString::fromStdString(panelName));
    if (!panel) {
        LOGE() << "not found panel with name: " << panelName << ", section: " << sectName;
        return false;
    }

    INavigationControl* control = findByName(panel->controls(), QString::fromStdString(controlName));
    if (!control) {
        LOGE() << "not found control with name: " << controlName << ", panel: " << panelName << ", section: " << sectName;
        QString has = "has:\n";
        for (const INavigationControl* c : panel->controls()) {
            has += c->name() + "\n";
        }
        LOGI() << has;
        return false;
    }

    onActiveRequested(section, panel, control, true);
    return true;
}

bool NavigationController::requestActivateByIndex(const std::string& sectName, const std::string& panelName,
                                                  const INavigation::Index& controlIndex)
{
    INavigationSection* section = findByName(m_sections, QString::fromStdString(sectName));
    if (!section) {
        LOGE() << "not found section with name: " << sectName;
        return false;
    }

    INavigationPanel* panel = findByName(section->panels(), QString::fromStdString(panelName));
    if (!panel) {
        LOGE() << "not found panel with name: " << panelName << ", section: " << sectName;
        return false;
    }

    INavigationControl* control = findByIndex(panel->controls(), controlIndex);
    if (!control) {
        LOGE() << "not found control with index: " << controlIndex.to_string() << ", panel: " << panelName << ", section: " << sectName;
        std::string has = "has:\n";
        for (const INavigationControl* c : panel->controls()) {
            has += c->index().to_string() + "\n";
        }
        LOGI() << has;
        return false;
    }

    onActiveRequested(section, panel, control, true);
    return true;
}

void NavigationController::onActiveRequested(INavigationSection* sect, INavigationPanel* panel, INavigationControl* ctrl, bool force)
{
    TRACEFUNC;
    UNUSED(force);

    if (m_sections.empty()) {
        return;
    }

    bool isChanged = false;

    DEFER {
        if (isChanged) {
            m_navigationChanged.notify();
        }
    };

    INavigationSection* activeSec = findActive(m_sections);
    if (activeSec && activeSec != sect) {
        doDeactivateSection(activeSec);
    }

    if (!sect->active()) {
        sect->setActive(true);
        isChanged = true;
        MYLOG() << "activated section: " << sect->name() << ", order: " << sect->index().order();
    }

    if (!panel) {
        panel = firstEnabled(sect->panels());
    }

    INavigationPanel* activePanel = findActive(sect->panels());
    if (activePanel && activePanel != panel) {
        doDeactivatePanel(activePanel);
    }

    if (!panel) {
        return;
    }

    if (!panel->active()) {
        panel->setActive(true);
        isChanged = true;
        MYLOG() << "activated panel: " << panel->name() << ", order: " << panel->index().order();
    }

    if (!ctrl) {
        ctrl = firstEnabled(panel->controls());
    }

    INavigationControl* activeCtrl = findActive(panel->controls());
    if (activeCtrl && activeCtrl != ctrl) {
        activeCtrl->setActive(false);
    }

    if (!ctrl) {
        return;
    }

    if (!ctrl->active()) {
        ctrl->setActive(true);
        isChanged = true;
        MYLOG() << "activated control: " << ctrl->name() << ", row: " << ctrl->index().row << ", column: " << ctrl->index().column;
    }
}

inline QTextStream& operator<<(QTextStream& s, const std::string& v)
{
    s << v.c_str();
    return s;
}

void NavigationController::dump() const
{
    QString str;
    QTextStream stream(&str);
    stream << "Navigation Dump:" << Qt::endl;
    for (const INavigationSection* sect : m_sections) {
        stream << "section: " << sect->name()
               << ", index: " << sect->index().to_string()
               << ", enabled: " << sect->enabled()
               << ", active: " << sect->active()
               << Qt::endl;

        for (const INavigationPanel* panel : sect->panels()) {
            stream << "    panel: " << panel->name()
                   << ", index: " << panel->index().to_string()
                   << ", enabled: " << panel->enabled()
                   << ", active: " << panel->active()
                   << Qt::endl;

            for (const INavigationControl* ctrl : panel->controls()) {
                stream << "        control: " << ctrl->name()
                       << ", index: " << ctrl->index().to_string()
                       << ", enabled: " << ctrl->enabled()
                       << ", active: " << ctrl->active()
                       << Qt::endl;
            }
        }
    }

    stream << Qt::endl;

    std::cout << str.toStdString();
}
