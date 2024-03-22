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
#ifndef MU_UI_NAVIGATIONCONTROLLER_H
#define MU_UI_NAVIGATIONCONTROLLER_H

#include <QObject>
#include <QList>

#include "modularity/ioc.h"
#include "global/iinteractive.h"
#include "async/asyncable.h"
#include "ui/imainwindow.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"

#include "../inavigationcontroller.h"

namespace mu::ui {
class NavigationController : public QObject, public INavigationController, public actions::Actionable, public async::Asyncable
{
    INJECT(actions::IActionsDispatcher, dispatcher)
    INJECT(IInteractive, interactive)
    INJECT(IMainWindow, mainWindow)

public:
    NavigationController() = default;

    enum MoveDirection {
        First = 0,
        Last,
        Right,
        Left,
        Up,
        Down
    };

    void reg(INavigationSection* section) override;
    void unreg(INavigationSection* section) override;

    const std::set<INavigationSection*>& sections() const override;

    bool requestActivateByName(const std::string& section, const std::string& panel, const std::string& controlName) override;
    bool requestActivateByIndex(const std::string& section, const std::string& panel, const INavigation::Index& controlIndex) override;

    INavigationSection* activeSection() const override;
    INavigationPanel* activePanel() const override;
    INavigationControl* activeControl() const override;

    void setDefaultNavigationControl(INavigationControl* control) override;

    void resetNavigation() override;

    async::Notification navigationChanged() const override;

    bool isHighlight() const override;
    void setIsHighlight(bool isHighlight) override;
    async::Notification highlightChanged() const override;

    void setIsResetOnMousePress(bool arg) override;

    void dump() const override;

    void init();

private:

    enum class NavigationType {
        NextSection,
        PrevSection,
        PrevSectionActiveLastPanel,
        NextPanel,
        PrevPanel,
        Left,
        Right,
        Up,
        Down,
        FirstControl,
        LastControl,
        NextRowControl,
        PrevRowControl
    };

    bool eventFilter(QObject* watched, QEvent* event) override;

    void navigateTo(NavigationType type);

    void goToNextSection();
    void goToPrevSection(bool isActivateLastPanel = false);
    void goToNextPanel();
    void goToPrevPanel();

    void goToFirstControl();
    void goToLastControl();
    void goToNextRowControl();
    void goToPrevRowControl();

    void goToControl(MoveDirection direction, INavigationPanel* activePanel = nullptr);

    void onLeft();
    void onRight();
    void onUp();
    void onDown();
    void onEscape();

    void doTriggerControl();
    void onActiveRequested(INavigationSection* sect, INavigationPanel* panel, INavigationControl* ctrl, bool force = false);

    void doActivateSection(INavigationSection* sect, bool isActivateLastPanel = false);
    void doDeactivateSection(INavigationSection* sect);
    void doActivatePanel(INavigationPanel* panel);
    void doDeactivatePanel(INavigationPanel* panel);
    void doActivateControl(INavigationControl* ctrl);
    void doDeactivateControl(INavigationControl* ctrl);

    void doActivateFirst();
    void doActivateLast();

    void resetIfNeed(QObject* watched);

    std::set<INavigationSection*> m_sections;
    async::Notification m_navigationChanged;
    async::Notification m_highlightChanged;

    INavigationControl* m_defaultNavigationControl = nullptr;

    bool m_isHighlight = false;

    bool m_isResetOnMousePress = true;
};
}

#endif // MU_UI_NAVIGATIONCONTROLLER_H
