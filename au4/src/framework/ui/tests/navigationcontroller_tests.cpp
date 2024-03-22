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
#include <gtest/gtest.h>

#include <vector>

#include <QWindow>
#include <QQuickWindow>

#include "ui/internal/navigationcontroller.h"
#include "actions/internal/actionsdispatcher.h"

#include "global/tests/mocks/applicationmock.h"
#include "ui/tests/mocks/mainwindowmock.h"

#include "ui/view/navigationcontrol.h"
#include "ui/view/navigationpanel.h"
#include "ui/view/navigationsection.h"

#include "log.h"

using ::testing::Return;
using ::testing::ReturnRef;
using ::testing::NiceMock;
using ::testing::_;
using ::testing::SaveArgPointee;
using ::testing::DoAll;

using namespace mu;
using namespace mu::ui;

class Ui_NavigationControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_controller = std::make_shared<NavigationController>();

        m_dispatcher = std::make_shared<actions::ActionsDispatcher>();
        m_controller->setdispatcher(m_dispatcher);

        m_mainWindow = std::make_shared<ui::MainWindowMock>();
        ON_CALL(*m_mainWindow, qWindow()).WillByDefault(Return(&m_window));
        m_controller->setmainWindow(m_mainWindow);

        m_applicationMock = std::make_shared<ApplicationMock>();
        ON_CALL(*m_applicationMock, focusWindow()).WillByDefault(Return(&m_window));

        m_controller->init();

        m_idxsRefs.reserve(10000);
    }

    INavigation::Index& make_idx()
    {
        m_idxsRefs.push_back(INavigation::Index());
        return m_idxsRefs.back();
    }

    struct Control {
        NavigationControl* control = nullptr;

        ~Control()
        {
            delete control;
        }
    };

    struct Panel {
        NavigationPanel* panel = nullptr;
        std::vector<Control*> controls;
        std::set<INavigationControl*> icontrols;

        ~Panel()
        {
            delete panel;
            for (Control* c : controls) {
                delete c;
            }
        }
    };

    struct Section {
        NavigationSection* section = nullptr;
        std::vector<Panel*> panels;
        std::set<INavigationPanel*> ipanels;
        OnActiveRequested activeCallback;

        ~Section()
        {
            delete section;
            for (Panel* p : panels) {
                delete p;
            }
        }
    };

    Control* make_control(INavigation::Index& idx)
    {
        Control* c = new Control();
        NavigationControl* navCtrl = new NavigationControl();
        navCtrl->setRow(idx.row);
        navCtrl->setColumn(idx.column);
        navCtrl->setOrder(idx.order());
        navCtrl->setEnabled(true);
        navCtrl->setActive(false);

        setComponentWindow(navCtrl, &m_window);

        navCtrl->setnavigationController(m_controller);

        c->control = navCtrl;

        return c;
    }

    Panel* make_panel(int panelOrder, size_t controlsCount)
    {
        Panel* p = new Panel();

        NavigationPanel* navPanel = new NavigationPanel();

        for (size_t ci = 0; ci < controlsCount; ++ci) {
            INavigation::Index& idx = make_idx();
            idx.column = static_cast<int>(ci);

            Control* c = make_control(idx);
            c->control->setPanel(navPanel);

            p->controls.push_back(c);
            p->icontrols.insert(c->control);
        }

        navPanel->setEnabled(true);
        navPanel->setActive(false);

        INavigation::Index& idx = make_idx();
        idx.setOrder(panelOrder);

        navPanel->setRow(idx.row);
        navPanel->setColumn(idx.column);
        navPanel->setOrder(idx.order());

        setComponentWindow(navPanel, &m_window);

        navPanel->setnavigationController(m_controller);

        p->panel = navPanel;

        return p;
    }

    Section* make_section(int sectOrder, size_t panelsCount, size_t controlsCount)
    {
        Section* s = new Section();

        NavigationSection* navSection = new NavigationSection();

        for (size_t pi = 0; pi < panelsCount; ++pi) {
            Panel* p = make_panel(static_cast<int>(pi), controlsCount);
            p->panel->setSection(navSection);

            s->panels.push_back(p);
            s->ipanels.insert(p->panel);
        }

        navSection->setType(NavigationSection::QmlType::Regular);
        navSection->setEnabled(true);
        navSection->setActive(false);

        INavigation::Index& idx = make_idx();
        idx.setOrder(sectOrder);

        navSection->setRow(idx.row);
        navSection->setColumn(idx.column);
        navSection->setOrder(idx.order());

        setComponentWindow(navSection, &m_window);
        navSection->setapplication(m_applicationMock);

        navSection->setnavigationController(m_controller);

        s->section = navSection;

        return s;
    }

    void setComponentWindow(AbstractNavigation* navigation, const QQuickWindow* window)
    {
        QQuickItem* parentItem = new QQuickItem(window->contentItem());
        navigation->setParent(parentItem);
    }

    void print(Section* s)
    {
        LOGI() << "section: " << s->section->name() << ", idx: " << s->section->index().to_string()
               << ", active: " << s->section->active() << ", enabled: " << s->section->enabled();

        for (const Panel* p : s->panels) {
            LOGI() << "panel: " << p->panel->name() << ", idx: " << p->panel->index().to_string()
                   << ", active: " << p->panel->active() << ", enabled: " << p->panel->enabled();

            for (const Control* c : p->controls) {
                LOGI() << "control: " << c->control->name() << ", idx: " << c->control->index().to_string()
                       << ", active: " << c->control->active() << ", enabled: " << c->control->enabled();
            }
        }
    }

    std::shared_ptr<NavigationController> m_controller;
    std::shared_ptr<actions::IActionsDispatcher> m_dispatcher;
    std::shared_ptr<MainWindowMock> m_mainWindow;
    std::shared_ptr<ApplicationMock> m_applicationMock;

    QQuickWindow m_window;

    //! NOTE Garbage and references
    std::vector<INavigation::Index> m_idxsRefs;
};

TEST_F(Ui_NavigationControllerTests, FirstActiveOnNextSection)
{
    //! CASE Nothing active, and we call next section (F6)

    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [WHEN] Send action `nav-next-section` (usually F6)
    m_dispatcher->dispatch("nav-next-section");

    //! [THEN] The first section, the first panel, the first control must be activated
    EXPECT_TRUE(sect1->section->active());
    EXPECT_TRUE(sect1->panels[0]->panel->active());
    EXPECT_TRUE(sect1->panels[0]->controls[0]->control->active());

    //! [THEN] The second section must not be activated
    EXPECT_FALSE(sect2->section->active());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, FirstActiveOnNextPanelOnExclusiveSection)
{
    //! CASE Nothing active, and we call next panel (Tab)

    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    sect1->section->setName("sect1");
    Section* sect2 = make_section(2, 2, 3);
    sect2->section->setName("sect2");

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [GIVEN] Section 2 is exclusive and active
    sect2->section->setType(NavigationSection::QmlType::Exclusive);
    sect2->section->requestActive();

    //! [GIEN] Last panel of section 2 is active
    sect2->panels.back()->panel->requestActive();

    //! [WHEN] Send action `nav-next-panel` (usually Tab)
    m_dispatcher->dispatch("nav-next-panel");

    //! [THEN] The second section, the first panel, the first control must be activated
    EXPECT_TRUE(sect2->section->active());
    EXPECT_TRUE(sect2->panels[0]->panel->active());
    EXPECT_TRUE(sect2->panels[0]->controls[0]->control->active());

    //! [THEN] The first section must not be activated
    EXPECT_FALSE(sect1->section->active());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, FirstActiveOnNextSectionOnExclusiveSection)
{
    //! CASE Nothing active, and we call next section (F6)

    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [GIVEN] Section 2 is exclusive and active
    sect2->section->setType(NavigationSection::QmlType::Exclusive);
    sect2->section->requestActive();

    //! [WHEN] Send action `nav-next-section` (usually F6)
    m_dispatcher->dispatch("nav-next-section");

    //! [THEN] The second section, the first panel, the first control must be activated
    EXPECT_TRUE(sect2->section->active());
    EXPECT_TRUE(sect2->panels[0]->panel->active());
    EXPECT_TRUE(sect2->panels[0]->controls[0]->control->active());

    //! [THEN] The first section must not be activated
    EXPECT_FALSE(sect1->section->active());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, FirstActiveOnNextSectionExclusive)
{
    //! CASE Nothing active, and we call next section (F6)

    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [GIVEN] Section 2 is exclusive,
    //!         we have this behavior for menus and dropdowns
    sect2->section->setType(NavigationSection::Exclusive);
    sect2->section->requestActive();

    //! [WHEN] Send action `nav-next-section` (usually F6)
    m_dispatcher->dispatch("nav-next-section");

    //! [THEN] The second section, the first panel, the first control must be activated
    EXPECT_TRUE(sect2->section->active());
    EXPECT_TRUE(sect2->panels[0]->panel->active());
    EXPECT_TRUE(sect2->panels[0]->controls[0]->control->active());

    //! [THEN] The first section must not be activated
    EXPECT_FALSE(sect1->section->active());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, FirstActiveOnPrevSectionExclusive)
{
    //! CASE Nothing active, and we call previous section (Shift+F6)

    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [GIVEN] Section 2 is exclusive,
    //!         we have this behavior for menus and dropdowns
    sect2->section->setType(NavigationSection::Exclusive);
    sect2->section->requestActive();

    //! [WHEN] Send action `nav-next-section` (usually F6)
    m_dispatcher->dispatch("nav-prev-section");

    //! [THEN] The second section, the first panel, the first control must be activated
    EXPECT_TRUE(sect2->section->active());
    EXPECT_TRUE(sect2->panels[0]->panel->active());
    EXPECT_TRUE(sect2->panels[0]->controls[0]->control->active());

    //! [THEN] The first section must not be activated
    EXPECT_FALSE(sect1->section->active());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, FirstActiveOnNextPanel)
{
    //! CASE Nothing active, and we call next panel (Tab)

    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! DO Send action `nav-next-section` (usually Tab)
    m_dispatcher->dispatch("nav-next-panel");

    //! [THEN] The first section, the first panel, the first control must be activated
    EXPECT_TRUE(sect1->section->active());
    EXPECT_TRUE(sect1->panels[0]->panel->active());
    EXPECT_TRUE(sect1->panels[0]->controls[0]->control->active());

    //! [THEN] The second section must not be activated
    EXPECT_FALSE(sect2->section->active());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, FirstActiveOnPrevSection)
{
    //! CASE Nothing active, and we call prev section (Shift+F6)

    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [WHEN] Send action `nav-next-section` (usually Shift+F6)
    m_dispatcher->dispatch("nav-prev-section");

    //! [THEN] The last section, the first panel, the first control must be activated.
    EXPECT_TRUE(sect2->section->active());
    EXPECT_TRUE(sect2->panels[0]->panel->active());
    EXPECT_TRUE(sect2->panels[0]->controls[0]->control->active());

    //! [THEN] The first section must not be activated
    EXPECT_FALSE(sect1->section->active());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, FirstActiveOnPrevPanel)
{
    //! CASE Nothing active, and we call prev panel (Shift+Tab)

    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [WHEN] Send action `nav-next-section` (usually Shift+Tab)
    m_dispatcher->dispatch("nav-prev-panel");

    //! [THEN] The second section, the first panel, the first control must be activated
    EXPECT_TRUE(sect2->section->active());
    EXPECT_TRUE(sect2->panels[0]->panel->active());
    EXPECT_TRUE(sect2->panels[0]->controls[0]->control->active());

    //! [THEN] The first section must not be activated
    EXPECT_FALSE(sect1->section->active());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, UserPressedSomeKeyHasActiveKey)
{
    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [GIVEN] Section 1, panel 1 and control 1 are active
    sect1->panels[1]->controls[1]->control->requestActive();

    //! [GIVEN] The navigation is not activated
    m_controller->setIsHighlight(false);

    //! [WHEN] The user has requested the activation of navigation on any key
    m_dispatcher->dispatch("nav-right");

    //! [THEN] Next control and highlight must be activated
    EXPECT_EQ(m_controller->activeControl(), sect1->panels[1]->controls[2]->control);
    EXPECT_TRUE(m_controller->isHighlight());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, UserClickedOnControlOnMainWindow)
{
    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [GIVEN] Control on main window
    setComponentWindow(sect1->panels[1]->controls[1]->control, &m_window);

    //! [WHEN] The user has clicked on control
    sect1->panels[1]->controls[1]->control->requestActiveByInteraction();

    //! [THEN] The control is not activated
    EXPECT_NE(m_controller->activeControl(), sect1->panels[1]->controls[1]->control);
    EXPECT_FALSE(m_controller->isHighlight());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, UserClickedOnControlOnNonMainWindow)
{
    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    //! [GIVEN] Second section on non main window
    sect2->section->setType(NavigationSection::QmlType::Exclusive);
    QQuickWindow* controlWindow = new QQuickWindow();
    setComponentWindow(sect2->panels[1]->controls[1]->control, controlWindow);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [WHEN] The user has clicked on control of second section
    sect2->panels[1]->controls[1]->control->requestActiveByInteraction();

    //! [THEN] The control is activated
    EXPECT_EQ(m_controller->activeControl(), sect2->panels[1]->controls[1]->control);
    EXPECT_FALSE(m_controller->isHighlight());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, UserClickedNotOnControl)
{
    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [GIVEN] First control is active
    sect1->panels[1]->controls[1]->control->requestActive();

    //! [WHEN] User clicked somewhere on application
    QEvent mouseEvent(QEvent::MouseButtonPress);
    qApp->sendEvent(qApp, &mouseEvent);

    //! [THEN] The control is not active
    EXPECT_FALSE(sect1->panels[1]->controls[1]->control->active());
    EXPECT_FALSE(m_controller->isHighlight());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, UserClickedNotOnControlHasDefaultControl)
{
    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [GIVEN] Active control
    sect1->panels[1]->controls[1]->control->requestActive();

    //! [GIVEN] Default control
    m_controller->setDefaultNavigationControl(sect2->panels[0]->controls[1]->control);

    //! [WHEN] User clicked somewhere on application
    QEvent mouseEvent(QEvent::MouseButtonPress);
    qApp->sendEvent(qApp, &mouseEvent);

    //! [THEN] Active control was reseted to default control
    EXPECT_FALSE(sect1->panels[1]->controls[1]->control->active());
    EXPECT_TRUE(sect2->panels[0]->controls[1]->control->active());
    EXPECT_FALSE(m_controller->isHighlight());

    delete sect1;
    delete sect2;
}

TEST_F(Ui_NavigationControllerTests, UserClickedNotOnControlHasDefaultControlWithNotEnabledSection)
{
    //! [GIVEN] Two section, not active
    Section* sect1 = make_section(1, 2, 3);
    Section* sect2 = make_section(2, 2, 3);

    m_controller->reg(sect1->section);
    m_controller->reg(sect2->section);

    //! [GIVEN] Active control
    sect1->panels[1]->controls[1]->control->requestActive();

    //! [GIVEN] Default control in not enabled section
    m_controller->setDefaultNavigationControl(sect2->panels[0]->controls[1]->control);
    sect2->section->setEnabled(false);

    //! [WHEN] User clicked somewhere on application
    QEvent mouseEvent(QEvent::MouseButtonPress);
    qApp->sendEvent(qApp, &mouseEvent);

    //! [THEN] Active control was reseted
    EXPECT_FALSE(sect1->panels[1]->controls[1]->control->active());
    EXPECT_FALSE(sect2->panels[0]->controls[1]->control->active());
    EXPECT_FALSE(m_controller->isHighlight());

    delete sect1;
    delete sect2;
}
