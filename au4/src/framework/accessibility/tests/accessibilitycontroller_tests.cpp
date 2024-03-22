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

#include <QWindow>
#include <QKeyEvent>

#include "accessibility/internal/accessibilitycontroller.h"

#include "ui/tests/mocks/mainwindowmock.h"
#include "global/tests/mocks/applicationmock.h"
#include "mocks/accessibilityconfigurationmock.h"

using ::testing::Return;
using ::testing::_;
using ::testing::SaveArg;
using ::testing::DoAll;

using namespace mu;
using namespace mu::accessibility;

class Accessibility_ControllerTests : public ::testing::Test
{
public:

    void SetUp() override
    {
        m_controller = std::make_shared<AccessibilityController>();

        m_mainWindow = std::make_shared<ui::MainWindowMock>();
        m_controller->mainWindow.set(m_mainWindow);

        m_application = std::make_shared<ApplicationMock>();
        m_controller->application.set(m_application);

        m_configuration = std::make_shared<AccessibilityConfigurationMock>();
        m_controller->configuration.set(m_configuration);
    }

    class AccessibleItem : public accessibility::IAccessible
    {
    public:
        void setParent(AccessibleItem* parent) { m_parent = parent; }

        const IAccessible* accessibleParent() const override { return m_parent; }
        size_t accessibleChildCount() const override { return 0; }
        const IAccessible* accessibleChild(size_t) const override { return nullptr; }
        QWindow* accessibleWindow() const override { return nullptr; }
        IAccessible::Role accessibleRole() const override { return IAccessible::NoRole; }
        QString accessibleName() const override { return QString(); }
        QString accessibleDescription() const override { return QString(); }
        bool accessibleState(State) const override { return false; }
        QRect accessibleRect() const override { return QRect(); }
        bool accessibleIgnored() const override { return false; }

        QVariant accessibleValue() const override { return QString(); }
        QVariant accessibleMaximumValue() const override { return QString(); }
        QVariant accessibleMinimumValue() const override { return QString(); }
        QVariant accessibleValueStepSize() const override { return QString(); }

        void accessibleSelection(int, int*, int*) const override { }
        int accessibleSelectionCount() const override { return 0; }

        int accessibleCursorPosition() const override { return 0; }

        QString accessibleText(int, int) const override { return QString(); }

        QString accessibleTextBeforeOffset(int, TextBoundaryType, int*, int*) const override { return QString(); }
        QString accessibleTextAfterOffset(int, TextBoundaryType, int*, int*) const override { return QString(); }
        QString accessibleTextAtOffset(int, TextBoundaryType, int*, int*) const override { return QString(); }
        int accessibleCharacterCount() const override { return 0; }

        int accessibleRowIndex() const override { return 0; }

        async::Channel<IAccessible::Property, Val> accessiblePropertyChanged() const override
        {
            return m_propertyChanged;
        }

        async::Channel<IAccessible::State, bool> accessibleStateChanged() const override
        {
            return m_stateChanged;
        }

        void setState(State, bool) override {}

        async::Channel<IAccessible::Property, Val> m_propertyChanged;
        async::Channel<IAccessible::State, bool> m_stateChanged;

        AccessibleItem* m_parent = nullptr;
    };

    AccessibleItem* make_item()
    {
        AccessibleItem* item = new AccessibleItem();

        AccessibleItem* parent = new AccessibleItem();
        item->setParent(parent);

        return item;
    }

#ifdef Q_OS_LINUX
    void expectDispatchEventOnFocus(QEvent** event)
    {
        //! For Linux it needs to send spontaneous event for canceling reading the name of previous control on accessibility
        QWindow* window = new QWindow();
        EXPECT_CALL(*m_mainWindow, qWindow()).WillOnce(Return(window));
        EXPECT_CALL(*m_application, notify(window, _)).WillOnce(DoAll(SaveArg<1>(event), Return(true)));
    }

    void notExpectDispatchEventOnFocus()
    {
        //! If accessibility is active then the accessibility system should handle sended event,
        //! Otherwise, the item that is currently on focus will process the signal
        //! So, shouldn't dispatch event if accessibility is not active
        QWindow* window = new QWindow();
        EXPECT_CALL(*m_mainWindow, qWindow()).Times(0);
        EXPECT_CALL(*m_application, notify(window, _)).Times(0);
    }

    void checkDispatchEventOnFocus(const QEvent* event)
    {
        EXPECT_TRUE(event);
        EXPECT_TRUE(event->spontaneous());
    }

#else
    void expectDispatchEventOnFocus(QEvent**) {}
    void notExpectDispatchEventOnFocus() {}
    void checkDispatchEventOnFocus(const QEvent*) {}
#endif

    std::shared_ptr<AccessibilityController> m_controller;
    std::shared_ptr<ui::MainWindowMock> m_mainWindow;
    std::shared_ptr<AccessibilityConfigurationMock> m_configuration;
    std::shared_ptr<ApplicationMock> m_application;
};

TEST_F(Accessibility_ControllerTests, SendEventOnFocusChanged)
{
    //! [GIVEN] Accessibility is enabled
    ON_CALL(*m_configuration, enabled()).WillByDefault(Return(true));

    //! [GIVEN] Accessibility is active
    ON_CALL(*m_configuration, active()).WillByDefault(Return(true));

    //! [GIVEN] Two items
    AccessibleItem* item1 = make_item();
    AccessibleItem* item2 = make_item();

    //! [GIVEN] Register items
    m_controller->reg(item1);
    m_controller->reg(item2);

    //! [GIVEN] First is on focus
    item1->accessibleStateChanged().send(IAccessible::State::Focused, true);

    QEvent* event = nullptr;
    expectDispatchEventOnFocus(&event);

    //! [WHEN] Focus on second item
    item2->accessibleStateChanged().send(IAccessible::State::Focused, true);

    //! [THEN] It is expected that there will be an event dispatch when the focus is changed
    checkDispatchEventOnFocus(event);

    delete item1;
    delete item2;

    //! NOTE: need if tested class was created as a shared pointer
    testing::Mock::AllowLeak(m_mainWindow.get());
    testing::Mock::AllowLeak(m_application.get());
    testing::Mock::AllowLeak(m_configuration.get());
}

TEST_F(Accessibility_ControllerTests, NotSendEventOnFocusChangedIfAccessibilityIsNotActive)
{
    //! [GIVEN] Accessibility is enabled
    ON_CALL(*m_configuration, enabled()).WillByDefault(Return(true));

    //! [GIVEN] Accessibility is not active
    ON_CALL(*m_configuration, active()).WillByDefault(Return(false));

    //! [GIVEN] Two items
    AccessibleItem* item1 = make_item();
    AccessibleItem* item2 = make_item();

    //! [GIVEN] Register items
    m_controller->reg(item1);
    m_controller->reg(item2);

    notExpectDispatchEventOnFocus();

    //! [GIVEN] First is on focus
    item1->accessibleStateChanged().send(IAccessible::State::Focused, true);

    //! [WHEN] Focus on second item
    item2->accessibleStateChanged().send(IAccessible::State::Focused, true);

    delete item1;
    delete item2;

    //! NOTE: need if tested class was created as a shared pointer
    testing::Mock::AllowLeak(m_mainWindow.get());
    testing::Mock::AllowLeak(m_application.get());
    testing::Mock::AllowLeak(m_configuration.get());
}
