/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2024 MuseScore BVBA and others
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

#include "ui/view/qmltooltip.h"

using namespace mu;
using namespace mu::ui;

namespace mu::ui {
class QmlToolTipTests : public ::testing::Test, public QObject
{
public:
    void SetUp() override
    {
        m_tooltip = new QmlToolTip();

        QObject::connect(m_tooltip, &QmlToolTip::showToolTip, this, [this]() {
            m_isToolTipShown = true;
        });

        QObject::connect(m_tooltip, &QmlToolTip::hideToolTip, this, [this]() {
            m_isToolTipShown = false;
        });
    }

    void TearDown() override
    {
        delete m_tooltip;
    }

protected:
    bool isToolTipInited() const
    {
        return m_tooltip->m_item != nullptr;
    }

    bool isToolTipShown() const
    {
        return m_isToolTipShown;
    }

    bool isOpenTimerActive() const
    {
        return m_tooltip->m_openTimer.isActive();
    }

    void waitForToolTipOpen()
    {
        QTimer timer;
        QEventLoop loop;
        QObject::connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);
        timer.start(1000);
        loop.exec();
    }

    QmlToolTip* m_tooltip = nullptr;
    bool m_isToolTipShown = false;
};

TEST_F(QmlToolTipTests, Show_ByTimer)
{
    //! [GIVEN] A QQuickItem item
    QQuickItem* item = new QQuickItem();

    //! [WHEN] Show the tooltip
    m_tooltip->show(item, "title");

    //! [THEN] The tooltip should be inited, but not shown, and the open timer should be active
    ASSERT_FALSE(isToolTipShown());
    ASSERT_TRUE(isToolTipInited());
    ASSERT_TRUE(isOpenTimerActive());

    //! [WHEN] Wait for the tooltip to open
    waitForToolTipOpen();

    //! [THEN] The tooltip should be shown
    ASSERT_TRUE(isToolTipShown());
    ASSERT_TRUE(isToolTipInited());
    ASSERT_FALSE(isOpenTimerActive());

    delete item;
}

TEST_F(QmlToolTipTests, Hide)
{
    //! [GIVEN] A QQuickItem item
    QQuickItem* item = new QQuickItem();

    //! [WHEN] Show the tooltip
    m_tooltip->show(item, "title");

    //! [THEN] The tooltip should be inited, but not shown, and the open timer should be active
    ASSERT_FALSE(isToolTipShown());
    ASSERT_TRUE(isToolTipInited());
    ASSERT_TRUE(isOpenTimerActive());

    //! [WHEN] Wait for the tooltip to open
    waitForToolTipOpen();

    //! [THEN] The tooltip should be shown
    ASSERT_TRUE(isToolTipShown());
    ASSERT_TRUE(isToolTipInited());
    ASSERT_FALSE(isOpenTimerActive());

    //! [WHEN] Hide the tooltip
    m_tooltip->hide(item, true);

    //! [THEN] The tooltip shouldn't be shown
    ASSERT_FALSE(isToolTipShown());
    ASSERT_FALSE(isToolTipInited());
    ASSERT_FALSE(isOpenTimerActive());

    delete item;
}

TEST_F(QmlToolTipTests, Hide_BeforeShow)
{
    //! [GIVEN] A QQuickItem item
    QQuickItem* item = new QQuickItem();

    //! [WHEN] Show the tooltip
    m_tooltip->show(item, "title");

    //! [THEN] The tooltip shouldn't be shown
    ASSERT_FALSE(isToolTipShown());
    ASSERT_TRUE(isToolTipInited());
    ASSERT_TRUE(isOpenTimerActive());

    //! [WHEN] No wait for the tooltip to open

    //! [WHEN] Hide the tooltip
    m_tooltip->hide(item, true);

    //! [THEN] The tooltip shouldn't be shown
    ASSERT_FALSE(isToolTipShown());
    ASSERT_FALSE(isToolTipInited());
    ASSERT_FALSE(isOpenTimerActive());

    delete item;
}

TEST_F(QmlToolTipTests, Hide_BeforeShowDestroyItem)
{
    //! [GIVEN] A QQuickItem item
    QQuickItem* item = new QQuickItem();

    //! [WHEN] Show the tooltip
    m_tooltip->show(item, "title");

    //! [THEN] The tooltip shouldn't be shown
    ASSERT_FALSE(isToolTipShown());
    ASSERT_TRUE(isToolTipInited());
    ASSERT_TRUE(isOpenTimerActive());

    //! [WHEN] Destroy the item
    delete item;

    //! [THEN] The tooltip shouldn't be shown
    ASSERT_FALSE(isToolTipShown());
    ASSERT_FALSE(isToolTipInited());
    ASSERT_FALSE(isOpenTimerActive());

    //! [WHEN] Wait for the tooltip to open
    waitForToolTipOpen();

    //! [THEN] The tooltip shouldn't be shown
    ASSERT_FALSE(isToolTipShown());
    ASSERT_FALSE(isToolTipInited());
    ASSERT_FALSE(isOpenTimerActive());
}
}
