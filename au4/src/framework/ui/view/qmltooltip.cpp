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
#include "qmltooltip.h"

#include <QGuiApplication>

static constexpr int INTERVAL = 500;

using namespace mu::ui;

QmlToolTip::QmlToolTip(QObject* parent)
    : QObject(parent)
{
    connect(&m_openTimer, &QTimer::timeout, this, &QmlToolTip::doShow);

    m_closeTimer.setSingleShot(true);
    connect(&m_closeTimer, &QTimer::timeout, this, &QmlToolTip::doHide);

    qApp->installEventFilter(this);
}

void QmlToolTip::show(QQuickItem* item, const QString& title, const QString& description, const QString& shortcut)
{
    if (item == m_item) {
        m_closeTimer.stop();
        return;
    }

    m_title = title;
    m_description = description;
    m_shortcut = shortcut;

    bool toolTipNotOpened = m_item == nullptr;
    bool openTimerStarted = m_openTimer.isActive();

    m_item = item;
    m_shouldBeClosed = false;

    if (m_item) {
        connect(m_item, &QObject::destroyed, this, &QmlToolTip::onItemDestruction);
    }

    if (toolTipNotOpened || openTimerStarted) {
        m_openTimer.start(INTERVAL);
    } else {
        doShow();
    }
}

void QmlToolTip::hide(QQuickItem* item, bool force)
{
    if (m_item != item) {
        return;
    }

    m_shouldBeClosed = true;

    if (force) {
        doHide();
        return;
    }

    m_closeTimer.start(INTERVAL);
}

void QmlToolTip::init()
{
    interactiveProvider()->currentUriAboutToBeChanged().onNotify(this, [this]() {
        m_shouldBeClosed = true;
        doHide();
    });
}

void QmlToolTip::doShow()
{
    m_openTimer.stop();
    m_closeTimer.stop();

    if (!m_item) {
        return;
    }

    if (m_shouldBeClosed) {
        clear();
        return;
    }

    emit showToolTip(m_item, m_title, m_description, m_shortcut);
}

void QmlToolTip::onItemDestruction()
{
    m_shouldBeClosed = true;
    doHide();
}

void QmlToolTip::doHide()
{
    if (!m_shouldBeClosed) {
        return;
    }

    m_openTimer.stop();
    m_closeTimer.stop();

    clear();

    emit hideToolTip();
}

bool QmlToolTip::eventFilter(QObject*, QEvent* event)
{
    if (event->type() == QEvent::Wheel) {
        m_shouldBeClosed = true;
        doHide();
    }

    return false;
}

void QmlToolTip::clear()
{
    if (m_item) {
        disconnect(m_item, &QObject::destroyed, this, &QmlToolTip::onItemDestruction);
    }

    m_item = nullptr;
    m_title = QString();
    m_description = QString();
    m_shortcut = QString();

    m_shouldBeClosed = false;
}
