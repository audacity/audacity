/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2022 MuseScore BVBA and others
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

#include "splashscreen.h"

#include <QApplication>
#include <QPainter>
#include <QScreen>

#include "loadingscreenview.h"
#include "newinstanceloadingscreenview.h"

using namespace au::appshell;

#ifdef Q_OS_MAC
// Necessary to remove undesired background, so that we really get our rounded corners
static constexpr Qt::WindowFlags splashScreenWindowFlags = (Qt::SplashScreen | Qt::FramelessWindowHint) & ~Qt::Sheet | Qt::Window;
#else
static constexpr Qt::WindowFlags splashScreenWindowFlags = Qt::SplashScreen | Qt::FramelessWindowHint;
#endif

SplashScreen::SplashScreen(SplashScreen::SplashScreenType type, bool forNewScore, const QString& openingFileName)
    : QWidget(nullptr, splashScreenWindowFlags)
{
    setAttribute(Qt::WA_TranslucentBackground);

    switch (type) {
    case SplashScreen::Default:
        m_view = new LoadingScreenView(this);
        break;
    case SplashScreen::ForNewInstance:
        m_view = new NewInstanceLoadingScreenView(forNewScore, openingFileName, this);
        break;
    }

    setSize(m_view->size());

    repaint();
}

void SplashScreen::repaint()
{
    QWidget::repaint();
    QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents);
}

bool SplashScreen::event(QEvent* event)
{
    if (event->type() == QEvent::Paint) {
        QPainter painter(this);
        painter.setLayoutDirection(layoutDirection());
        draw(&painter);
    }

    return QWidget::event(event);
}

void SplashScreen::draw(QPainter* painter)
{
    if (m_view) {
        m_view->render(painter);
    }
}

void SplashScreen::setSize(const QSize& size)
{
    if (m_view) {
        resize(m_view->size());

        if (screen()) {
            move(screen()->geometry().center() - QPoint(size.width() / 2, size.height() / 2));
        }
    }
}
