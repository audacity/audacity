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

#include "loadingscreenview.h"

#include <QApplication>
#include <QPainter>
#include <QScreen>
#include <QSvgRenderer>

#include "translation.h"

using namespace au::appshell;
using namespace muse;

static const QString imagePath(":/resources/LoadingScreen.svg");

static constexpr QSize loadingScreenSize(800, 380);

static const QColor messageColor("#99FFFFFF");
static constexpr QRectF messageRect(loadingScreenSize.width() / 2, 269, 0, 0);

static const QString website("www.audacityteam.org");
static constexpr QRectF websiteRect(76, 240, 0, 0);

static constexpr qreal versionNumberSpacing = 5.0;

LoadingScreenView::LoadingScreenView(QWidget* parent)
    : QWidget(parent),
    m_backgroundRenderer(new QSvgRenderer(imagePath, this))
{
    setAttribute(Qt::WA_TranslucentBackground);
    resize(loadingScreenSize);

    m_message = qtrc("appshell", "Loading…\u200e");
}

bool LoadingScreenView::event(QEvent* event)
{
    if (event->type() == QEvent::Paint) {
        QPainter painter(this);
        painter.setLayoutDirection(layoutDirection());
        draw(&painter);
    }

    return QWidget::event(event);
}

void LoadingScreenView::draw(QPainter* painter)
{
    painter->setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing | QPainter::SmoothPixmapTransform);

    // Draw background
    m_backgroundRenderer->render(painter);

    // Draw message
    QFont font(QString::fromStdString(uiConfiguration()->fontFamily()));
    font.setPixelSize(uiConfiguration()->fontSize());

    painter->setFont(font);

    QPen pen(messageColor);
    painter->setPen(pen);

    // painter->drawText(messageRect, Qt::AlignTop | Qt::AlignHCenter | Qt::TextDontClip, m_message);

    //! TODO AU4
    Qt::AlignmentFlag alignment = Qt::AlignLeft;
    // Qt::AlignmentFlag alignment = languagesService()->currentLanguage().direction == Qt::RightToLeft
    //                               ? Qt::AlignLeft : Qt::AlignRight;

    // Draw website URL
    QRectF websiteBoundingRect;
    painter->drawText(websiteRect, Qt::AlignBottom | alignment | Qt::TextDontClip, website, &websiteBoundingRect);

    // Draw version number
    pen.setColor(uiConfiguration()->currentTheme().extra["logo_main_color"].value<QColor>());
    painter->setPen(pen);

    painter->drawText(websiteRect.translated(0.0, -websiteBoundingRect.height() - versionNumberSpacing),
                      Qt::AlignBottom | alignment | Qt::TextDontClip,
                      qtrc("appshell", "Version %1").arg(application()->version().major()));
}
