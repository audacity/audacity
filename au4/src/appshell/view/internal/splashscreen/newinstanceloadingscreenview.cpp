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

#include "newinstanceloadingscreenview.h"

#include <QApplication>
#include <QPainter>
#include <QScreen>

#include "translation.h"

using namespace au::appshell;

NewInstanceLoadingScreenView::NewInstanceLoadingScreenView(bool forNewScore, const QString& openingFileName, QWidget* parent)
    : QWidget(parent)
{
    setAttribute(Qt::WA_TranslucentBackground);

    if (forNewScore) {
        // When a new instance is being opened because a new score has to be created
        m_message = qtrc("appshell", "Loading new score…\u200e");
        m_dialogSize = QSize(288, 80);
    } else if (!openingFileName.isEmpty()) {
        // When a new instance is being opened because a score has to be opened, of which the name is known
        m_message = qtrc("appshell", "Loading “%1”…\u200e").arg(openingFileName);
        m_dialogSize = QSize(360, 80);
    } else {
        // When a new instance is being opened because a score has to be opened, of which the name is not known
        // This is the case when opening a score from the file manager or from MuseScore.com on non-macOS systems,
        // because then a new instance is launched by the OS, which doesn't tell MuseScore about the name of the score.
        m_message = qtrc("appshell", "Loading score…\u200e");
        m_dialogSize = QSize(288, 80);
    }

    resize(m_dialogSize);
}

bool NewInstanceLoadingScreenView::event(QEvent* event)
{
    if (event->type() == QEvent::Paint) {
        QPainter painter(this);
        painter.setLayoutDirection(layoutDirection());
        draw(&painter);
    }

    return QWidget::event(event);
}

void NewInstanceLoadingScreenView::draw(QPainter* painter)
{
    painter->setRenderHints(QPainter::Antialiasing | QPainter::TextAntialiasing | QPainter::SmoothPixmapTransform);

    // Draw background
    QString bgColorStr = uiConfiguration()->currentTheme().values.value(ui::BACKGROUND_PRIMARY_COLOR).toString();
    painter->fillRect(0, 0, width(), height(), QColor(bgColorStr));

    // Draw message
    QFont font(QString::fromStdString(uiConfiguration()->fontFamily()));
    font.setPixelSize(uiConfiguration()->fontSize(ui::FontSizeType::BODY_LARGE));
    font.setBold(true);

    painter->setFont(font);

    QString messageColorStr = uiConfiguration()->currentTheme().values.value(ui::FONT_PRIMARY_COLOR).toString();
    QPen pen(messageColorStr);
    painter->setPen(pen);

    QFontMetrics fontMetrics(font);
    QRectF messageRectangle(0, 0, m_dialogSize.width(), m_dialogSize.height());
    messageRectangle -= QMargins(8, 8, 8, 8);

    QString elidedText = fontMetrics.elidedText(m_message, Qt::ElideMiddle, messageRectangle.width());
    painter->drawText(messageRectangle, Qt::AlignCenter | Qt::TextDontClip, elidedText);
}
