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

#include "widgetview.h"

#include <QWidget>

using namespace mu::uicomponents;

WidgetView::WidgetView(QQuickItem* parent)
    : QuickPaintedView(parent)
{
    setFlag(QQuickItem::ItemAcceptsDrops, true);
    setFlag(QQuickItem::ItemHasContents, true);

    setAcceptHoverEvents(true);
    setAcceptedMouseButtons(Qt::AllButtons);
}

void WidgetView::paint(QPainter* painter)
{
    if (qWidget()) {
        qWidget()->render(painter, QPoint(), QRegion(),
                          QWidget::DrawWindowBackground | QWidget::DrawChildren);
    }
}

bool WidgetView::event(QEvent* event)
{
    if (!m_widget) {
        return QQuickItem::event(event);
    }

    bool ok = true;

    switch (event->type()) {
    case QEvent::HoverEnter:
    case QEvent::HoverMove:
    case QEvent::HoverLeave:
        ok = handleHoverEvent(dynamic_cast<QHoverEvent*>(event));
        break;
    case QEvent::MouseButtonPress:
    case QEvent::MouseButtonDblClick:
        setFocus(true);
        [[fallthrough]];
    default:
        ok = m_widget->handleEvent(event);
        break;
    }

    if (ok) {
        update();
    }

    return ok;
}

bool WidgetView::handleHoverEvent(QHoverEvent* event)
{
    auto convertEventType = [](QEvent::Type type) {
        static const QMap<QEvent::Type, QEvent::Type> types {
            { QEvent::HoverLeave, QEvent::Leave },
            { QEvent::HoverEnter, QEvent::Enter },
            { QEvent::HoverMove, QEvent::MouseMove }
        };

        return types[type];
    };

    QEvent::Type convertedType = convertEventType(event->type());

    if (convertedType == QEvent::MouseMove) {
#ifdef MU_QT5_COMPAT
        QPointF eventPos = event->posF();
#else
        QPointF eventPos = event->position();
#endif

        QMouseEvent mouseEvent(convertedType, eventPos,
                               Qt::NoButton, Qt::NoButton, event->modifiers());
        mouseEvent.setAccepted(event->isAccepted());
        mouseEvent.setTimestamp(event->timestamp());
        bool ok = m_widget->handleEvent(&mouseEvent);
        setCursor(qWidget()->cursor());
        return ok;
    }

    QEvent newEvent(convertedType);
    newEvent.setAccepted(event->isAccepted());

    return m_widget->handleEvent(&newEvent);
}

void WidgetView::componentComplete()
{
    QQuickItem::componentComplete();

    connect(this, &QQuickItem::widthChanged, [this]() {
        updateSizeConstraints();
    });

    connect(this, &QQuickItem::heightChanged, [this]() {
        updateSizeConstraints();
    });
}

QWidget* WidgetView::qWidget() const
{
    return m_widget ? m_widget->qWidget() : nullptr;
}

void WidgetView::updateSizeConstraints()
{
    if (qWidget()) {
        qWidget()->setFixedSize(width(), height());
    }
}

void WidgetView::setWidget(std::shared_ptr<IDisplayableWidget> widget)
{
    m_widget = widget;

    updateSizeConstraints();
}
