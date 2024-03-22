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

#include "menuview.h"

#include "log.h"

using namespace mu::uicomponents;

static const QString MENU_VIEW_CONTENT_OBJECT_NAME("_MenuViewContent");

MenuView::MenuView(QQuickItem* parent)
    : PopupView(parent)
{
    setObjectName("MenuView");
    setErrCode(Ret::Code::Ok);

    setPadding(8);
    setShowArrow(false);
}

int MenuView::viewVerticalMargin() const
{
    return 4;
}

Qt::AlignmentFlag MenuView::cascadeAlign() const
{
    return m_cascadeAlign;
}

void MenuView::setCascadeAlign(Qt::AlignmentFlag cascadeAlign)
{
    if (m_cascadeAlign == cascadeAlign) {
        return;
    }

    m_cascadeAlign = cascadeAlign;
    emit cascadeAlignChanged(m_cascadeAlign);
}

void MenuView::componentComplete()
{
    m_contentItem->setObjectName(m_contentItem->objectName() + MENU_VIEW_CONTENT_OBJECT_NAME);

    PopupView::componentComplete();
}

void MenuView::updateGeometry()
{
    const QQuickItem* parent = parentItem();
    IF_ASSERT_FAILED(parent) {
        return;
    }

    QPointF parentTopLeft = parent->mapToGlobal(QPoint(0, 0));

    if (m_globalPos.isNull()) {
        m_globalPos = parentTopLeft + m_localPos;
    }

    QRectF anchorRect = anchorGeometry();
    QRectF viewRect = viewGeometry();

    setOpensUpward(false);
    setCascadeAlign(Qt::AlignmentFlag::AlignRight);

    auto movePos = [this, &viewRect](qreal x, qreal y) {
        m_globalPos.setX(x);
        m_globalPos.setY(y);

        viewRect.moveTopLeft(m_globalPos);
    };

    const QQuickItem* parentMenuContentItem = this->parentMenuContentItem();
    bool isCascade = parentMenuContentItem != nullptr;

    if (isCascade) {
        movePos(parentTopLeft.x() + parent->width(), m_globalPos.y() - parent->height() - viewVerticalMargin());
    }

    if (viewRect.left() < anchorRect.left()) {
        // move to the right to an area that doesn't fit
        movePos(m_globalPos.x() + anchorRect.left() - viewRect.left(), m_globalPos.y());
    }

    if (viewRect.bottom() > anchorRect.bottom()) {
        if (isCascade) {
            // move to the top to an area that doesn't fit
            movePos(m_globalPos.x(), m_globalPos.y() - (viewRect.bottom() - anchorRect.bottom()));
        } else {
            qreal newY = parentTopLeft.y() - viewRect.height();
            if (anchorRect.top() < newY) {
                // move to the top of the parent
                movePos(m_globalPos.x(), newY);
                setOpensUpward(true);
            } else {
                // move to the right of the parent and move to top to an area that doesn't fit
                movePos(parentTopLeft.x() + parent->width(), m_globalPos.y() - (viewRect.bottom() - anchorRect.bottom()));
            }
        }
    }

    Qt::AlignmentFlag parentCascadeAlign = this->parentCascadeAlign(parentMenuContentItem);
    if (viewRect.right() > anchorRect.right() || parentCascadeAlign != Qt::AlignmentFlag::AlignRight) {
        if (isCascade) {
            // move to the right of the parent
            movePos(parentTopLeft.x() - viewRect.width() + padding() * 2, m_globalPos.y());
            setCascadeAlign(Qt::AlignmentFlag::AlignLeft);
        } else {
            // move to the left to an area that doesn't fit
            movePos(m_globalPos.x() - (viewRect.right() - anchorRect.right()) + padding() * 2, m_globalPos.y());
        }
    }

    // remove padding for arrow
    movePos(m_globalPos.x() - padding(), m_globalPos.y());
}

QRect MenuView::viewGeometry() const
{
    return QRect(m_globalPos.toPoint(), QSize(contentWidth() + padding() * 2, contentHeight() + padding() * 2));
}

Qt::AlignmentFlag MenuView::parentCascadeAlign(const QQuickItem* parent) const
{
    if (!parent) {
        return Qt::AlignmentFlag::AlignRight;
    }

    return static_cast<Qt::AlignmentFlag>(parent->property("cascadeAlign").toInt());
}

QQuickItem* MenuView::parentMenuContentItem() const
{
    QQuickItem* parent = parentItem();
    while (parent) {
        if (parent->objectName().contains(MENU_VIEW_CONTENT_OBJECT_NAME)) {
            return parent;
        }

        parent = parent->parentItem();
    }

    return nullptr;
}

int MenuView::contentWidth() const
{
    return m_contentWidth;
}

void MenuView::setContentWidth(int newContentWidth)
{
    if (m_contentWidth == newContentWidth) {
        return;
    }

    m_contentWidth = newContentWidth;
    emit contentWidthChanged();
}

int MenuView::contentHeight() const
{
    return m_contentHeight;
}

void MenuView::setContentHeight(int newContentHeight)
{
    if (m_contentHeight == newContentHeight) {
        return;
    }

    m_contentHeight = newContentHeight;
    emit contentHeightChanged();
}
