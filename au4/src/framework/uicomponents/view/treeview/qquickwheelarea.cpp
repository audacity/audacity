/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** This file is part of the Qt Quick Controls module of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 3 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL3 included in the
** packaging of this file. Please review the following information to
** ensure the GNU Lesser General Public License version 3 requirements
** will be met: https://www.gnu.org/licenses/lgpl-3.0.html.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 2.0 or (at your option) the GNU General
** Public license version 3 or any later version approved by the KDE Free
** Qt Foundation. The licenses are as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL2 and LICENSE.GPL3
** included in the packaging of this file. Please review the following
** information to ensure the GNU General Public License requirements will
** be met: https://www.gnu.org/licenses/gpl-2.0.html and
** https://www.gnu.org/licenses/gpl-3.0.html.
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "qquickwheelarea_p.h"

QT_BEGIN_NAMESPACE

// On Mac OS X, the scrolling speed in Safari is roughly 2.5 times faster
// than in TextEdit (the native app). The former is using high-resolution
// pixel-based delta values as they are, which is fine for a typical web
// content, whereas the latter evidently makes scrolling slower to make it
// feel natural and more precise for typical document type of content.
// => we'll compromise between the two for now, and pick an arbitrary value
//    to make the pixel-based scrolling speed something between the two
static const qreal pixelDeltaAdjustment = 0.5;

// The default scroll speed for typical angle-based mouse wheels. The value
// comes originally from QTextEdit, which sets 20px steps by default.
static const qreal defaultScrollSpeed = 20.0;

QQuickWheelArea1::QQuickWheelArea1(QQuickItem *parent)
    : QQuickItem(parent),
      m_horizontalMinimumValue(0),
      m_horizontalMaximumValue(0),
      m_verticalMinimumValue(0),
      m_verticalMaximumValue(0),
      m_horizontalValue(0),
      m_verticalValue(0),
      m_verticalDelta(0),
      m_horizontalDelta(0),
      m_scrollSpeed(defaultScrollSpeed),
      m_active(false)
{

}

QQuickWheelArea1::~QQuickWheelArea1()
{

}

bool QQuickWheelArea1::isAtXEnd() const
{
    return qFuzzyCompare(m_horizontalMaximumValue, m_horizontalValue);
}

bool QQuickWheelArea1::isAtXBeginning() const
{
    return qFuzzyCompare(m_horizontalMinimumValue, m_horizontalValue);
}

bool QQuickWheelArea1::isAtYEnd() const
{
    return qFuzzyCompare(m_verticalMaximumValue, m_verticalValue);
}

bool QQuickWheelArea1::isAtYBeginning() const
{
    return qFuzzyCompare(m_verticalMinimumValue, m_verticalValue);
}

bool QQuickWheelArea1::isInverted() const
{
    return m_inverted;
}

#ifndef QT_NO_WHEELEVENT
void QQuickWheelArea1::wheelEvent(QWheelEvent *we)
{
    if (we->phase() == Qt::ScrollBegin)
        setActive(true);
    else if (we->phase() == Qt::ScrollEnd)
        setActive(false);

    QPoint numPixels = we->pixelDelta();
    QPoint numDegrees = we->angleDelta() / 8;

    m_inverted = we->inverted();

    if (!numPixels.isNull()) {
        setHorizontalDelta(numPixels.x() * pixelDeltaAdjustment);
        setVerticalDelta(numPixels.y() * pixelDeltaAdjustment);
    } else if (!numDegrees.isNull()) {
        setHorizontalDelta(numDegrees.x() / 15.0 * m_scrollSpeed);
        setVerticalDelta(numDegrees.y() / 15.0 * m_scrollSpeed);
    }

    // This allows other parent WheelArea's to handle scrolling
    // For example this allows for ScrollView inside of another ScrollView to work correctly
    // Once this scrollbar can't scroll anymore, ie it reaches the limits,
    // it will ignore the scroll event so the parent WheelArea can start scrolling
    if ((numPixels.x() != 0 || numDegrees.x() != 0) &&
        m_horizontalMinimumValue <= m_horizontalMaximumValue &&
        (isAtXBeginning() || isAtXEnd())) {
        we->ignore();
    } else if ((numPixels.y() != 0 || numDegrees.y() != 0) &&
               m_verticalMinimumValue <= m_verticalMaximumValue &&
               (isAtYBeginning() || isAtYEnd())) {
        we->ignore();
    } else {
        we->accept();
    }
}
#endif

void QQuickWheelArea1::setHorizontalMinimumValue(qreal value)
{
    if (value == m_horizontalMinimumValue)
        return;

    m_horizontalMinimumValue = value;
    emit horizontalMinimumValueChanged();
}

qreal QQuickWheelArea1::horizontalMinimumValue() const
{
    return m_horizontalMinimumValue;
}

void QQuickWheelArea1::setHorizontalMaximumValue(qreal value)
{
    if (value == m_horizontalMaximumValue)
        return;

    m_horizontalMaximumValue = value;
    emit horizontalMaximumValueChanged();
}

qreal QQuickWheelArea1::horizontalMaximumValue() const
{
    return m_horizontalMaximumValue;
}

void QQuickWheelArea1::setVerticalMinimumValue(qreal value)
{
    if (value == m_verticalMinimumValue)
        return;

    m_verticalMinimumValue = value;
    emit verticalMinimumValueChanged();
}

qreal QQuickWheelArea1::verticalMinimumValue() const
{
    return m_verticalMinimumValue;
}

void QQuickWheelArea1::setVerticalMaximumValue(qreal value)
{
    if (value == m_verticalMaximumValue)
        return;

    m_verticalMaximumValue = value;
    emit verticalMaximumValueChanged();
}

qreal QQuickWheelArea1::verticalMaximumValue() const
{
    return m_verticalMaximumValue;
}

void QQuickWheelArea1::setHorizontalValue(qreal value)
{
    value = qBound<qreal>(m_horizontalMinimumValue, value, m_horizontalMaximumValue);

    if (value != m_horizontalValue) {
        m_horizontalValue = value;
        emit horizontalValueChanged();
    }
}

qreal QQuickWheelArea1::horizontalValue() const
{
    return m_horizontalValue;
}

void QQuickWheelArea1::setVerticalValue(qreal value)
{
    value = qBound<qreal>(m_verticalMinimumValue, value, m_verticalMaximumValue);

    if (value != m_verticalValue) {
        m_verticalValue = value;
        emit verticalValueChanged();
    }
}

qreal QQuickWheelArea1::verticalValue() const
{
    return m_verticalValue;
}

void QQuickWheelArea1::setVerticalDelta(qreal value)
{
    m_verticalDelta = value;
    setVerticalValue(m_verticalValue - m_verticalDelta);

    emit verticalWheelMoved();
}

qreal QQuickWheelArea1::verticalDelta() const
{
    return m_verticalDelta;
}

void QQuickWheelArea1::setHorizontalDelta(qreal value)
{
    m_horizontalDelta = value;
    setHorizontalValue(m_horizontalValue - m_horizontalDelta);

    emit horizontalWheelMoved();
}

qreal QQuickWheelArea1::horizontalDelta() const
{
    return m_horizontalDelta;
}

void QQuickWheelArea1::setScrollSpeed(qreal value)
{
    if (value != m_scrollSpeed) {
        m_scrollSpeed = value;
        emit scrollSpeedChanged();
    }
}

qreal QQuickWheelArea1::scrollSpeed() const
{
    return m_scrollSpeed;
}

bool QQuickWheelArea1::isActive() const
{
    return m_active;
}

void QQuickWheelArea1::setActive(bool active)
{
    if (active != m_active) {
        m_active = active;
        emit activeChanged();
    }
}

QT_END_NAMESPACE
