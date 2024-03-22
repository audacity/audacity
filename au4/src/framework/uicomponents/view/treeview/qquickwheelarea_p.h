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

#ifndef QQUICKWHEELAREA_P_H
#define QQUICKWHEELAREA_P_H

#include <QtGui/qevent.h>
#include <QtQuick/qquickitem.h>

QT_BEGIN_NAMESPACE

class QQuickWheelArea1 : public QQuickItem
{
    Q_OBJECT
    Q_PROPERTY(qreal verticalDelta READ verticalDelta WRITE setVerticalDelta NOTIFY verticalWheelMoved)
    Q_PROPERTY(qreal horizontalDelta READ horizontalDelta WRITE setHorizontalDelta NOTIFY horizontalWheelMoved)
    Q_PROPERTY(qreal horizontalMinimumValue READ horizontalMinimumValue WRITE setHorizontalMinimumValue NOTIFY horizontalMinimumValueChanged)
    Q_PROPERTY(qreal horizontalMaximumValue READ horizontalMaximumValue WRITE setHorizontalMaximumValue NOTIFY horizontalMaximumValueChanged)
    Q_PROPERTY(qreal verticalMinimumValue READ verticalMinimumValue WRITE setVerticalMinimumValue NOTIFY verticalMinimumValueChanged)
    Q_PROPERTY(qreal verticalMaximumValue READ verticalMaximumValue WRITE setVerticalMaximumValue NOTIFY verticalMaximumValueChanged)
    Q_PROPERTY(qreal horizontalValue READ horizontalValue WRITE setHorizontalValue NOTIFY horizontalValueChanged)
    Q_PROPERTY(qreal verticalValue READ verticalValue WRITE setVerticalValue NOTIFY verticalValueChanged)
    Q_PROPERTY(qreal scrollSpeed READ scrollSpeed WRITE setScrollSpeed NOTIFY scrollSpeedChanged)
    Q_PROPERTY(bool active READ isActive WRITE setActive NOTIFY activeChanged)
    Q_PROPERTY(bool inverted READ isInverted)

public:
    QQuickWheelArea1(QQuickItem *parent = 0);
    virtual ~QQuickWheelArea1();

    void setHorizontalMinimumValue(qreal value);
    qreal horizontalMinimumValue() const;

    void setHorizontalMaximumValue(qreal value);
    qreal horizontalMaximumValue() const;

    void setVerticalMinimumValue(qreal value);
    qreal verticalMinimumValue() const;

    void setVerticalMaximumValue(qreal value);
    qreal verticalMaximumValue() const;

    void setHorizontalValue(qreal value);
    qreal horizontalValue() const;

    void setVerticalValue(qreal value);
    qreal verticalValue() const;

    void setVerticalDelta(qreal value);
    qreal verticalDelta() const;

    void setHorizontalDelta(qreal value);
    qreal horizontalDelta() const;

    void setScrollSpeed(qreal value);
    qreal scrollSpeed() const;

    bool isActive() const;
    void setActive(bool active);
    bool isInverted() const;

#ifndef QT_NO_WHEELEVENT
    void wheelEvent(QWheelEvent *event) override;
#endif

    bool isAtXEnd() const;
    bool isAtXBeginning() const;
    bool isAtYEnd() const;
    bool isAtYBeginning() const;

Q_SIGNALS:
    void verticalValueChanged();
    void verticalMinimumValueChanged();
    void verticalMaximumValueChanged();
    void horizontalValueChanged();
    void horizontalMinimumValueChanged();
    void horizontalMaximumValueChanged();
    void verticalWheelMoved();
    void horizontalWheelMoved();
    void scrollSpeedChanged();
    void activeChanged();

private:
    qreal m_horizontalMinimumValue;
    qreal m_horizontalMaximumValue;
    qreal m_verticalMinimumValue;
    qreal m_verticalMaximumValue;
    qreal m_horizontalValue;
    qreal m_verticalValue;
    qreal m_verticalDelta;
    qreal m_horizontalDelta;
    qreal m_scrollSpeed;
    bool m_active;
    bool m_inverted;

    Q_DISABLE_COPY(QQuickWheelArea1)
};

QT_END_NAMESPACE

QML_DECLARE_TYPE(QQuickWheelArea1)

#endif // QQUICKWHEELAREA_P_H
