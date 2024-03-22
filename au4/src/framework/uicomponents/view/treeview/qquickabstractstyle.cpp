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

#include "qquickabstractstyle_p.h"

QT_BEGIN_NAMESPACE

/*!
    \qmltype AbstractStyle
    \instantiates QQuickAbstractStyle1
    \inqmlmodule QtQuick.Controls.Styles
    \qmlabstract
    \internal
*/

/*!
    \qmlpropertygroup QtQuick.Controls.Styles::AbstractStyle::padding
    \qmlproperty int AbstractStyle::padding.top
    \qmlproperty int AbstractStyle::padding.left
    \qmlproperty int AbstractStyle::padding.right
    \qmlproperty int AbstractStyle::padding.bottom

    This grouped property holds the \c top, \c left, \c right and \c bottom padding.
*/

QQuickAbstractStyle1::QQuickAbstractStyle1(QObject *parent) : QObject(parent)
{
}

QQmlListProperty<QObject> QQuickAbstractStyle1::data()
{
    return QQmlListProperty<QObject>(this, 0, &QQuickAbstractStyle1::data_append, &QQuickAbstractStyle1::data_count,
                                     &QQuickAbstractStyle1::data_at, &QQuickAbstractStyle1::data_clear);
}

void QQuickAbstractStyle1::data_append(QQmlListProperty<QObject> *list, QObject *object)
{
    if (QQuickAbstractStyle1 *style = qobject_cast<QQuickAbstractStyle1 *>(list->object))
        style->m_data.append(object);
}

#ifdef MU_QT5_COMPAT
int QQuickAbstractStyle1::data_count(QQmlListProperty<QObject> *list)
{
    if (QQuickAbstractStyle1 *style = qobject_cast<QQuickAbstractStyle1 *>(list->object))
        return style->m_data.count();
    return 0;
}

QObject *QQuickAbstractStyle1::data_at(QQmlListProperty<QObject> *list, int index)
{
    if (QQuickAbstractStyle1 *style = qobject_cast<QQuickAbstractStyle1 *>(list->object))
        return style->m_data.at(index);
    return 0;
}

#else
qsizetype QQuickAbstractStyle1::data_count(QQmlListProperty<QObject> *list)
{
    if (QQuickAbstractStyle1 *style = qobject_cast<QQuickAbstractStyle1 *>(list->object))
        return style->m_data.count();
    return 0;
}

QObject *QQuickAbstractStyle1::data_at(QQmlListProperty<QObject> *list, qsizetype index)
{
    if (QQuickAbstractStyle1 *style = qobject_cast<QQuickAbstractStyle1 *>(list->object))
        return style->m_data.at(index);
    return 0;
}

#endif

void QQuickAbstractStyle1::data_clear(QQmlListProperty<QObject> *list)
{
    if (QQuickAbstractStyle1 *style = qobject_cast<QQuickAbstractStyle1 *>(list->object))
        style->m_data.clear();
}

QT_END_NAMESPACE
