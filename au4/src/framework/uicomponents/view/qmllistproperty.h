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

#ifndef MU_UICOMPONENTS_QMLLISTPROPERTY_H
#define MU_UICOMPONENTS_QMLLISTPROPERTY_H

#include <QObject>
#include <QQmlListProperty>

namespace mu::uicomponents {
class QmlListPropertyNotifier : public QObject
{
    Q_OBJECT
public:

    QmlListPropertyNotifier();

    void fireAppended(int index);
    void fireCleared();

signals:
    void appended(int index);
    void cleared();
};

template<typename T>
class QmlListProperty
{
public:

    explicit QmlListProperty(QObject* parent)
        : _parent(parent), _notifier(new QmlListPropertyNotifier())
    {}
    ~QmlListProperty() { delete _notifier; }

#ifdef MU_QT5_COMPAT
    static int s_count_qt5(QQmlListProperty<T>* list)
    {
        return static_cast<int>(QmlListProperty::s_count(list));
    }

    static T* s_at_qt5(QQmlListProperty<T>* list, int index)
    {
        return QmlListProperty::s_at(list, static_cast<int>(index));
    }

#endif

    QQmlListProperty<T> property()
    {
        return QQmlListProperty<T>(_parent,
                                   this,
                                   &QmlListProperty::s_append,
#ifdef MU_QT5_COMPAT
                                   &s_count_qt5,
                                   &s_at_qt5,
#else
                                   &QmlListProperty::s_count,
                                   &QmlListProperty::s_at,
#endif
                                   &QmlListProperty::s_clear);
    }

    QList<T*> list() const
    {
        return _list;
    }

    QmlListPropertyNotifier* notifier() const
    {
        return _notifier;
    }

    void append(T* t)
    {
        _list.append(t);
        _notifier->fireAppended(_list.count() - 1);
    }

    int count() const
    {
        return _list.count();
    }

    T* at(int index) const
    {
        return _list.at(index);
    }

    void clear()
    {
        _list.clear();
        _notifier->fireCleared();
    }

private:

    static QmlListProperty* get(QQmlListProperty<T>* list)
    {
        return reinterpret_cast< QmlListProperty* >(list->data);
    }

    static void s_append(QQmlListProperty<T>* list, T* t)
    {
        get(list)->append(t);
    }

    static qsizetype s_count(QQmlListProperty<T>* list)
    {
        return get(list)->count();
    }

    static T* s_at(QQmlListProperty<T>* list, qsizetype index)
    {
        return get(list)->at(static_cast<int>(index));
    }

    static void s_clear(QQmlListProperty<T>* list)
    {
        get(list)->clear();
    }

    QObject* _parent = nullptr;
    QList<T*> _list;
    QmlListPropertyNotifier* _notifier = nullptr;
};
}

#endif // MU_UICOMPONENTS_QMLLISTPROPERTY_H
