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
#ifndef MU_UI_ABSTRACTNAVIGATION_H
#define MU_UI_ABSTRACTNAVIGATION_H

#include <QObject>
#include <QQmlParserStatus>

#include "async/asyncable.h"
#include "../inavigation.h"
#include "qmlaccessible.h"
#include "navigationevent.h"

#include "modularity/ioc.h"
#include "../inavigationcontroller.h"

namespace mu::ui {
class AbstractNavigation : public QObject, public QQmlParserStatus, public async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QString name READ name WRITE setName NOTIFY nameChanged)

    // see INavigation::Index
    Q_PROPERTY(int order READ order WRITE setOrder NOTIFY orderChanged)
    Q_PROPERTY(int column READ column WRITE setColumn NOTIFY columnChanged)
    Q_PROPERTY(int row READ row WRITE setRow NOTIFY rowChanged)

    Q_PROPERTY(bool enabled READ enabled WRITE setEnabled NOTIFY enabledChanged)
    Q_PROPERTY(bool active READ active NOTIFY activeChanged)
    Q_PROPERTY(bool highlight READ highlight NOTIFY highlightChanged)

    Q_PROPERTY(AccessibleItem * accessible READ accessible WRITE setAccessible NOTIFY accessibleChanged)

    Q_INTERFACES(QQmlParserStatus)

    INJECT(INavigationController, navigationController)

public:
    explicit AbstractNavigation(QObject* parent = nullptr);

    int order() const;
    int column() const;
    int row() const;

    QString name() const;

    const INavigation::Index& index() const;
    void setIndex(const INavigation::Index& index);
    async::Channel<INavigation::Index> indexChanged() const;

    bool enabled() const;
    async::Channel<bool> enabledChanged() const;

    bool active() const;
    async::Channel<bool> activeChanged() const;

    bool highlight() const;

    AccessibleItem* accessible() const;
    void setAccessibleParent(AccessibleItem* p);

    void onEvent(INavigation::EventPtr e);

    QWindow* window() const;

    // QQmlParserStatus
    void classBegin() override;
    void componentComplete() override;

public slots:
    void setName(QString name);
    void setOrder(int order);
    void setColumn(int column);
    void setRow(int row);
    void setEnabled(bool enabled);
    void setActive(bool active);
    void setAccessible(AccessibleItem* accessible);

signals:
    void nameChanged(QString name);
    void orderChanged(int order);
    void columnChanged(int column);
    void rowChanged(int row);
    void enabledChanged(bool enabled);
    void activeChanged(bool active);
    void highlightChanged();
    void accessibleChanged();

    void navigationEvent(QVariant event);

protected:

    QString m_name;
    INavigation::Index m_index;
    async::Channel<INavigation::Index> m_indexChanged;

    bool m_enabled = true;
    async::Channel<bool> m_enabledChanged;

    bool m_active = false;
    async::Channel<bool> m_activeChanged;

    NavigationEvent* m_event = nullptr;

    mutable AccessibleItem* m_accessible = nullptr;
};
}

#endif // MU_UI_ABSTRACTNAVIGATION_H
