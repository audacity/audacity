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

#ifndef MU_DOCK_DOCKPANELVIEW_H
#define MU_DOCK_DOCKPANELVIEW_H

#include "internal/dockbase.h"

#include "framework/uicomponents/view/qmllistproperty.h"

#include "uicomponents/view/abstractmenumodel.h"

namespace mu::uicomponents {
class AbstractMenuModel;
}

namespace mu::dock {
class DockPanelView : public DockBase
{
    Q_OBJECT

    Q_PROPERTY(QString groupName READ groupName WRITE setGroupName NOTIFY groupNameChanged)
    Q_PROPERTY(QObject * navigationSection READ navigationSection WRITE setNavigationSection NOTIFY navigationSectionChanged)
    Q_PROPERTY(
        mu::uicomponents::AbstractMenuModel
        * contextMenuModel READ contextMenuModel WRITE setContextMenuModel NOTIFY contextMenuModelChanged)

public:
    explicit DockPanelView(QQuickItem* parent = nullptr);
    ~DockPanelView() override;

    QString groupName() const;
    QObject* navigationSection() const;
    uicomponents::AbstractMenuModel* contextMenuModel() const;

    bool isTabAllowed(const DockPanelView* tab) const;
    void addPanelAsTab(DockPanelView* tab);
    void setCurrentTabIndex(int index);

public slots:
    void setGroupName(const QString& name);
    void setNavigationSection(QObject* newNavigation);
    void setContextMenuModel(uicomponents::AbstractMenuModel* model);

signals:
    void groupNameChanged();
    void navigationSectionChanged();
    void contextMenuModelChanged();

private:
    void componentComplete() override;

    QString m_groupName;
    QObject* m_navigationSection = nullptr;

    class DockPanelMenuModel;
    DockPanelMenuModel* m_menuModel = nullptr;
};
}

#endif // MU_DOCK_DOCKPANELVIEW_H
