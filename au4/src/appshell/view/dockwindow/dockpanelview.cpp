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

#include "dockpanelview.h"

#include "thirdparty/KDDockWidgets/src/DockWidgetQuick.h"
#include "thirdparty/KDDockWidgets/src/private/Frame_p.h"

#include "types/translatablestring.h"

#include "log.h"

#include "ui/uitypes.h"
#include "uicomponents/view/abstractmenumodel.h"

using namespace mu::dock;
using namespace mu::ui;
using namespace mu::uicomponents;
using namespace mu::actions;

static const QString SET_DOCK_OPEN_ACTION_CODE = "dock-set-open";
static const QString TOGGLE_FLOATING_ACTION_CODE = "dock-toggle-floating";

class DockPanelView::DockPanelMenuModel : public uicomponents::AbstractMenuModel
{
public:
    DockPanelMenuModel(DockPanelView* panel)
        : AbstractMenuModel(panel), m_panel(panel)
    {
        listenFloatingChanged();
    }

    void load() override
    {
        TRACEFUNC;

        MenuItemList items;

        if (m_customMenuModel && m_customMenuModel->rowCount() > 0) {
            items << m_customMenuModel->items();
            items << makeSeparator();
        }

        MenuItem* closeDockItem = makeMenuItem(SET_DOCK_OPEN_ACTION_CODE, TranslatableString("appshell/dock", "Close"));
        closeDockItem->setArgs(ActionData::make_arg2<QString, bool>(m_panel->objectName(), false));
        items << closeDockItem;

        MenuItem* toggleFloatingItem = makeMenuItem(TOGGLE_FLOATING_ACTION_CODE, toggleFloatingActionTitle());
        toggleFloatingItem->setArgs(ActionData::make_arg1<QString>(m_panel->objectName()));
        items << toggleFloatingItem;

        setItems(items);
    }

    AbstractMenuModel* customMenuModel() const
    {
        return m_customMenuModel;
    }

    void setCustomMenuModel(AbstractMenuModel* model)
    {
        m_customMenuModel = model;

        if (!model) {
            return;
        }

        connect(model, &AbstractMenuModel::itemsChanged, this, [this]() {
            load();
        });

        connect(model, &AbstractMenuModel::itemChanged, this, [this](MenuItem* item) {
            updateItem(item);
        });
    }

private:
    MenuItem* makeMenuItem(const QString& actionCode, const TranslatableString& title)
    {
        MenuItem* item = new MenuItem(this);
        item->setId(actionCode);

        UiAction action;
        action.code = codeFromQString(actionCode);
        action.title = title;
        item->setAction(action);

        UiActionState state;
        state.enabled = true;
        item->setState(state);

        return item;
    }

    TranslatableString toggleFloatingActionTitle() const
    {
        return m_panel->floating() ? TranslatableString("appshell/dock", "Dock") : TranslatableString("appshell/dock", "Undock");
    }

    void listenFloatingChanged()
    {
        connect(m_panel, &DockPanelView::floatingChanged, this, [this]() {
            int index = itemIndex(TOGGLE_FLOATING_ACTION_CODE);

            if (index == INVALID_ITEM_INDEX) {
                return;
            }

            MenuItem& item = this->item(index);

            UiAction action = item.action();
            action.title = toggleFloatingActionTitle();
            item.setAction(action);
        });
    }

    void updateItem(MenuItem* newItem)
    {
        int index = itemIndex(newItem->id());

        if (index == INVALID_ITEM_INDEX) {
            return;
        }

        setItem(index, newItem);
    }

    AbstractMenuModel* m_customMenuModel = nullptr;
    DockPanelView* m_panel = nullptr;
};

DockPanelView::DockPanelView(QQuickItem* parent)
    : DockBase(DockType::Panel, parent), m_menuModel(new DockPanelMenuModel(this))
{
    setLocation(Location::Left);
}

DockPanelView::~DockPanelView()
{
    KDDockWidgets::DockWidgetQuick* dockWidget = this->dockWidget();
    IF_ASSERT_FAILED(dockWidget) {
        return;
    }

    dockWidget->setProperty(DOCK_PANEL_PROPERTY, QVariant::fromValue(nullptr));
    dockWidget->setProperty(CONTEXT_MENU_MODEL_PROPERTY, QVariant::fromValue(nullptr));
}

QString DockPanelView::groupName() const
{
    return m_groupName;
}

void DockPanelView::setGroupName(const QString& name)
{
    if (m_groupName == name) {
        return;
    }

    m_groupName = name;
    emit groupNameChanged();
}

void DockPanelView::componentComplete()
{
    DockBase::componentComplete();

    KDDockWidgets::DockWidgetQuick* dockWidget = this->dockWidget();
    IF_ASSERT_FAILED(dockWidget) {
        return;
    }

    m_menuModel->load();

    dockWidget->setProperty(DOCK_PANEL_PROPERTY, QVariant::fromValue(this));
    dockWidget->setProperty(CONTEXT_MENU_MODEL_PROPERTY, QVariant::fromValue(m_menuModel));

    connect(m_menuModel, &AbstractMenuModel::itemsChanged, [dockWidget, this]() {
        if (dockWidget) {
            dockWidget->setProperty(CONTEXT_MENU_MODEL_PROPERTY, QVariant::fromValue(m_menuModel));
        }
    });
}

QObject* DockPanelView::navigationSection() const
{
    return m_navigationSection;
}

void DockPanelView::setNavigationSection(QObject* newNavigation)
{
    if (m_navigationSection == newNavigation) {
        return;
    }

    m_navigationSection = newNavigation;
    emit navigationSectionChanged();
}

AbstractMenuModel* DockPanelView::contextMenuModel() const
{
    return m_menuModel->customMenuModel();
}

void DockPanelView::setContextMenuModel(AbstractMenuModel* model)
{
    if (m_menuModel->customMenuModel() == model) {
        return;
    }

    m_menuModel->setCustomMenuModel(model);
    m_menuModel->load();
    emit contextMenuModelChanged();
}

bool DockPanelView::isTabAllowed(const DockPanelView* tab) const
{
    IF_ASSERT_FAILED(tab) {
        return false;
    }

    if (tab == this) {
        return false;
    }

    if (!isOpen()) {
        return false;
    }

    if (floating()) {
        return false;
    }

    if (m_groupName.isEmpty() || tab->m_groupName.isEmpty()) {
        return false;
    }

    return m_groupName == tab->m_groupName;
}

void DockPanelView::addPanelAsTab(DockPanelView* tab)
{
    IF_ASSERT_FAILED(tab && dockWidget()) {
        return;
    }

    if (!isTabAllowed(tab)) {
        return;
    }

    dockWidget()->addDockWidgetAsTab(tab->dockWidget());
    tab->setVisible(true);
}

void DockPanelView::setCurrentTabIndex(int index)
{
    IF_ASSERT_FAILED(dockWidget()) {
        return;
    }

    KDDockWidgets::Frame* frame = dockWidget()->frame();
    if (frame) {
        frame->setCurrentTabIndex(index);
    }
}
