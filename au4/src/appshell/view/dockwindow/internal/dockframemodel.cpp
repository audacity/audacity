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

#include "dockframemodel.h"

#include <QQuickItem>
#include <QApplication>

#include "thirdparty/KDDockWidgets/src/private/Frame_p.h"

#include "../docktypes.h"
#include "../dockpanelview.h"

#include "log.h"

#include "uicomponents/view/abstractmenumodel.h"

using namespace mu::dock;
using namespace mu::actions;
using namespace mu::uicomponents;

DockFrameModel::DockFrameModel(QObject* parent)
    : QObject(parent)
{
    qApp->installEventFilter(this);
}

bool DockFrameModel::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() != QEvent::DynamicPropertyChange) {
        return QObject::eventFilter(watched, event);
    }

    auto propertyChangeEvent = dynamic_cast<QDynamicPropertyChangeEvent*>(event);
    if (!propertyChangeEvent) {
        return QObject::eventFilter(watched, event);
    }

    if (propertyChangeEvent->propertyName() == CONTEXT_MENU_MODEL_PROPERTY) {
        emit tabsChanged();

        if (watched == currentDockWidget()) {
            emit currentDockChanged();
        }
    }

    if (propertyChangeEvent->propertyName() == "highlightingRect") {
        emit highlightingVisibleChanged();
    }

    return QObject::eventFilter(watched, event);
}

QQuickItem* DockFrameModel::frame() const
{
    return m_frame;
}

QVariantList DockFrameModel::tabs() const
{
    QVariantList result;

    if (!m_frame || m_frame->hasSingleDockWidget()) {
        return result;
    }

    for (const KDDockWidgets::DockWidgetBase* dock : m_frame->dockWidgets()) {
        QVariantMap tab;
        tab["title"] = dock->title();
        tab[CONTEXT_MENU_MODEL_PROPERTY] = dock->property(CONTEXT_MENU_MODEL_PROPERTY);

        result << tab;
    }

    return result;
}

bool DockFrameModel::titleBarVisible() const
{
    return m_titleBarVisible;
}

bool DockFrameModel::isHorizontalPanel() const
{
    return m_isHorizontalPanel;
}

void DockFrameModel::setFrame(QQuickItem* frame)
{
    if (frame == m_frame) {
        return;
    }

    m_frame = dynamic_cast<KDDockWidgets::Frame*>(frame);
    emit frameChanged(frame);

    listenChangesInFrame();
}

void DockFrameModel::listenChangesInFrame()
{
    if (!m_frame) {
        return;
    }

    connect(m_frame, &KDDockWidgets::Frame::numDockWidgetsChanged, this, [this]() {
        emit tabsChanged();

        if (!currentDockWidget()) {
            m_frame->setCurrentTabIndex(0);
        }

        auto allDocks = m_frame->dockWidgets();
        if (allDocks.isEmpty()) {
            setTitleBarVisible(false);
            return;
        }

        DockProperties properties = readPropertiesFromObject(allDocks.first());
        bool isHorizontalPanel = (properties.type == DockType::Panel)
                                 && (properties.location == Location::Top || properties.location == Location::Bottom);
        setIsHorizontalPanel(isHorizontalPanel);

        bool visible = (allDocks.size() == 1) && (properties.type == DockType::Panel) && (properties.floatable || properties.closable);
        setTitleBarVisible(visible);

        updateNavigationSection();
    });

    connect(m_frame, &KDDockWidgets::Frame::currentDockWidgetChanged, this, [this]() {
        updateNavigationSection();

        emit currentDockChanged();
    });
}

void DockFrameModel::setTitleBarVisible(bool visible)
{
    if (visible == m_titleBarVisible) {
        return;
    }

    m_titleBarVisible = visible;
    emit titleBarVisibleChanged(visible);
}

void DockFrameModel::setIsHorizontalPanel(bool is)
{
    if (is == m_isHorizontalPanel) {
        return;
    }

    m_isHorizontalPanel = is;
    emit isHorizontalPanelChanged();
}

QObject* DockFrameModel::currentNavigationSection() const
{
    auto dockPanel = currentDockProperty(DOCK_PANEL_PROPERTY).value<DockPanelView*>();
    return dockPanel ? dockPanel->navigationSection() : nullptr;
}

void DockFrameModel::updateNavigationSection()
{
    QObject* n = currentNavigationSection();
    if (m_navigationSection != n) {
        m_navigationSection = n;
        emit navigationSectionChanged();
    }
}

QObject* DockFrameModel::navigationSection() const
{
    return m_navigationSection;
}

QString DockFrameModel::currentDockUniqueName() const
{
    auto dock = currentDockWidget();
    return dock ? dock->uniqueName() : QString();
}

QVariant DockFrameModel::currentDockContextMenuModel() const
{
    return currentDockProperty(CONTEXT_MENU_MODEL_PROPERTY);
}

bool DockFrameModel::highlightingVisible() const
{
    return highlightingRect().isValid();
}

QRect DockFrameModel::highlightingRect() const
{
    if (!m_frame) {
        return QRect();
    }

    for (auto dock : m_frame->dockWidgets()) {
        DockProperties properties = readPropertiesFromObject(dock);

        if (properties.highlightingRect.isValid()) {
            return properties.highlightingRect;
        }
    }

    return QRect();
}

KDDockWidgets::DockWidgetBase* DockFrameModel::currentDockWidget() const
{
    return m_frame && !m_frame->isEmpty() ? m_frame->currentDockWidget() : nullptr;
}

QVariant DockFrameModel::currentDockProperty(const char* propertyName) const
{
    const QObject* dock = currentDockWidget();
    return dock ? dock->property(propertyName) : QVariant();
}

void DockFrameModel::handleMenuItem(const QString& itemId) const
{
    auto menuModel = currentDockContextMenuModel().value<AbstractMenuModel*>();

    if (menuModel) {
        menuModel->handleMenuItem(itemId);
    }
}
