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
#ifndef AU_APPSHELL_NAVIGABLEAPPMENUMODEL_H
#define AU_APPSHELL_NAVIGABLEAPPMENUMODEL_H

#include <optional>

#include <QObject>

#include "appmenumodel.h"

namespace au::appshell {
class NavigableAppMenuModel : public AppMenuModel
{
    Q_OBJECT

    Q_PROPERTY(bool isNavigationStarted READ isNavigationStarted NOTIFY highlightedMenuIdChanged)
    Q_PROPERTY(QString highlightedMenuId READ highlightedMenuId NOTIFY highlightedMenuIdChanged)
    Q_PROPERTY(QString openedMenuId READ openedMenuId WRITE setOpenedMenuId NOTIFY openedMenuIdChanged)

    Q_PROPERTY(QRect appMenuAreaRect READ appMenuAreaRect WRITE setAppMenuAreaRect NOTIFY appMenuAreaRectChanged)
    Q_PROPERTY(QRect openedMenuAreaRect READ openedMenuAreaRect WRITE setOpenedMenuAreaRect NOTIFY openedMenuAreaRectChanged)

    Q_PROPERTY(QWindow * appWindow READ appWindow WRITE setAppWindow)

public:
    explicit NavigableAppMenuModel(QObject* parent = nullptr);

    Q_INVOKABLE void load() override;
    Q_INVOKABLE void handleMenuItem(const QString& itemId) override;
    Q_INVOKABLE void openMenu(const QString& menuId, bool byHover);

    bool isNavigationStarted() const;
    bool isMenuOpened() const;

    QRect appMenuAreaRect() const;
    QRect openedMenuAreaRect() const;

    QWindow* appWindow() const;

public slots:
    void setAppWindow(QWindow* appWindow);
    void setHighlightedMenuId(QString highlightedMenuId);
    void setOpenedMenuId(QString openedMenuId);
    void setAppMenuAreaRect(QRect appMenuAreaRect);
    void setOpenedMenuAreaRect(QRect openedMenuAreaRect);

signals:
    void openMenuRequested(const QString& menuId, bool byHover);
    void closeOpenedMenuRequested();
    void highlightedMenuIdChanged(QString highlightedMenuId);
    void openedMenuIdChanged(QString openedMenuId);
    void appMenuAreaRectChanged(QRect appMenuAreaRect);
    void openedMenuAreaRectChanged(QRect openedMenuAreaRect);

private:
    bool eventFilter(QObject* watched, QEvent* event) override;
    bool processEventForOpenedMenu(QEvent* event);
    bool processEventForAppMenu(QEvent* event);

    bool isNavigateKey(int key) const;
    void navigate(const QSet<int>& activatePossibleKeys);
    void navigateToSubItem(const QString& menuId, const QSet<int>& activatePossibleKeys);

    bool hasItem(const QSet<int>& activatePossibleKeys);
    bool hasSubItem(const QString& menuId, const QSet<int>& activatePossibleKeys);
    void navigate(int scanCode);

    void resetNavigation();
    void navigateToFirstMenu();

    struct MUNavigationSystemState {
        std::string sectionName;
        std::string panelName;
        std::string controlName;
    };

    void saveMUNavigationSystemState();
    void restoreMUNavigationSystemState();

    void activateHighlightedMenu();

    QString highlightedMenuId() const;
    QString openedMenuId() const;

    QString menuItemId(const muse::uicomponents::MenuItemList& items, const QSet<int>& activatePossibleKeys);

    QString m_highlightedMenuId;
    QString m_openedMenuId;

    bool m_needActivateHighlight = true;
    std::optional<MUNavigationSystemState> m_lastActiveMUNavigationState;
    bool m_needActivateLastMUNavigationControl = false;

    QWindow* m_appWindow = nullptr;
    QRect m_appMenuAreaRect;
    QRect m_openedMenuAreaRect;
};
}

#endif // AU_APPSHELL_NAVIGABLEAPPMENUMODEL_H
