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

#ifndef MU_UICOMPONENTS_POPUPVIEWCLOSECONTROLLER_H
#define MU_UICOMPONENTS_POPUPVIEWCLOSECONTROLLER_H

#include <QObject>
#include <QQuickItem>

#include "async/notification.h"
#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "ui/imainwindow.h"
#include "ui/iinteractiveprovider.h"

namespace mu::uicomponents {
class PopupViewCloseController : public QObject, public async::Asyncable
{
    Q_OBJECT

    INJECT(ui::IMainWindow, mainWindow)
    INJECT(ui::IInteractiveProvider, interactiveProvider)

public:
    explicit PopupViewCloseController(QObject* parent = nullptr);
    ~PopupViewCloseController() override = default;

    void init();

    bool active() const;
    void setActive(bool active);

    QQuickItem* parentItem() const;
    void setParentItem(QQuickItem* parentItem);

    QWindow* popupWindow() const;
    void setWindow(QWindow* window);

    bool popupHasFocus() const;
    void setPopupHasFocus(bool hasFocus);

    void setIsCloseOnPressOutsideParent(bool close);

    async::Notification closeNotification() const;

private slots:
    void onApplicationStateChanged(Qt::ApplicationState state);

protected:
    bool eventFilter(QObject* watched, QEvent* event) override;

    void doFocusOut();
    virtual void doUpdateEventFilters();

    bool isMouseWithinBoundaries(const QPoint& mousePos) const;

    void notifyAboutClose();

private:
    bool m_active = false;

    QQuickItem* m_parentItem = nullptr;
    QWindow* m_popupWindow = nullptr;

    bool m_popupHasFocus = true;
    bool m_isCloseOnPressOutsideParent = false;

    async::Notification m_closeNotification;
};
}

#endif // MU_UICOMPONENTS_POPUPVIEWCLOSECONTROLLER_H
