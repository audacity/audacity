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
#ifndef MU_UICOMPONENTS_POPUPWINDOW_QQUICKVIEW_H
#define MU_UICOMPONENTS_POPUPWINDOW_QQUICKVIEW_H

#include <QObject>
#include <QQuickView>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "ui/iinteractiveprovider.h"
#include "ui/imainwindow.h"
#include "ui/iuiconfiguration.h"

#include "ipopupwindow.h"

namespace mu::uicomponents {
class PopupWindow_QQuickView : public IPopupWindow, public async::Asyncable
{
    Q_OBJECT

    INJECT(ui::IInteractiveProvider, interactiveProvider)
    INJECT(ui::IMainWindow, mainWindow)
    INJECT(ui::IUiConfiguration, uiConfiguration)

public:
    explicit PopupWindow_QQuickView(QObject* parent = nullptr);
    ~PopupWindow_QQuickView();

    void init(QQmlEngine* engine, bool isDialogMode, bool isFrameless) override;

    void setContent(QQmlComponent* component, QQuickItem* item) override;

    void show(QScreen* screen, QRect geometry, bool activateFocus) override;
    void close() override;
    void raise() override;
    void setPosition(QPoint position) override;

    QWindow* qWindow() const override;
    bool isVisible() const override;
    QRect geometry() const override;

    QWindow* parentWindow() const override;
    void setParentWindow(QWindow* window) override;

    bool resizable() const override;
    void setResizable(bool resizable) override;

    void setPosition(const QPoint& position) const override;

    void forceActiveFocus() override;

    void setOnHidden(const std::function<void()>& callback) override;

private:
    bool eventFilter(QObject* watched, QEvent* event) override;

    void updateSize(const QSize& newSize);

    QQuickView* m_view = nullptr;
    bool m_resizable = false;
    std::function<void()> m_onHidden;
    bool m_activeFocusOnParentOnClose = false;

    QWindow* m_parentWindow = nullptr;
};
}
#endif // POPUPWINDOW_QQUICKVIEW_H
