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

#ifndef MU_DOCK_MAINWINDOW_H
#define MU_DOCK_MAINWINDOW_H

#include <QObject>
#include <QWindow>

#include "framework/ui/imainwindow.h"

#include "async/asyncable.h"
#include "async/notification.h"

namespace mu::ui {
class MainWindow : public IMainWindow, public async::Asyncable
{
public:
    MainWindow() = default;

    void init(MainWindowBridge* bridge) override;
    void deinit() override;

    QWindow* qWindow() const override;

    void requestShowOnBack() override;
    void requestShowOnFront() override;

    bool isFullScreen() const override;
    async::Notification isFullScreenChanged() const override;
    void toggleFullScreen() override;

    QScreen* screen() const override;

private:
    MainWindowBridge* m_bridge = nullptr;

    async::Notification m_isFullScreenChanged;
};
}

#endif // MU_DOCK_MAINWINDOW_H
