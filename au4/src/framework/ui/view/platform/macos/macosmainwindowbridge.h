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
#ifndef MU_APPSHELL_MACOS_MAINWINDOWBRIDGE_H
#define MU_APPSHELL_MACOS_MAINWINDOWBRIDGE_H

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

#include "ui/view/mainwindowbridge.h"

namespace mu::ui {
class MacOSMainWindowBridge : public MainWindowBridge, public async::Asyncable
{
    Q_OBJECT

    INJECT(IUiConfiguration, uiConfiguration)

public:
    explicit MacOSMainWindowBridge(QObject* parent = nullptr);

    bool fileModified() const override;

public slots:
    void setFileModified(bool modified) override;

private:
    void init() override;
};
}

#endif // MU_APPSHELL_MACOS_MAINWINDOWBRIDGE_H
