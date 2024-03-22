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

#ifndef MU_UICOMPONENTS_TOPLEVELDIALOG_H
#define MU_UICOMPONENTS_TOPLEVELDIALOG_H

#include <QDialog>

#include "modularity/ioc.h"
#include "ui/imainwindow.h"

namespace mu::uicomponents {
class TopLevelDialog : public QDialog
{
    INJECT(ui::IMainWindow, mainWindow)

public:
    explicit TopLevelDialog(QWidget* parent = nullptr);
#ifdef MU_QT5_COMPAT
    TopLevelDialog(const TopLevelDialog& dialog);
#endif

protected:
    bool event(QEvent* e) override;
};
}

#endif // MU_UICOMPONENTS_TOPLEVELDIALOG_H
