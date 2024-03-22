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

#include "topleveldialog.h"

#include <QApplication>
#include <QKeyEvent>
#include <QWindow>

using namespace mu::uicomponents;

TopLevelDialog::TopLevelDialog(QWidget* parent)
    : QDialog(parent)
{
    setWindowFlag(Qt::WindowContextHelpButtonHint, false);

    // We want some windows to be on top of the main window.
    // But not on top of all other applications when MuseScore isn't active.
    // On Windows, we achieve this by setting the transient parent.
    // On macOS, we have to use a workaround:
    // When the application becomes active, the windows will get the StayOnTop hint.
    // and when the application becomes inactive, the hint will be removed.
#ifdef Q_OS_MAC
    auto updateStayOnTopHint = [this]() {
        bool stay = qApp->applicationState() == Qt::ApplicationActive;

        bool wasShown = isVisible();
        bool wasActive = isActiveWindow();

        setWindowFlag(Qt::WindowStaysOnTopHint, stay);
        if (wasShown) {
            if (!wasActive) {
                setAttribute(Qt::WA_ShowWithoutActivating, true);
            }
            show();
            setAttribute(Qt::WA_ShowWithoutActivating, false);
        }
    };
    updateStayOnTopHint();
    connect(qApp, &QApplication::applicationStateChanged, this, updateStayOnTopHint);
#endif
}

#ifdef MU_QT5_COMPAT
TopLevelDialog::TopLevelDialog(const TopLevelDialog& dialog)
    : QDialog(dialog.parentWidget())
{
}

#endif

bool TopLevelDialog::event(QEvent* e)
{
#ifndef Q_OS_MAC
    if (e->type() == QEvent::Show) {
        windowHandle()->setTransientParent(mainWindow()->qWindow());
    }
#endif

    if (e->type() == QEvent::ShortcutOverride) {
        if (QKeyEvent* keyEvent = dynamic_cast<QKeyEvent*>(e)) {
            if (keyEvent->key() == Qt::Key_Escape && keyEvent->modifiers() == Qt::NoModifier) {
                close();
                return true;
            }
        }
    }

    return QDialog::event(e);
}
