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
#include "macosmainwindowbridge.h"

#include <Cocoa/Cocoa.h>
#include <QWindow>

using namespace mu::ui;

static NSWindow* nsWindowForQWindow(QWindow* qWindow)
{
    if (!qWindow) {
        return nullptr;
    }

    NSView* nsView = (__bridge NSView*)reinterpret_cast<void*>(qWindow->winId());
    NSWindow* nsWindow = [nsView window];
    return nsWindow;
}

MacOSMainWindowBridge::MacOSMainWindowBridge(QObject* parent)
    : MainWindowBridge(parent)
{
}

void MacOSMainWindowBridge::init()
{
    MainWindowBridge::init();

    uiConfiguration()->applyPlatformStyle(m_window);

    uiConfiguration()->currentThemeChanged().onNotify(this, [this]() {
        uiConfiguration()->applyPlatformStyle(m_window);
    });
}

bool MacOSMainWindowBridge::fileModified() const
{
    //! NOTE QWindow misses an API for this, so we'll do it ourselves.
    NSWindow* nsWindow = nsWindowForQWindow(m_window);
    if (!nsWindow) {
        return false;
    }

    return [nsWindow isDocumentEdited];
}

void MacOSMainWindowBridge::setFileModified(bool modified)
{
    NSWindow* nsWindow = nsWindowForQWindow(m_window);
    if (!nsWindow) {
        return;
    }

    if ([nsWindow isDocumentEdited] == modified) {
        return;
    }

    [nsWindow setDocumentEdited:modified];
    emit fileModifiedChanged();
}
