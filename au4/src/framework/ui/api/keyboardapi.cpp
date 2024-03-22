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
#include "keyboardapi.h"

#include <QGuiApplication>
#include <QKeyEvent>
#include <QWindow>
#include <QKeySequence>

#include "log.h"

using namespace mu::api;

KeyboardApi::KeyboardApi(IApiEngine* e)
    : ApiObject(e)
{
}

void KeyboardApi::key(const QString& key)
{
    LOGD() << key;
#ifdef MU_QT5_COMPAT
    int code = QKeySequence::fromString(key.toUpper())[0];
#else
    int code = QKeySequence::fromString(key.toUpper())[0].toCombined();
#endif

    QWindow* w = qApp->focusWindow();
    if (!w) {
        w = mainWindow()->qWindow();
    }

    QKeyEvent pressEvent(QEvent::KeyPress, code, Qt::NoModifier, key);
    qApp->sendEvent(w, &pressEvent);

    QKeyEvent* releaseEvent = new QKeyEvent(QEvent::KeyRelease, code, Qt::NoModifier, key);
    qApp->postEvent(w, releaseEvent);
}

void KeyboardApi::repeatKey(const QString& k, int count)
{
    for (int i = 0; i < count; ++i) {
        key(k);
    }
}

void KeyboardApi::text(const QString& text)
{
    for (const QChar& ch : text) {
        key(ch);
    }
}
