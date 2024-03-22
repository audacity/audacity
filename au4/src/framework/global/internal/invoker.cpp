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
#include "invoker.h"

#include <QMetaObject>
#include <QApplication>

#include "log.h"

using namespace mu;

std::thread::id Invoker::m_mainThreadId;

void Invoker::setup()
{
    m_mainThreadId = std::this_thread::get_id();
}

void Invoker::invoke(const Call& func, bool isAlwaysQueued)
{
    IF_ASSERT_FAILED(func) {
        return;
    }

    if (!isAlwaysQueued && std::this_thread::get_id() == m_mainThreadId) {
        func();
    } else {
        static const char* name = "doInvoke";

        Functor* f = new Functor(func);
        void* ptr = reinterpret_cast<void*>(f);
        QMetaObject::invokeMethod(this, name, Qt::QueuedConnection, Q_ARG(void*, ptr));
    }
}

void Invoker::invokeQueuedCalls()
{
    qApp->sendPostedEvents(this, QEvent::MetaCall);
}

void Invoker::doInvoke(void* ptr)
{
    Functor* f = reinterpret_cast<Functor*>(ptr);
    f->call();
    delete f;
}
