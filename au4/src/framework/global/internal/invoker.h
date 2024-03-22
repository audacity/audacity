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
#ifndef MU_GLOBAL_INVOKER_H
#define MU_GLOBAL_INVOKER_H

#include <QObject>
#include <thread>
#include <functional>

namespace mu {
class Invoker : public QObject
{
    Q_OBJECT
public:
    Invoker() = default;

    using Call = std::function<void ()>;

    static void setup();

    void invoke(const Call& func = nullptr, bool isAlwaysQueued = false);
    void invokeQueuedCalls();

public slots:
    void doInvoke(void* ptr);

private:

    struct Functor {
        Call call;
        Functor(const Call& c)
            : call(c) {}
    };

    static std::thread::id m_mainThreadId;
};
}

#endif // MU_GLOBAL_INVOKER_H
