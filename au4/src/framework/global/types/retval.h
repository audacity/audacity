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
#ifndef MU_GLOBAL_RETVAL_H
#define MU_GLOBAL_RETVAL_H

#include "types/ret.h"
#include "async/channel.h"
#include "async/notification.h"

namespace mu {
template<typename T>
struct RetVal {
    Ret ret;
    T val;

    RetVal() = default;
    RetVal(const Ret& r)
        : ret(r) {}

    static RetVal<T> make_ok(const T& v)
    {
        RetVal<T> rv;
        rv.ret = make_ret(Ret::Code::Ok);
        rv.val = v;
        return rv;
    }
};

template<typename T1, typename T2>
struct RetVal2 {
    Ret ret;
    T1 val1;
    T2 val2;
};

template<typename T>
struct RetValCh {
    Ret ret;
    T val;
    async::Channel<T> ch;
    RetValCh() = default;
    RetValCh(const Ret& r)
        : ret(r) {}
};

template<typename T>
struct RetCh {
    Ret ret;
    async::Channel<T> ch;
};

template<typename T>
struct ValCh {
    T val = T();
    async::Channel<T> ch;

    void set(const T& v) { val = v; ch.send(v); }
};

template<typename T>
struct ValNt {
    T val = T();
    async::Notification notification;

    void set(const T& v) { val = v; notification.notify(); }
};
}

#endif // MU_GLOBAL_RETVAL_H
