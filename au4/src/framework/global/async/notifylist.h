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
#ifndef MU_ASYNC_NOTIFYLIST_H
#define MU_ASYNC_NOTIFYLIST_H

#include <vector>
#include <cassert>

#include "../thirdparty/kors_async/async/changednotify.h"

#include "asyncable.h"

namespace mu::async {
template<typename T>
using ChangedNotifier = kors::async::ChangedNotifier<T>;

template<typename T>
using ChangedNotify = kors::async::ChangedNotify<T>;

template<typename T>
class NotifyList : public std::vector<T>
{
public:
    NotifyList() {}
    NotifyList(const NotifyList&) = default;
    NotifyList(std::shared_ptr<ChangedNotify<T> > n)
        : m_notify(n) {}
    NotifyList(const std::vector<T>& l, std::shared_ptr<ChangedNotify<T> > n)
        : std::vector<T>(l), m_notify(n) {}

    void setNotify(std::shared_ptr<ChangedNotify<T> > n)
    {
        m_notify = n;
    }

    NotifyList<T>& operator =(const NotifyList<T>& nl)
    {
        std::vector<T>::operator=(nl);
        m_notify = nl.m_notify;
        return *this;
    }

    template<typename Call>
    void onChanged(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        assert(m_notify);
        if (!m_notify) {
            return;
        }
        m_notify->onChanged(caller, f, mode);
    }

    void resetOnChanged(Asyncable* caller)
    {
        assert(m_notify);
        if (!m_notify) {
            return;
        }
        m_notify->resetOnChanged(caller);
    }

    template<typename Call>
    void onItemChanged(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        assert(m_notify);
        if (!m_notify) {
            return;
        }
        m_notify->onItemChanged(caller, f, mode);
    }

    void resetOnItemChanged(Asyncable* caller)
    {
        assert(m_notify);
        if (!m_notify) {
            return;
        }
        m_notify->resetOnItemChanged(caller);
    }

    template<typename Call>
    void onItemAdded(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        assert(m_notify);
        if (!m_notify) {
            return;
        }
        m_notify->onItemAdded(caller, f, mode);
    }

    void resetOnItemAdded(Asyncable* caller)
    {
        assert(m_notify);
        if (!m_notify) {
            return;
        }
        m_notify->resetOnItemAdded(caller);
    }

    template<typename Call>
    void onItemRemoved(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        assert(m_notify);
        if (!m_notify) {
            return;
        }
        m_notify->onItemRemoved(caller, f, mode);
    }

    void resetOnItemRemoved(Asyncable* caller)
    {
        m_notify->resetOnItemRemoved(caller);
    }

    template<typename Call>
    void onItemReplaced(Asyncable* caller, Call f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        assert(m_notify);
        if (!m_notify) {
            return;
        }
        m_notify->onItemReplaced(caller, f, mode);
    }

    void resetOnItemReplaced(Asyncable* caller)
    {
        m_notify->resetOnItemReplaced(caller);
    }

private:
    std::shared_ptr<ChangedNotify<T> > m_notify = nullptr;
};
}

#endif // MU_ASYNC_NOTIFYLIST_H
