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
#ifndef MU_GLOBAL_TIMER_H
#define MU_GLOBAL_TIMER_H

#include "async/notification.h"
#include "async/asyncable.h"
#include <chrono>
#include <memory>
#include <thread>

namespace mu {
/*!
 * mu::Timer
 * usage:
 *      auto timer = new Timer(std::chrono::microseconds(20));
 *      timer.onTimeout(this, []() { LOGI() << "Timer call";});
 *      timer.run();
 */
class Timer
{
public:
    using time_t = std::chrono::microseconds;

    explicit Timer(time_t interval)
        : m_interval(interval), m_active(false) {}

    ~Timer() { stop(); }

    //! start timer's thread
    void start()
    {
        if (!isActive()) {
            m_active = true;
            m_thread.reset(std::move(new std::thread([this]() {
                timerLoop();
            })));
            m_thread->detach();
            m_started = std::chrono::steady_clock::now();
        }
    }

    //! stop and kill the timer's thread
    void stop()
    {
        m_active = false;
        if (m_thread) {
            m_thread.release();
        }
    }

    //! return current timer's status
    bool isActive() const
    {
        return m_active;
    }

    //! add notification on timer's interval
    template<typename Func>
    void onTimeout(const async::Asyncable* receiver, Func function)
    {
        m_notification.onNotify(receiver, function);
    }

    //! return total seconds since start
    float secondsSinceStart()
    {
        std::chrono::duration<float, std::ratio<1> > diff(std::chrono::steady_clock::now() - m_started);
        return diff.count();
    }

private:
    void timerLoop()
    {
        //first sleep
        std::this_thread::sleep_for(m_interval);
        while (m_active) {
            auto start = std::chrono::steady_clock::now();
            m_notification.notify();
            auto end = std::chrono::steady_clock::now();
            if (m_interval > (end - start)) {
                auto diff = m_interval - (end - start);
                std::this_thread::sleep_for(diff);
            }
        }
    }

    time_t m_interval;
    std::chrono::time_point<std::chrono::steady_clock> m_started;
    std::atomic_bool m_active;
    async::Notification m_notification;

    std::unique_ptr<std::thread> m_thread = nullptr;
};
}
#endif // MU_GLOBAL_TIMER_H
