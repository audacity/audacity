/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2022 MuseScore BVBA and others
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

#ifndef MU_GLOBAL_TASKCHEDULER_H
#define MU_GLOBAL_TASKCHEDULER_H

#include <condition_variable>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include <atomic>
#include <queue>
#include <thread>
#include <type_traits>
#include <utility>

#include "log.h"

namespace mu {
typedef std::invoke_result_t<decltype(std::thread::hardware_concurrency)> thread_pool_size_t;

class TaskScheduler
{
public:

    //!Note Would be moved into globalmodule.cpp for better lifetime control
    static TaskScheduler* instance()
    {
        static TaskScheduler s;
        return &s;
    }

    explicit TaskScheduler(const thread_pool_size_t desiredThreadCount = 0)
        : m_threadPoolSize(vaildateThreadPoolCapacity(desiredThreadCount)),
        m_threadPool(std::make_unique<std::thread[]>(vaildateThreadPoolCapacity(desiredThreadCount)))
    {
        setupThreads();
    }

    ~TaskScheduler()
    {
        waitForAllTasksComplete();
        terminateThreads();
    }

    thread_pool_size_t threadPoolSize() const
    {
        return m_threadPoolSize;
    }

    template<typename FuncT, typename ... ArgsT>
    void push(FuncT&& task, ArgsT&&... args)
    {
        std::function<void()> taskFunctor = std::bind(std::forward<FuncT>(task), std::forward<ArgsT>(args)...);
        {
            const std::lock_guard lock(m_mutex);
            m_taskQueue.push(taskFunctor);
        }
        m_newTaskAvailableCv.notify_one();
    }

    template<typename FuncT, typename ... ArgsT, typename ReturnT = std::invoke_result_t<std::decay_t<FuncT>, std::decay_t<ArgsT>...> >
    std::future<ReturnT> submit(FuncT&& task, ArgsT&&... args)
    {
        std::function<ReturnT()> taskFunctor = std::bind(std::forward<FuncT>(task), std::forward<ArgsT>(args)...);
        std::shared_ptr<std::promise<ReturnT> > promise = std::make_shared<std::promise<ReturnT> >();
        push([taskFunctor, promise] {
            try {
                if constexpr (std::is_void_v<ReturnT>) {
                    std::invoke(taskFunctor);
                    promise->set_value();
                } else {
                    promise->set_value(std::invoke(taskFunctor));
                }
            } catch (...) {
                try {
                    promise->set_exception(std::current_exception());
                } catch (...) {
                    LOGE() << "Unable to schedule a task";
                }
            }
        });

        return promise->get_future();
    }

    void waitForAllTasksComplete()
    {
        m_isWaitingForAllTasksDone = true;
        std::unique_lock<std::mutex> tasks_lock(m_mutex);
        m_taskFinishedCv.wait(tasks_lock, [this] { return m_taskQueue.empty(); });
        m_isWaitingForAllTasksDone = false;
    }

    const std::set<std::thread::id>& threadIdSet() const
    {
        static std::set<std::thread::id> result;

        if (result.empty()) {
            for (thread_pool_size_t i = 0; i < m_threadPoolSize; ++i) {
                result.insert(m_threadPool[i].get_id());
            }
        }

        return result;
    }

    bool containsThread(const std::thread::id& id) const
    {
        const auto& idSet = threadIdSet();
        return idSet.find(id) != idSet.cend();
    }

private:
    void setupThreads()
    {
        m_isActive = true;
        for (thread_pool_size_t i = 0; i < m_threadPoolSize; ++i) {
            m_threadPool[i] = std::thread(&TaskScheduler::th_workerLoop, this);
        }
    }

    void terminateThreads()
    {
        m_isActive = false;
        m_newTaskAvailableCv.notify_all();
        for (thread_pool_size_t i = 0; i < m_threadPoolSize; ++i) {
            m_threadPool[i].join();
        }
    }

    thread_pool_size_t vaildateThreadPoolCapacity(const thread_pool_size_t desiredThreadCount)
    {
        thread_pool_size_t maxCapacity = std::thread::hardware_concurrency();

        if (maxCapacity <= 1) {
            return 1;
        }

        thread_pool_size_t optimalCapacity = maxCapacity / 2;

        if (desiredThreadCount <= 0) {
            return optimalCapacity;
        }

        return desiredThreadCount;
    }

    void th_workerLoop()
    {
        while (m_isActive) {
            std::unique_lock<std::mutex> lock(m_mutex);
            m_newTaskAvailableCv.wait(lock, [this] { return !m_taskQueue.empty() || !m_isActive; });

            if (!m_isActive) {
                return;
            }

            std::function<void()> task = m_taskQueue.front();
            m_taskQueue.pop();

            lock.unlock();

            task();

            if (m_isWaitingForAllTasksDone) {
                m_taskFinishedCv.notify_one();
            }
        }
    }

    std::atomic<bool> m_isActive = false;
    std::atomic<bool> m_isWaitingForAllTasksDone = false;

    mutable std::mutex m_mutex;
    std::condition_variable m_newTaskAvailableCv;
    std::condition_variable m_taskFinishedCv;
    std::queue<std::function<void()> > m_taskQueue;

    thread_pool_size_t m_threadPoolSize = 0;
    std::unique_ptr<std::thread[]> m_threadPool = nullptr;
};
}

#endif // MU_GLOBAL_TASKCHEDULER_H
