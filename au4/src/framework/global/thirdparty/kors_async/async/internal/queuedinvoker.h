/*
MIT License

Copyright (c) 2020 Igor Korsukov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
#ifndef KORS_ASYNC_QUEUEDINVOKER_H
#define KORS_ASYNC_QUEUEDINVOKER_H

#include <functional>
#include <queue>
#include <map>
#include <mutex>
#include <thread>

namespace kors::async {
class QueuedInvoker
{
public:

    static QueuedInvoker* instance();

    using Functor = std::function<void ()>;

    void invoke(const std::thread::id& th, const Functor& f, bool isAlwaysQueued = false);
    void processEvents();
    void onMainThreadInvoke(const std::function<void(const std::function<void()>&, bool)>& f);

private:

    QueuedInvoker() = default;

    using Queue = std::queue<Functor>;

    std::recursive_mutex m_mutex;
    std::map<std::thread::id, Queue > m_queues;

    std::function<void(const std::function<void()>&, bool)> m_onMainThreadInvoke;
    std::thread::id m_mainThreadID;
};
}

#endif // KORS_ASYNC_QUEUEDINVOKER_H
