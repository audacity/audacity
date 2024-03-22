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
#include "queuedinvoker.h"

using namespace kors::async;

QueuedInvoker* QueuedInvoker::instance()
{
    static QueuedInvoker i;
    return &i;
}

void QueuedInvoker::invoke(const std::thread::id& callbackTh, const Functor& f, bool isAlwaysQueued)
{
    if (m_onMainThreadInvoke) {
        if (callbackTh == m_mainThreadID) {
            m_onMainThreadInvoke(f, isAlwaysQueued);
        }
    }

    std::lock_guard<std::recursive_mutex> lock(m_mutex);
    m_queues[callbackTh].push(f);
}

void QueuedInvoker::processEvents()
{
    Queue q;
    {
        std::lock_guard<std::recursive_mutex> lock(m_mutex);
        auto n = m_queues.extract(std::this_thread::get_id());
        if (!n.empty()) {
            q = n.mapped();
        }
    }
    while (!q.empty()) {
        const auto& f = q.front();
        if (f) {
            f();
        }
        q.pop();
    }
}

void QueuedInvoker::onMainThreadInvoke(const std::function<void(const std::function<void()>&, bool)>& f)
{
    m_onMainThreadInvoke = f;
    m_mainThreadID = std::this_thread::get_id();
}
