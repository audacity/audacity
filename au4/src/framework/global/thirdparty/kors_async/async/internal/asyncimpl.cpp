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
#include "asyncimpl.h"

#include "queuedinvoker.h"

using namespace kors::async;

AsyncImpl* AsyncImpl::instance()
{
    static AsyncImpl a;
    return &a;
}

void AsyncImpl::disconnectAsync(Asyncable* caller)
{
    std::lock_guard locker(m_mutex);
    uint64_t key = 0;
    std::map<uint64_t, Call>::const_iterator it = m_calls.cbegin(), end = m_calls.cend();
    for (; it != end; ++it) {
        if (it->second.caller == caller) {
            key = it->first;
            break;
        }
    }

    if (key) {
        m_calls.erase(key);
    }
}

void AsyncImpl::call(Asyncable* caller, IFunction* f, const std::thread::id& th)
{
    if (caller) {
        caller->connectAsync(this);
    }

    uint64_t key = reinterpret_cast<uint64_t>(f);
    {
        std::lock_guard locker(m_mutex);
        m_calls[key] = Call(caller, f);
    }

    auto functor = [this, key]() { onCall(key); };
    QueuedInvoker::instance()->invoke(th, functor, true);
}

void AsyncImpl::onCall(uint64_t key)
{
    Call c;
    {
        std::lock_guard locker(m_mutex);
        auto it = m_calls.find(key);

        //! NOTE Probably disconnected
        if (it == m_calls.end()) {
            return;
        }

        c = it->second;
        m_calls.erase(it);
    }

    c.f->call();

    if (c.caller) {
        c.caller->disconnectAsync(this);
    }

    delete c.f;
}
