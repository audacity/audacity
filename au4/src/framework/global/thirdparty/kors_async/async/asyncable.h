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
#ifndef KORS_ASYNC_ASYNCABLE_H
#define KORS_ASYNC_ASYNCABLE_H

#include <set>
#include <cstdint>

namespace kors::async {
class Asyncable
{
public:

    enum class AsyncMode {
        AsyncSetOnce = 0,
        AsyncSetRepeat
    };

    virtual ~Asyncable()
    {
        disconnectAll();
    }

    struct IConnectable {
        virtual ~IConnectable() {}
        virtual void disconnectAsync(Asyncable* a) = 0;
    };

    bool isConnectedAsync() const { return !m_connects.empty(); }

    void connectAsync(IConnectable* c)
    {
        if (c && m_connects.count(c) == 0) {
            m_connects.insert(c);
        }
    }

    void disconnectAsync(IConnectable* c)
    {
        m_connects.erase(c);
    }

    void disconnectAll()
    {
        auto copy = m_connects;
        for (IConnectable* c : copy) {
            c->disconnectAsync(this);
        }

        m_connects.clear();
    }

private:
    std::set<IConnectable*> m_connects;
};
}

#endif // KORS_ASYNC_ASYNCABLE_H
