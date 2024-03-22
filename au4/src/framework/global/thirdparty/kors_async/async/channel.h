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
#ifndef KORS_ASYNC_CHANNEL_H
#define KORS_ASYNC_CHANNEL_H

#include <memory>
#include "internal/abstractinvoker.h"

namespace kors::async {
template<typename ... T>
class Channel
{
public:
    Channel() = default;
    Channel(const Channel& ch)
        : m_ptr(ch.ptr()) {}
    ~Channel() {}

    Channel& operator=(const Channel& ch)
    {
        if (m_ptr == ch.ptr()) {
            return *this;
        }

        m_ptr = ch.ptr();
        return *this;
    }

    void send(const T&... d)
    {
        NotifyData nd;
        nd.setArg<T...>(0, d ...);
        ptr()->invoke(Receive, nd);
    }

    template<typename Func>
    void onReceive(const Asyncable* receiver, Func f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        ptr()->addCallBack(Receive, const_cast<Asyncable*>(receiver), new ReceiveCall<Func, T...>(f), mode);
    }

    void resetOnReceive(const Asyncable* receiver)
    {
        ptr()->removeCallBack(Receive, const_cast<Asyncable*>(receiver));
    }

    void close()
    {
        ptr()->invoke(Close);
    }

    template<typename Func>
    void onClose(const Asyncable* receiver, Func f, Asyncable::AsyncMode mode = Asyncable::AsyncMode::AsyncSetOnce)
    {
        ptr()->addCallBack(Close, const_cast<Asyncable*>(receiver), new CloseCall<Func>(f), mode);
    }

    bool isConnected() const
    {
        return m_ptr && ptr()->isConnected();
    }

private:

    enum CallType {
        Undefined = 0,
        Receive,
        Close
    };

    struct IReceive {
        virtual ~IReceive() {}
        virtual void received(const NotifyData& d) = 0;
    };

    template<typename Call, typename ... Arg>
    struct ReceiveCall : public IReceive {
        Call f;
        ReceiveCall(Call _f)
            : f(_f) {}
        void received(const NotifyData& d) { std::apply(f, d.args<Arg...>()); }
    };

    struct IClose {
        virtual ~IClose() {}
        virtual void closed() = 0;
    };

    template<typename Call>
    struct CloseCall : public IClose {
        Call f;
        CloseCall(Call _f)
            : f(_f) {}
        void closed() { f(); }
    };

    struct ChannelInvoker : public AbstractInvoker
    {
        friend class Channel;

        ChannelInvoker() = default;
        ~ChannelInvoker()
        {
            removeAllCallBacks();
        }

        void deleteCall(int _type, void* call) override
        {
            CallType type = static_cast<CallType>(_type);
            switch (type) {
            case Undefined: {} break;
            case Receive: {
                delete static_cast<IReceive*>(call);
            } break;
            case Close: {
                delete static_cast<IClose*>(call);
            } break;
            }
        }

        void doInvoke(int callKey, void* call, const NotifyData& d) override
        {
            CallType type = static_cast<CallType>(callKey);
            switch (type) {
            case Undefined:  break;
            case Receive:
                static_cast<IReceive*>(call)->received(d);
                break;
            case Close:
                static_cast<IClose*>(call)->closed();
                break;
            }
        }
    };

    std::shared_ptr<ChannelInvoker> ptr() const
    {
        if (!m_ptr) {
            m_ptr = std::make_shared<ChannelInvoker>();
        }
        return m_ptr;
    }

    mutable std::shared_ptr<ChannelInvoker> m_ptr = nullptr;
};
}

#endif // KORS_ASYNC_CHANNEL_H
