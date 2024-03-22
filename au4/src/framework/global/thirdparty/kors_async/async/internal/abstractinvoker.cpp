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
#include "abstractinvoker.h"

#include <cassert>

#include "queuedinvoker.h"

using namespace kors::async;

AbstractInvoker::AbstractInvoker()
{
}

AbstractInvoker::~AbstractInvoker()
{
    std::lock_guard<std::mutex> lock(m_qInvokersMutex);
    for (QInvoker* qi : m_qInvokers) {
        qi->invalidate();
    }
}

void AbstractInvoker::invoke(int type)
{
    invoke(type, NotifyData());
}

void AbstractInvoker::invoke(int type, const NotifyData& data)
{
    auto it = m_callbacks.find(type);
    if (it == m_callbacks.end()) {
        return;
    }

    std::thread::id threadID = std::this_thread::get_id();

    //! NOTE: explicit copy because collection can be modified from elsewhere
    CallBacks callbacks = it->second;

    for (const CallBack& c : callbacks) {
        if (!it->second.containsReceiver(c.receiver)) {
            std::cout << "Skipping removed receiver";
            continue;
        }
        if (c.threadID == threadID) {
            invokeCallback(type, c, data);
        } else {
            QInvoker* qi = new QInvoker(this, type, c, data);
            QueuedInvoker::instance()->invoke(c.threadID, [qi]() {
                qi->invoke();
                delete qi;
            });
        }
    }
}

void AbstractInvoker::invokeCallback(int type, const CallBack& c, const NotifyData& data)
{
    assert(c.threadID == std::this_thread::get_id());

    if (!containsReceiver(c.receiver)) {
        return;
    }

    if (c.receiver && !c.receiver->isConnectedAsync()) {
        return;
    }

    doInvoke(type, c.call, data);
}

void AbstractInvoker::processEvents()
{
    QueuedInvoker::instance()->processEvents();
}

void AbstractInvoker::onMainThreadInvoke(const std::function<void(const std::function<void()>&, bool)>& f)
{
    QueuedInvoker::instance()->onMainThreadInvoke(f);
}

bool AbstractInvoker::isConnected() const
{
    for (auto it = m_callbacks.cbegin(); it != m_callbacks.cend(); ++it) {
        const CallBacks& cs = it->second;
        if (cs.size() > 0) {
            return true;
        }
    }
    return false;
}

int AbstractInvoker::CallBacks::receiverIndexOf(Asyncable* receiver) const
{
    for (size_t i = 0; i < size(); ++i) {
        if (at(i).receiver == receiver) {
            return int(i);
        }
    }
    return -1;
}

bool AbstractInvoker::CallBacks::containsReceiver(Asyncable* receiver) const
{
    return receiverIndexOf(receiver) > -1;
}

void AbstractInvoker::removeCallBack(int type, Asyncable* receiver)
{
    auto it = m_callbacks.find(type);
    if (it == m_callbacks.end()) {
        return;
    }

    CallBacks& callbacks = it->second;
    int index = callbacks.receiverIndexOf(receiver);
    if (index < 0) {
        return;
    }

    CallBack c = callbacks.at(index);
    if (c.receiver) {
        c.receiver->disconnectAsync(this);
    }
    callbacks.erase(callbacks.begin() + index);

    {
        std::lock_guard<std::mutex> lock(m_qInvokersMutex);
        for (QInvoker* qi : m_qInvokers) {
            if (qi->call.call == c.call) {
                qi->invalidate();
                break;
            }
        }
    }

    deleteCall(type, c.call);
}

void AbstractInvoker::removeAllCallBacks()
{
    for (auto it = m_callbacks.begin(); it != m_callbacks.end(); ++it) {
        for (CallBack& c : it->second) {
            if (c.receiver) {
                c.receiver->disconnectAsync(this);
            }

            deleteCall(c.type, c.call);
        }
    }
    m_callbacks.clear();
}

void AbstractInvoker::addCallBack(int type, Asyncable* receiver, void* call, Asyncable::AsyncMode mode)
{
    const CallBacks& callbacks = m_callbacks[type];
    if (callbacks.containsReceiver(receiver)) {
        switch (mode) {
        case Asyncable::AsyncMode::AsyncSetOnce:
            deleteCall(type, call);
            return;
        case Asyncable::AsyncMode::AsyncSetRepeat:
            removeCallBack(type, receiver);
            break;
        }
    }

    CallBack c(std::this_thread::get_id(), type, receiver, call);
    m_callbacks[type].push_back(c);

    if (c.receiver) {
        c.receiver->connectAsync(this);
    }
}

void AbstractInvoker::disconnectAsync(Asyncable* receiver)
{
    std::vector<int> types;
    for (auto it = m_callbacks.begin(); it != m_callbacks.end(); ++it) {
        for (CallBack& c : it->second) {
            if (c.receiver == receiver) {
                types.push_back(c.type);
            }
        }
    }

    for (int type : types) {
        removeCallBack(type, receiver);
    }
}

void AbstractInvoker::addQInvoker(QInvoker* qi)
{
    std::lock_guard<std::mutex> lock(m_qInvokersMutex);
    m_qInvokers.push_back(qi);
}

void AbstractInvoker::removeQInvoker(QInvoker* qi)
{
    std::lock_guard<std::mutex> lock(m_qInvokersMutex);
    m_qInvokers.remove(qi);
}

bool AbstractInvoker::containsReceiver(Asyncable* receiver) const
{
    for (auto it = m_callbacks.begin(); it != m_callbacks.end(); ++it) {
        for (const CallBack& c : it->second) {
            if (c.receiver == receiver) {
                return true;
            }
        }
    }

    return false;
}
