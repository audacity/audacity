/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ConnectionProxy.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "ConnectionProxy.h"

internal::ConnectionProxy::ConnectionProxy(Steinberg::Vst::IConnectionPoint* source)
    : mSource(source)
{
    mThreadId = std::this_thread::get_id();
    FUNKNOWN_CTOR;
}

internal::ConnectionProxy::~ConnectionProxy()
{
    clearPendingMessages();
    FUNKNOWN_DTOR;
}

void internal::ConnectionProxy::clearPendingMessages()
{
    std::lock_guard<std::mutex> lock(mPendingMutex);
    for (auto& message : mPending) {
        message = nullptr;
    }
    mPendingHead = 0;
    mPendingCount = 0;
}

Steinberg::tresult internal::ConnectionProxy::connect(IConnectionPoint* other)
{
    if (other == nullptr) {
        return Steinberg::kInvalidArgument;
    }
    if (mTarget.get() != nullptr) {
        return Steinberg::kResultFalse;
    }

    //Looks a bit awkward, but the source can send messages to
    //the target during connection
    mTarget = other;
    auto result = mSource->connect(this);
    if (result != Steinberg::kResultOk) {
        mTarget = nullptr;
    }
    return result;
}

Steinberg::tresult internal::ConnectionProxy::disconnect(IConnectionPoint* other)
{
    if (other == nullptr) {
        return Steinberg::kInvalidArgument;
    }
    if (other != mTarget.get()) {
        return Steinberg::kResultFalse;
    }

    //Anything still queued is addressed to a connection point that is going away
    clearPendingMessages();

    auto result = mSource->disconnect(this);
    if (result == Steinberg::kResultOk) {
        mTarget = nullptr;
    }
    return result;
}

Steinberg::tresult internal::ConnectionProxy::notify(Steinberg::Vst::IMessage* message)
{
    if (mTarget.get() == nullptr || message == nullptr) {
        return Steinberg::kResultFalse;
    }

    if (std::this_thread::get_id() == mThreadId) {
        return mTarget->notify(message);
    }

    //Off-thread, typically the plug-in's realtime processing thread reporting live
    //data to its controller. The controller and its editor may only be touched on
    //the proxy's own thread, so take a reference to the message and hand it over in
    //deliverPendingMessages(). Everything below is allocation-free and never blocks.
    std::unique_lock<std::mutex> lock(mPendingMutex, std::try_to_lock);
    if (!lock.owns_lock()) {
        return Steinberg::kResultFalse;
    }

    if (mPendingCount == kPendingCapacity) {
        //Full: drop this message rather than evicting the oldest, which would release
        //it - and possibly free it - on a thread that must not be doing either.
        return Steinberg::kResultFalse;
    }

    const auto tail = (mPendingHead + mPendingCount) % kPendingCapacity;
    mPending[tail] = message;
    ++mPendingCount;
    return Steinberg::kResultOk;
}

void internal::ConnectionProxy::deliverPendingMessages()
{
    //Bounded by what was already queued when this started, rather than draining
    //until empty. notify() runs outside the lock, so the sending thread can enqueue
    //again between iterations; an "until empty" loop therefore need never return if
    //a plug-in produces faster than its target consumes, and the thread calling this
    //- the UI thread - would stop repainting and handling input. Anything that
    //arrives while this is running is delivered on the next call instead.
    //
    //The bound is the queued count and not a smaller fixed budget so that a burst is
    //still delivered in one go: the ring is small and drops the newest message when
    //full, so a budget below its capacity would leave the newest values waiting
    //behind older ones and make a meter read late under sustained load.
    std::size_t budget = 0;
    {
        std::lock_guard<std::mutex> lock(mPendingMutex);
        budget = mPendingCount;
    }

    //One at a time, with the message released outside the lock: notify() reaches
    //plug-in code that may call back into this proxy, and the final release of a
    //message may free it - neither should happen while holding the lock.
    for (std::size_t delivered = 0; delivered < budget; ++delivered) {
        Steinberg::IPtr<Steinberg::Vst::IMessage> message;
        Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> target;
        {
            std::lock_guard<std::mutex> lock(mPendingMutex);
            if (mPendingCount == 0) {
                return;
            }
            message = std::move(mPending[mPendingHead]);
            mPendingHead = (mPendingHead + 1) % kPendingCapacity;
            --mPendingCount;
            target = mTarget;
        }

        if (target && message) {
            target->notify(message);
        }
    }
}

IMPLEMENT_FUNKNOWN_METHODS(internal::ConnectionProxy, Steinberg::Vst::IConnectionPoint, Steinberg::Vst::IConnectionPoint::iid);
