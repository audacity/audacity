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
    FUNKNOWN_DTOR;
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

    auto result = mSource->disconnect(this);
    if (result == Steinberg::kResultOk) {
        mTarget = nullptr;
    }
    return result;
}

Steinberg::tresult internal::ConnectionProxy::notify(Steinberg::Vst::IMessage* message)
{
    if (mTarget.get() == nullptr
        || std::this_thread::get_id() != mThreadId) {
        return Steinberg::kResultFalse;
    }

    return mTarget->notify(message);
}

IMPLEMENT_FUNKNOWN_METHODS(internal::ConnectionProxy, Steinberg::Vst::IConnectionPoint, Steinberg::Vst::IConnectionPoint::iid);
