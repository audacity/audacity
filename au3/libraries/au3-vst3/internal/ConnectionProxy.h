/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ConnectionProxy.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include <array>
#include <cstddef>
#include <mutex>
#include <utility>
#include <thread>
#include <pluginterfaces/vst/ivstmessage.h>

namespace internal {
//!Host's proxy object between connection points
/*! Though it's not necessary to place proxy, it's recommended to do so.
    * This proxy makes sure the target connection point is only ever notified
    * on the thread the proxy object itself was created on (the UI thread).
    *
    * A plug-in whose processor and controller are separate objects cannot reach
    * its own editor directly - it streams live data to the controller (meters,
    * gain reduction, analyser curves) through IConnectionPoint, and does so from
    * its processing thread. Those notifications therefore arrive here off the UI
    * thread; discarding them leaves the plug-in's own GUI frozen. They are queued
    * instead, and handed over by deliverPendingMessages() on the UI thread.
    */
class ConnectionProxy final : public Steinberg::Vst::IConnectionPoint
{
    //!Bounded on purpose: this carries a continuous stream where only recent data
    //!matters, and it must not grow while nothing is draining it. Sized to give a
    //!UI thread refreshing at ~60Hz about a second of slack before messages are lost.
    static constexpr std::size_t kPendingCapacity = 64;

    std::thread::id mThreadId;

    Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mSource;
    Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mTarget;

    //!Messages received on a thread other than mThreadId, awaiting delivery.
    //!notify() may be called from a plug-in's realtime processing thread, so this is
    //!a fixed-size, pre-allocated ring: enqueuing never allocates, frees, or blocks.
    std::mutex mPendingMutex;
    std::array<Steinberg::IPtr<Steinberg::Vst::IMessage>, kPendingCapacity> mPending;
    std::size_t mPendingHead { 0 };
    std::size_t mPendingCount { 0 };

    void clearPendingMessages();

public:

    DECLARE_FUNKNOWN_METHODS;

    ConnectionProxy(Steinberg::Vst::IConnectionPoint* source);
    virtual ~ConnectionProxy();

    Steinberg::tresult PLUGIN_API connect(IConnectionPoint* other) override;

    Steinberg::tresult PLUGIN_API disconnect(IConnectionPoint* other) override;

    Steinberg::tresult PLUGIN_API notify(Steinberg::Vst::IMessage* message) override;

    //!Delivers everything notify() queued from other threads. Must be called on the
    //!thread this proxy was created on; call it periodically while a plug-in's editor
    //!is open, otherwise the plug-in's own GUI never sees its live data.
    void deliverPendingMessages();
};
}
