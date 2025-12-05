/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ConnectionProxy.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include <thread>
#include <pluginterfaces/vst/ivstmessage.h>

namespace internal {
//!Host's proxy object between connection points
/*! Though it's not necessary to place proxy, it's recommended to do so.
    * Right now the only "useful" task performed by this proxy is ensuring
    * that messages are sent on the same thread on which proxy object itself
    * was created
    */
class ConnectionProxy final : public Steinberg::Vst::IConnectionPoint
{
    std::thread::id mThreadId;

    Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mSource;
    Steinberg::IPtr<Steinberg::Vst::IConnectionPoint> mTarget;
public:

    DECLARE_FUNKNOWN_METHODS;

    ConnectionProxy(Steinberg::Vst::IConnectionPoint* source);
    virtual ~ConnectionProxy();

    Steinberg::tresult PLUGIN_API connect(IConnectionPoint* other) override;

    Steinberg::tresult PLUGIN_API disconnect(IConnectionPoint* other) override;

    Steinberg::tresult PLUGIN_API notify(Steinberg::Vst::IMessage* message) override;
};
}
