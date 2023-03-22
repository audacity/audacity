#pragma once

#include <pluginterfaces/vst/ivsthostapplication.h>
#include <pluginterfaces/vst/ivstpluginterfacesupport.h>

class AudacityVst3HostApplication final :
   public Steinberg::Vst::IHostApplication,
   public Steinberg::Vst::IPlugInterfaceSupport
{
public:

   DECLARE_FUNKNOWN_METHODS

   AudacityVst3HostApplication();
   virtual ~AudacityVst3HostApplication();

   static IHostApplication& Get();

   Steinberg::tresult PLUGIN_API getName(Steinberg::Vst::String128 name) override;
   Steinberg::tresult PLUGIN_API createInstance(Steinberg::TUID cid, Steinberg::TUID _iid, void** obj) override;
   Steinberg::tresult PLUGIN_API isPlugInterfaceSupported(const Steinberg::TUID _iid) override;
};

