#pragma once

#include <pluginterfaces/vst/ivsthostapplication.h>

class AudacityVst3HostApplication final : public Steinberg::Vst::IHostApplication
{
public:

   DECLARE_FUNKNOWN_METHODS

   AudacityVst3HostApplication();
   virtual ~AudacityVst3HostApplication();

   static AudacityVst3HostApplication& Get();

   Steinberg::tresult PLUGIN_API getName(Steinberg::Vst::String128 name) override;
   Steinberg::tresult PLUGIN_API createInstance(Steinberg::TUID cid, Steinberg::TUID _iid, void** obj) override;
};

