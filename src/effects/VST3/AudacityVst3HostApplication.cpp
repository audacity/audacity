#include "AudacityVst3HostApplication.h"

#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <pluginterfaces/vst/ivstcomponent.h>
#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <public.sdk/source/vst/hosting/hostclasses.h>
#include <public.sdk/source/vst/utility/stringconvert.h>
#include <algorithm>


AudacityVst3HostApplication::AudacityVst3HostApplication()
{
   FUNKNOWN_CTOR
}

AudacityVst3HostApplication::~AudacityVst3HostApplication()
{
   FUNKNOWN_DTOR
}

Steinberg::Vst::IHostApplication& AudacityVst3HostApplication::Get()
{
   static AudacityVst3HostApplication instance;
   return instance;
}


Steinberg::tresult PLUGIN_API AudacityVst3HostApplication::queryInterface (const char* _iid, void** obj)
{
   QUERY_INTERFACE (_iid, obj, FUnknown::iid, IHostApplication)
   QUERY_INTERFACE (_iid, obj, IHostApplication::iid, IHostApplication)
   QUERY_INTERFACE (_iid, obj, IPlugInterfaceSupport::iid, IPlugInterfaceSupport)

	*obj = nullptr;
	return Steinberg::kResultFalse;
}

Steinberg::uint32 PLUGIN_API AudacityVst3HostApplication::addRef ()
{
	return 1;
}

Steinberg::uint32 PLUGIN_API AudacityVst3HostApplication::release ()
{
	return 1;
}

Steinberg::tresult PLUGIN_API AudacityVst3HostApplication::getName(Steinberg::Vst::String128 name)
{
   return VST3::StringConvert::convert ("Audacity VST3 host application", name) ?
      Steinberg::kResultTrue : Steinberg::kInternalError;
}

Steinberg::tresult PLUGIN_API AudacityVst3HostApplication::createInstance(Steinberg::TUID cid, Steinberg::TUID _iid, void** obj)
{
   using namespace Steinberg;

   FUID classID (FUID::fromTUID (cid));
	FUID interfaceID (FUID::fromTUID (_iid));
	if (classID == Vst::IMessage::iid && interfaceID == Vst::IMessage::iid)
	{
		*obj = safenew Vst::HostMessage;
		return kResultTrue;
	}
   else if (classID == Vst::IAttributeList::iid && interfaceID == Vst::IAttributeList::iid)
	{
		*obj = safenew Vst::HostAttributeList;
		return kResultTrue;
	}
	*obj = nullptr;
	return kResultFalse;
}

Steinberg::tresult AudacityVst3HostApplication::isPlugInterfaceSupported(const Steinberg::TUID _iid)
{
   static auto supportedInterfaces = {
      Steinberg::Vst::IComponent::iid,
      Steinberg::Vst::IAudioProcessor::iid,
      Steinberg::Vst::IEditController::iid,
      Steinberg::Vst::IConnectionPoint::iid
   };

   auto uid = Steinberg::FUID::fromTUID(_iid);
   if(std::find(supportedInterfaces.begin(), supportedInterfaces.end(), uid) != supportedInterfaces.end())
      return Steinberg::kResultTrue;
   return Steinberg::kResultFalse;
}

