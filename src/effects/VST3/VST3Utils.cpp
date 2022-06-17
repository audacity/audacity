/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Utils.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/


#include "VST3Utils.h"
#include "Base64.h"

#include <wx/string.h>
#include <wx/sizer.h>

#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>


wxString VST3Utils::MakePluginPathString(const wxString& modulePath, const std::string& effectUIDString)
{
   return wxString::Format("%s;%s", modulePath, effectUIDString); 
}

bool VST3Utils::ParsePluginPath(const wxString& pluginPath, wxString* modulePath,
                                std::string* effectUIDString)
{
   const auto sep = pluginPath.Find(';', true);
   if(sep != wxNOT_FOUND &&
      //modulePath not empty
      sep > 0 &&
      //effectUIDString not empty
      static_cast<size_t>(sep) < pluginPath.Length() - 1)
   {
      if(modulePath != nullptr)
         *modulePath = pluginPath.Left(sep);
      if(effectUIDString != nullptr)
         *effectUIDString = pluginPath.Mid(static_cast<size_t>(sep) + 1);
      return true;
   }
   return false;
}

wxString VST3Utils::ToWxString(const Steinberg::Vst::TChar* str)
{
   static const wxCSConv csConv { wxFONTENCODING_UTF16 };
   return { reinterpret_cast<const char*>(str), csConv };
}

wxString VST3Utils::MakeAutomationParameterKey(const Steinberg::Vst::ParameterInfo& parameterInfo)
{
   auto suffix = ToWxString(parameterInfo.shortTitle);
   if(suffix.empty())
      suffix = ToWxString(parameterInfo.title);

   if(!suffix.empty())
      return wxString::Format("%lu_", static_cast<unsigned long>(parameterInfo.id)) + suffix;

   return wxString::Format("%lu", static_cast<unsigned long>(parameterInfo.id));
}

bool VST3Utils::ParseAutomationParameterKey(const wxString& key, Steinberg::Vst::ParamID& paramId)
{
   const auto pos = key.Find('_');
   const auto idStr = pos == wxNOT_FOUND ? key : key.Left(pos);
   unsigned long value { };
   if(idStr.ToULong(&value))
   {
      paramId = static_cast<Steinberg::Vst::ParamID>(value);
      return true;
   }
   return false;

}



Steinberg::IPtr<PresetsBufferStream> PresetsBufferStream::fromString(const wxString& str)
{
   Steinberg::Buffer buffer(str.size() / 4 * 3);
   auto len = Base64::Decode(str, buffer);
   assert(len <= buffer.getSize());
   buffer.setSize(len);

   auto result = owned(safenew PresetsBufferStream);
   result->mBuffer.take(buffer);
   return result;
}


wxString PresetsBufferStream::toString() const
{
   return Base64::Encode(mBuffer, mBuffer.getSize());
}



bool VST3Wrapper::FetchSettings(VST3EffectSettings& vst3Settings) const
{
   using namespace Steinberg;

   auto processorState = owned(safenew PresetsBufferStream);
   if (mEffectComponent->getState(processorState) == kResultOk)
   {
      vst3Settings.mProcessorStateStr = processorState->toString();
   }
   else
   {
      vst3Settings.mProcessorStateStr = std::nullopt;
   }

   auto controllerState = owned(safenew PresetsBufferStream);
   if (mEditController->getState(controllerState) == kResultOk)
   {
      vst3Settings.mControllerStateStr = controllerState->toString();
   }
   else
   {
      vst3Settings.mControllerStateStr = std::nullopt;
   }

   return true;   
}



bool VST3Wrapper::StoreSettings(const VST3EffectSettings& vst3settings) const
{
   using namespace Steinberg;

   // we need at least the processor state string, otherwise we can not set the EditController
   if (!vst3settings.mProcessorStateStr)
      return false;

   auto processorState = PresetsBufferStream::fromString(*vst3settings.mProcessorStateStr);
   if (mEffectComponent->setState(processorState) != kResultOk)
      return false;

   if (vst3settings.mControllerStateStr)
   {
      auto controllerState = PresetsBufferStream::fromString(*vst3settings.mControllerStateStr);

      if (mEditController->setComponentState(processorState) != kResultOk ||
         mEditController->setState(controllerState) != kResultOk)
         return false;
   }

   return true;
}


bool VST3Wrapper::LoadPreset(Steinberg::IBStream* fileStream)
{
   using namespace Steinberg;

   return Vst::PresetFile::loadPreset
   (
      fileStream,
      FUID::fromTUID(mEffectClassInfo.ID().data()),
      mEffectComponent.get(),
      mEditController.get()
   );
}


bool VST3Wrapper::SavePreset(Steinberg::IBStream* fileStream) const
{
   using namespace Steinberg;

   return Vst::PresetFile::savePreset
   (
      fileStream,
      FUID::fromTUID(mEffectClassInfo.ID().data()),
      mEffectComponent.get(),
      mEditController.get()
   );
}
