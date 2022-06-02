/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Utils.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3Utils.h"

#include <wx/string.h>
#include <wx/sizer.h>

#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>
#include <public.sdk/source/vst/vstpresetfile.h>

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


const std::vector<VST3Wrapper::ParameterInfo>* VST3Wrapper::getParameterInfos() const
{
   using namespace Steinberg;
      
   if (mParameterInfos.size() == 0)
   {
      if (mEditController == nullptr)
         return nullptr;

      for (size_t i = 0, count = mEditController->getParameterCount(); i < count; ++i)
      {
         Steinberg::Vst::ParameterInfo vstParameterInfo{ };
         if (mEditController->getParameterInfo(i, vstParameterInfo) == Steinberg::kResultOk)
         {
            // Add all parameterinfos which are not hidden, regardless of whether they
            // can be automated or not, because different flags of them will be checked
            // by different methods.

            if (!(vstParameterInfo.flags & Vst::ParameterInfo::kIsHidden))
               mParameterInfos.push_back(vstParameterInfo);
         }
      }
   }

   return &mParameterInfos;
}


bool VST3Wrapper::ForEachParameter(ParameterVisitor visitor) const
{
   const auto pParameterInfos = getParameterInfos();
   if (pParameterInfos == nullptr)
      return false;

   for (const auto& parameterInfo : *pParameterInfos)
   {
      if (!visitor(parameterInfo))
         break;
   }

   return true;
}


bool VST3Wrapper::AnyOf(ParameterVisitor visitor) const
{
   const auto pParameterInfos = getParameterInfos();
   if (pParameterInfos == nullptr)
      return false;

   for (const auto& parameterInfo : *pParameterInfos)
   {
      if (visitor(parameterInfo))
      {
         return true;
      }
   }

   return false;
}


bool VST3Wrapper::LoadPreset(Steinberg::IBStream* fileStream, const Steinberg::FUID& classID)
{
   using namespace Steinberg;

   return Vst::PresetFile::loadPreset
   (
      fileStream,
      classID,
      mEffectComponent.get(),
      mEditController.get()
   );
}


bool VST3Wrapper::SavePreset(Steinberg::IBStream* fileStream, const Steinberg::FUID& classID) const
{
   using namespace Steinberg;

   return Vst::PresetFile::savePreset
   (
      fileStream,
      classID,
      mEffectComponent.get(),
      mEditController.get()
   );
}


bool VST3Wrapper::FetchSettings(VST3EffectSettings& settings) const
{
   settings.mValues.clear();

   return ForEachParameter(

      [&](const ParameterInfo& pi)
      {
         if (pi.flags & Steinberg::Vst::ParameterInfo::kIsReadOnly)
            return true;

         const auto id = pi.id;
         settings.mValues[id] = mEditController->getParamNormalized(id);
         return true;
      }
   );
}


bool VST3Wrapper::StoreSettings(const VST3EffectSettings& settings) const
{
   if (mEditController == nullptr)
      return false;

   return ForEachParameter
   (
      [&](const ParameterInfo& parameterinfo)
      {
         if (parameterinfo.flags & Steinberg::Vst::ParameterInfo::kIsReadOnly)
            return true;

         auto itr = settings.mValues.find(parameterinfo.id);
         if (itr != settings.mValues.end())
         {
            const auto& value = itr->second;

            mEditController->setParamNormalized(parameterinfo.id, value);
         }

         return true;
      }
   );

}
