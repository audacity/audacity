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

