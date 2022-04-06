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
   Steinberg::Buffer buffer(str.length() / 4 * 3);
   const auto numBytes = Base64::Decode(str, buffer);
   //BufferStream uses fill size as a cursor position and size as a stream end position
   //To prevent plugins from fetching bytes past the meaningful data we need to truncate
   //end position
   buffer.setSize(numBytes);

   auto result = owned(safenew PresetsBufferStream);
   result->mBuffer.take(buffer);
   return result;
}

wxString PresetsBufferStream::toString() const
{
   auto str = Base64::Encode(mBuffer, mBuffer.getFillSize());
   return str;
}
