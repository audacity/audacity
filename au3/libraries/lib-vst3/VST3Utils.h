/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VSTUtils.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <pluginterfaces/vst/ivstaudioprocessor.h>
#include <string>
#include <public.sdk/source/vst/hosting/module.h>
#include <public.sdk/source/vst/vstpresetfile.h>

class wxString;
class wxWindow;

namespace Steinberg
{
   namespace Vst
   {
      class IEditController;
      class IComponentHandler;
      struct ParameterInfo;
   }
}

/**
 * \brief Provides a set of useful functions, used across the Audacity VST3 module
 */
class VST3_API VST3Utils final
{
public:

   //Creates a plugin path string, which can be used to uniquely identify the effect.
   //modulePath - path to the vst3 file/catalog
   //effectUIDString - the effect's VST3::UID converted to a string
   static wxString MakePluginPathString(const wxString& modulePath, const std::string& effectUIDString);

   //Attempts to parse plugin path string, returns true if string is considered
   //to be a valid plugin path. modulePath and effectUIDString are written if
   //provided.
   static bool ParsePluginPath(const wxString& pluginPath, wxString* modulePath, std::string* effectUIDString);
   
   static wxString ToWxString(const Steinberg::Vst::TChar* str);

   //Builds a string key suitable to use as an Audacity macro param
   //key, which is guaranteed to be unique, and most likely be in
   //a human-readable form.
   static wxString MakeAutomationParameterKey(const Steinberg::Vst::ParameterInfo& info);

   //Attempts to extract a unique VST3 parameter id from the key.
   //Returns true on success.
   static bool ParseAutomationParameterKey(const wxString& key, Steinberg::Vst::ParamID& paramId);

   static wxString MakeFactoryPresetID(Steinberg::Vst::UnitID unitId, Steinberg::int32 programIndex);
   static bool ParseFactoryPresetID(const wxString& presetId, Steinberg::Vst::UnitID& unitId, Steinberg::int32& programIndex);

   static wxString GetFactoryPresetsPath(const VST3::Hosting::ClassInfo& effectClassInfo);
};

class PresetsBufferStream : public Steinberg::Vst::BufferStream
{
public:

   static Steinberg::IPtr<PresetsBufferStream> fromString(const wxString& str);

   wxString toString() const;
};
