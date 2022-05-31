/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VSTUtils.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <string>
#include <pluginterfaces/vst/vsttypes.h>
#include <pluginterfaces/vst/ivstaudioprocessor.h>

#include <unordered_map>
#include <vector>
#include <functional>

struct EffectSettings;

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

struct VST3Wrapper;

/**
 * \brief Provides a set of useful functions, used across the Audacity VST3 module
 */
class VST3Utils final
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

   /*! Builds a "plain" plugin interface with wx components.
    * editController is used to extract current values from
    * the parameters, and handler is used to update them
    * \param parent Where all parameter controls will be placed (not null)
    * \param editController Effect controller (not null)
    * \param handler Where to report parameter changes (not null)
    * \param wrapper ref, enabling us to visit the parameters
    */
   static void BuildPlainUI(
      wxWindow* parent,
      Steinberg::Vst::IEditController* editController,
      Steinberg::Vst::IComponentHandler* handler,
      const VST3Wrapper& wrapper);

   static wxString ToWxString(const Steinberg::Vst::TChar* str);

   //Builds a string key suitable to use as an Audacity macro param
   //key, which is guaranteed to be unique, and most likely be in
   //a human-readable form.
   static wxString MakeAutomationParameterKey(const Steinberg::Vst::ParameterInfo& info);

   //Attempts to extract a unique VST3 parameter id from the key.
   //Returns true on success.
   static bool ParseAutomationParameterKey(const wxString& key, Steinberg::Vst::ParamID& paramId);
};


struct VST3EffectSettings
{
   std::unordered_map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> mValues;
};

struct VST3Wrapper
{
   // For the time being, here we have only members that are needed
   // to iterate parameters and extract preset state
   //
   Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
   Steinberg::IPtr<Steinberg::Vst::IComponent>      mEffectComponent;

   using ParameterInfo = Steinberg::Vst::ParameterInfo;

   using ParameterVisitor = std::function< bool(const ParameterInfo& pi) >;

   mutable std::vector<ParameterInfo> mParameterInfos;

   // Visit parameters and keep doing so as long as visitor returns true.
   //
   // Return false if the parameters are not yet available
   //
   bool ForEachParameter(ParameterVisitor visitor) const;

   // Stop visiting as soon as the visitor returns true, and return true in that case;
   // otherwise if all visits returned false,  return false.
   //
   bool AtLeastOne(ParameterVisitor visitor) const;

   const std::vector<ParameterInfo>* getParameterInfos() const;

   bool LoadPreset(Steinberg::IBStream*, const Steinberg::FUID& classID);

   bool SavePreset(Steinberg::IBStream*, const Steinberg::FUID& classID) const;

   bool FetchSettings(VST3EffectSettings& settings) const;

   bool StoreSettings(const VST3EffectSettings& settings) const;

   VST3EffectSettings mSettings;  // temporary, until the effect is really stateless

   //! This function will be rewritten when the effect is really stateless
   VST3EffectSettings& GetSettings(EffectSettings&) const
   {
      return const_cast<VST3Wrapper*>(this)->mSettings;
   }

   //! This function will be rewritten when the effect is really stateless
   const VST3EffectSettings& GetSettings(const EffectSettings&) const
   {
      return mSettings;
   }

   //! This is what ::GetSettings will be when the effect becomes really stateless
   /*
   static inline VST3EffectSettings& GetSettings(EffectSettings& settings)
   {
      auto pSettings = settings.cast<VST3EffectSettings>();
      assert(pSettings);
      return *pSettings;
   }
   */
};

