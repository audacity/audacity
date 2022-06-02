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

#include <vector>
#include <functional>
#include <unordered_map>

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
   
   static wxString ToWxString(const Steinberg::Vst::TChar* str);

   //Builds a string key suitable to use as an Audacity macro param
   //key, which is guaranteed to be unique, and most likely be in
   //a human-readable form.
   static wxString MakeAutomationParameterKey(const Steinberg::Vst::ParameterInfo& info);

};

struct EffectSettings;

struct VST3EffectSettings
{
   std::unordered_map<Steinberg::Vst::ParamID, Steinberg::Vst::ParamValue> mValues;
};

struct VST3Wrapper
{
   VST3Wrapper(std::shared_ptr<VST3::Hosting::Module> module, VST3::Hosting::ClassInfo effectClassInfo)
      : mModule(std::move(module)),
        mEffectClassInfo(std::move(effectClassInfo))
   {}

   // Keep strong reference to a module; this because it has to be destroyed in the destructor of this class,
   // otherwise the destruction of mEditController and mEffectComponent would trigger a memory fault.
   std::shared_ptr<VST3::Hosting::Module> mModule;

   const VST3::Hosting::ClassInfo mEffectClassInfo;

   // For the time being, here we have only members that are needed
   // to iterate parameters and extract preset state
   //
   Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
   Steinberg::IPtr<Steinberg::Vst::IComponent>      mEffectComponent;

   using ParameterInfo = Steinberg::Vst::ParameterInfo;

   using ParameterVisitor = std::function< bool(const ParameterInfo& pi) >;

   //! Visit parameters and keep doing so as long as visitor returns true.
   //
   // @return false if the parameters are not yet available
   //
   bool ForEachParameter(ParameterVisitor visitor) const;

   //! Stop visiting as soon as the visitor returns true
   //
   // @return true  as soon as a visit returns true
   // @return false if none of the visits returned true
   //
   bool AnyOf(ParameterVisitor visitor) const;

   //! Return a pointer to the cached parameterinfos
   //
   // @return nullptr if the plugin could not be fully intialized
   //
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

private:

   //! made mutable because it is cached once on demand
   mutable std::vector<ParameterInfo> mParameterInfos;
};

