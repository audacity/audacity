/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3EffectBase.cpp

  @author Vitaly Sverchinsky

  Paul Licameli split from VST3Effect.cpp

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3EffectBase.h"
#include "VST3Utils.h"
#include "VST3Wrapper.h"
#include "VST3Instance.h"

EffectFamilySymbol VST3EffectBase::GetFamilySymbol()
{
   return XO("VST3");
}

VST3EffectBase::~VST3EffectBase() = default;

VST3EffectBase::VST3EffectBase(
   std::shared_ptr<VST3::Hosting::Module> module,
   VST3::Hosting::ClassInfo effectClassInfo)
      : mModule(std::move(module)), mEffectClassInfo(std::move(effectClassInfo))
{
}

PluginPath VST3EffectBase::GetPath() const
{
   return VST3Utils::MakePluginPathString( { mModule->getPath() }, mEffectClassInfo.ID().toString());
}

ComponentInterfaceSymbol VST3EffectBase::GetSymbol() const
{
   return wxString { mEffectClassInfo.name() };
}

VendorSymbol VST3EffectBase::GetVendor() const
{
   return wxString { mEffectClassInfo.vendor() };
}

wxString VST3EffectBase::GetVersion() const
{
   return mEffectClassInfo.version();
}

TranslatableString VST3EffectBase::GetDescription() const
{
   //i18n-hint VST3 effect description string
   return XO("SubCategories: %s").Format( mEffectClassInfo.subCategoriesString() );
}

EffectType VST3EffectBase::GetType() const
{
   using namespace Steinberg::Vst::PlugType;
   if(mEffectClassInfo.subCategoriesString() == kFxGenerator)
      return EffectTypeGenerate;
   const auto& cats = mEffectClassInfo.subCategories();

   if(std::find(cats.begin(), cats.end(), kFx) != cats.end())
      return EffectTypeProcess;

   return EffectTypeNone;
}

EffectFamilySymbol VST3EffectBase::GetFamily() const
{
   return VST3EffectBase::GetFamilySymbol();
}

bool VST3EffectBase::IsInteractive() const
{
   return true;
}

bool VST3EffectBase::IsDefault() const
{
   return false;
}

auto VST3EffectBase::RealtimeSupport() const -> RealtimeSince
{
   return GetType() == EffectTypeProcess
      ? RealtimeSince::After_3_1
      : RealtimeSince::Never;
}

bool VST3EffectBase::SupportsAutomation() const
{
   return true;
}

bool VST3EffectBase::SaveSettings(
   const EffectSettings& settings, CommandParameters& parms) const
{
   VST3Wrapper::SaveSettings(settings, parms);
   return true;
}

bool VST3EffectBase::LoadSettings(
   const CommandParameters& parms, EffectSettings& settings) const
{
   VST3Wrapper::LoadSettings(parms, settings);
   return true;
}

OptionalMessage VST3EffectBase::LoadUserPreset(
   const RegistryPath& name, EffectSettings& settings) const
{
   return VST3Wrapper::LoadUserPreset(*this, name, settings);
}

bool VST3EffectBase::SaveUserPreset(
   const RegistryPath& name, const EffectSettings& settings) const
{
   VST3Wrapper::SaveUserPreset(*this, name, settings);
   return true;
}

RegistryPaths VST3EffectBase::GetFactoryPresets() const
{
   if(!mRescanFactoryPresets)
      return mFactoryPresetNames;

   VST3Wrapper wrapper(*mModule, mEffectClassInfo);
   for(auto& desc : wrapper.FindFactoryPresets())
   {
      mFactoryPresetNames.push_back(desc.displayName);
      mFactoryPresetIDs.push_back(desc.id);
   }
   mRescanFactoryPresets = false;

   return mFactoryPresetNames;
}

OptionalMessage VST3EffectBase::LoadFactoryPreset(int index, EffectSettings& settings) const
{
   if(index >= 0 && index < mFactoryPresetIDs.size())
   {
      VST3Wrapper wrapper(*mModule, mEffectClassInfo);
      wrapper.InitializeComponents();
      wrapper.LoadPreset(mFactoryPresetIDs[index]);
      wrapper.FlushParameters(settings);
      wrapper.StoreSettings(settings);
      return { nullptr };
   }
   return { };
}

std::shared_ptr<EffectInstance> VST3EffectBase::MakeInstance() const
{
   return std::make_shared<VST3Instance>(*this, *mModule, mEffectClassInfo);
}

bool VST3EffectBase::CanExportPresets() const
{
   return true;
}

bool VST3EffectBase::HasOptions() const
{
   return true;
}

void VST3EffectBase::LoadPreset(const wxString& id, EffectSettings& settings) const
{
   auto wrapper = std::make_unique<VST3Wrapper>(*mModule, mEffectClassInfo);
   wrapper->InitializeComponents();
   wrapper->LoadPreset(id);
   wrapper->StoreSettings(settings);
}

EffectSettings VST3EffectBase::MakeSettings() const
{
   return VST3Wrapper::MakeSettings();
}

bool VST3EffectBase::CopySettingsContents(const EffectSettings& src, EffectSettings& dst) const
{
   VST3Wrapper::CopySettingsContents(src, dst);
   return true;
}
