/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3Effect.h"

#include <wx/log.h>

#include "internal/PlugFrame.h"
#include "internal/ConnectionProxy.h"

#include "SelectFile.h"

#include "ShuttleGui.h"

#include "VST3Utils.h"
#include "VST3ParametersWindow.h"
#include "VST3OptionsDialog.h"
#include "VST3Wrapper.h"

#include "ConfigInterface.h"
#include "VST3Instance.h"
#include "VST3Editor.h"


EffectFamilySymbol VST3EffectBase::GetFamilySymbol()
{
   return XO("VST3");
}

VST3EffectBase::~VST3EffectBase() = default;

VST3Effect::~VST3Effect()
{
   using namespace Steinberg;

   CloseUI();
}

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

int VST3Effect::ShowClientInterface(const EffectPlugin &,
   wxWindow& parent, wxDialog& dialog,
   EffectEditor *pEditor, bool forceModal) const
{
#ifdef __WXMSW__
   if(pEditor->IsGraphicalUI())
      //Not all platforms support window style change.
      //Plugins that support resizing provide their own handles,
      //which may overlap with system handle. Not all plugins
      //support free sizing (e.g. fixed steps or fixed ratio)
      dialog.SetWindowStyle(dialog.GetWindowStyle() & ~(wxRESIZE_BORDER | wxMAXIMIZE_BOX));
#endif

   if(forceModal)
      return dialog.ShowModal();

   dialog.Show();
   return 0;
}

std::shared_ptr<EffectInstance> VST3EffectBase::MakeInstance() const
{
   return std::make_shared<VST3Instance>(*this, *mModule, mEffectClassInfo);
}

std::unique_ptr<EffectEditor> VST3Effect::PopulateUI(const EffectPlugin &,
   ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess &access,
   const EffectOutputs *) const
{
   bool useGUI { true };
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
            wxT("UseGUI"),
            useGUI,
            useGUI);

   const auto vst3instance = dynamic_cast<VST3Instance*>(&instance);

   return std::make_unique<VST3Editor>(S.GetParent(),
      vst3instance->GetWrapper(), *this, GetType(), access, useGUI);
}

std::unique_ptr<EffectEditor> VST3Effect::MakeEditor(
   ShuttleGui &, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *) const
{
   //! Will not come here because Effect::PopulateUI is overridden
   assert(false);
   return nullptr;
}

bool VST3EffectBase::CanExportPresets() const
{
   return true;
}

void VST3Effect::ExportPresets(
   const EffectPlugin &, const EffectSettings& settings) const
{
   using namespace Steinberg;

   const auto path = SelectFile(FileNames::Operation::Presets,
      XO("Save VST3 Preset As:"),
      wxEmptyString,
      wxEmptyString,
      wxT(".vstpreset"),
      {
        { XO("VST3 preset file"), { wxT("vstpreset") }, true }
      },
      wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
      NULL);

   if (path.empty())
      return;
   
   auto wrapper = std::make_unique<VST3Wrapper>(*mModule, mEffectClassInfo);
   wrapper->InitializeComponents();

   auto dummy = EffectSettings { settings };
   wrapper->FetchSettings(dummy);
   wrapper->SavePresetToFile(path);
}

OptionalMessage VST3Effect::ImportPresets(
   const EffectPlugin &, EffectSettings& settings) const
{
   using namespace Steinberg;

   const auto path = SelectFile(FileNames::Operation::Presets,
      XO("Load VST3 preset:"),
      wxEmptyString,
      wxEmptyString,
      wxT(".vstpreset"),
      {
         { XO("VST3 preset file"), { wxT("vstpreset") }, true }
      },
      wxFD_OPEN | wxRESIZE_BORDER,
      nullptr
   );
   if(path.empty())
      return {};

   LoadPreset(path, settings);

   return { nullptr };
}

bool VST3EffectBase::HasOptions() const
{
   return true;
}

void VST3Effect::ShowOptions(const EffectPlugin &) const
{
   VST3OptionsDialog{ *this }.ShowModal();
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
