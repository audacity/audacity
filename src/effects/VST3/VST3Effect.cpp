/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3Effect.h"

#include "AudacityException.h"


#include "BasicUI.h"
#include "widgets/wxWidgetsWindowPlacement.h"

#include <wx/log.h>
#include <wx/stdpaths.h>
#include <wx/regex.h>

#include <public.sdk/source/vst/hosting/hostclasses.h>

#include "internal/PlugFrame.h"
#include "internal/ConnectionProxy.h"

#include "widgets/NumericTextCtrl.h"

#include "SelectFile.h"

#include "ShuttleGui.h"

#include "VST3Utils.h"
#include "VST3ParametersWindow.h"
#include "VST3OptionsDialog.h"
#include "VST3Wrapper.h"

#ifdef __WXMSW__
#include <shlobj.h>
#endif

#include "ConfigInterface.h"
#include "VST3Instance.h"
#include "VST3Editor.h"

namespace {


wxString GetFactoryPresetsBasePath()
{
#ifdef __WXMSW__
   PWSTR commonFolderPath { nullptr };
   auto cleanup = finally([&](){ CoTaskMemFree(commonFolderPath); });
   if(SHGetKnownFolderPath(FOLDERID_ProgramData, KF_FLAG_DEFAULT , NULL, &commonFolderPath) == S_OK)
      return wxString(commonFolderPath) + "\\VST3 Presets\\";
   return {};
#elif __WXMAC__
   return wxString("Library/Audio/Presets/");
#elif __WXGTK__
   return wxString("/usr/local/share/vst3/presets/");
#endif
}

wxString GetPresetsPath(const wxString& basePath, const VST3::Hosting::ClassInfo& effectClassInfo)
{
   wxRegEx fixName(R"([\\*?/:<>|])");
   wxString companyName = wxString (effectClassInfo.vendor()).Trim();
   wxString pluginName = wxString (effectClassInfo.name()).Trim();

   fixName.ReplaceAll( &companyName, { "_" });
   fixName.ReplaceAll( &pluginName, { "_" });

   wxFileName result;
   result.SetPath(basePath);
   result.AppendDir(companyName);
   result.AppendDir(pluginName);
   auto path = result.GetPath();

   return path;
}

wxString GetFactoryPresetsPath(const VST3::Hosting::ClassInfo& effectClassInfo)
{
   return GetPresetsPath(
      GetFactoryPresetsBasePath(),
      effectClassInfo
   );
}

}

EffectFamilySymbol VST3Effect::GetFamilySymbol()
{
   return XO("VST3");
}

VST3Effect::~VST3Effect()
{
   using namespace Steinberg;

   CloseUI();
}

VST3Effect::VST3Effect(
   std::shared_ptr<VST3::Hosting::Module> module,
   VST3::Hosting::ClassInfo effectClassInfo)
      : mModule(std::move(module)), mEffectClassInfo(std::move(effectClassInfo))
{
}

PluginPath VST3Effect::GetPath() const
{
   return VST3Utils::MakePluginPathString( { mModule->getPath() }, mEffectClassInfo.ID().toString());
}

ComponentInterfaceSymbol VST3Effect::GetSymbol() const
{
   return wxString { mEffectClassInfo.name() };
}

VendorSymbol VST3Effect::GetVendor() const
{
   return wxString { mEffectClassInfo.vendor() };
}

wxString VST3Effect::GetVersion() const
{
   return mEffectClassInfo.version();
}

TranslatableString VST3Effect::GetDescription() const
{
   //i18n-hint VST3 effect description string
   return XO("SubCategories: %s").Format( mEffectClassInfo.subCategoriesString() );
}

EffectType VST3Effect::GetType() const
{
   using namespace Steinberg::Vst::PlugType;
   if(mEffectClassInfo.subCategoriesString() == kFxGenerator)
      return EffectTypeGenerate;
   const auto& cats = mEffectClassInfo.subCategories();

   if(std::find(cats.begin(), cats.end(), kFx) != cats.end())
      return EffectTypeProcess;

   return EffectTypeNone;
}

EffectFamilySymbol VST3Effect::GetFamily() const
{
   return VST3Effect::GetFamilySymbol();
}

bool VST3Effect::IsInteractive() const
{
   return true;
}

bool VST3Effect::IsDefault() const
{
   return false;
}

auto VST3Effect::RealtimeSupport() const -> RealtimeSince
{
   return GetType() == EffectTypeProcess
      ? RealtimeSince::After_3_1
      : RealtimeSince::Never;
}

bool VST3Effect::SupportsAutomation() const
{
   return true;
}

bool VST3Effect::SaveSettings(
   const EffectSettings& settings, CommandParameters& parms) const
{
   VST3Wrapper::SaveSettings(settings, parms);
   return true;
}

bool VST3Effect::LoadSettings(
   const CommandParameters& parms, EffectSettings& settings) const
{
   VST3Wrapper::LoadSettings(parms, settings);
   return true;
}

OptionalMessage VST3Effect::LoadUserPreset(
   const RegistryPath& name, EffectSettings& settings) const
{
   return VST3Wrapper::LoadUserPreset(*this, name, settings);
}

bool VST3Effect::SaveUserPreset(
   const RegistryPath& name, const EffectSettings& settings) const
{
   VST3Wrapper::SaveUserPreset(*this, name, settings);
   return true;
}

RegistryPaths VST3Effect::GetFactoryPresets() const
{
   if(!mRescanFactoryPresets)
      return mFactoryPresets;

   wxArrayString paths;
   wxDir::GetAllFiles(GetFactoryPresetsPath(mEffectClassInfo), &paths);

   RegistryPaths result;
   for(auto& path : paths)
   {
      wxFileName filename(path);
      result.push_back(filename.GetName());
   }
   mFactoryPresets = std::move(result);
   mRescanFactoryPresets = false;

   return mFactoryPresets;
}

OptionalMessage VST3Effect::LoadFactoryPreset(int id, EffectSettings& settings) const
{
   if(id >= 0 && id < mFactoryPresets.size())
   {
      auto filename = wxFileName(GetFactoryPresetsPath(mEffectClassInfo), mFactoryPresets[id] + ".vstpreset");
      if (!LoadPreset(filename.GetFullPath(), settings))
         return {};
   }
   return { nullptr };
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

std::shared_ptr<EffectInstance> VST3Effect::MakeInstance() const
{
   return std::make_shared<VST3Instance>(*this, *mModule, mEffectClassInfo.ID());
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
      vst3instance->GetWrapper(), *this, access, useGUI);
}

std::unique_ptr<EffectEditor> VST3Effect::MakeEditor(
   ShuttleGui &, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *) const
{
   //! Will not come here because Effect::PopulateUI is overridden
   assert(false);
   return nullptr;
}

bool VST3Effect::CanExportPresets() const
{
   return true;
}

void VST3Effect::ExportPresets(
   const EffectPlugin &, const EffectSettings& settings) const
{
   using namespace Steinberg;

   auto path = SelectFile(FileNames::Operation::Presets,
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

   auto fileStream = owned(Vst::FileStream::open(path.c_str(), "wb"));
   if(!fileStream)
   {
      BasicUI::ShowMessageBox(
         //i18n-hint: VST3 preset export error
         XO("Cannot open file"),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
      );
      return;
   }

   auto wrapper = std::make_unique<VST3Wrapper>(*mModule, mEffectClassInfo.ID());
   auto dummy = EffectSettings { settings };
   wrapper->FetchSettings(dummy);

   if (!wrapper->SavePreset(fileStream))
   {
      BasicUI::ShowMessageBox(
         XO("Failed to save VST3 preset to file"),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
      );
   }
}

OptionalMessage VST3Effect::ImportPresets(
   const EffectPlugin &, EffectSettings& settings) const
{
   using namespace Steinberg;

   auto path = SelectFile(FileNames::Operation::Presets,
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

   if (!LoadPreset(path, settings))
      return {};

   return { nullptr };
}

bool VST3Effect::HasOptions() const
{
   return true;
}

void VST3Effect::ShowOptions(const EffectPlugin &) const
{
   VST3OptionsDialog{ *this }.ShowModal();
}

bool VST3Effect::LoadPreset(const wxString& path, EffectSettings& settings) const
{
   using namespace Steinberg;

   auto fileStream = owned(Vst::FileStream::open(path.c_str(), "rb"));
   if(!fileStream)
   {
      BasicUI::ShowMessageBox(
         XO("Cannot open VST3 preset file %s").Format(path),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
      );
      return false;
   }

   auto wrapper = std::make_unique<VST3Wrapper>(*mModule, mEffectClassInfo.ID());

   if (!wrapper->LoadPreset(fileStream))
   {
      BasicUI::ShowMessageBox(
         XO("Unable to apply VST3 preset file %s").Format(path),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
      );
      return false;
   }

   wrapper->StoreSettings(settings);

   return true;
}

EffectSettings VST3Effect::MakeSettings() const
{
   return VST3Wrapper::MakeSettings();
}

bool VST3Effect::CopySettingsContents(const EffectSettings& src, EffectSettings& dst) const
{
   VST3Wrapper::CopySettingsContents(src, dst);
   return true;
}
