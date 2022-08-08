/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Effect.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3Effect.h"

#include "BasicUI.h"
#include "widgets/wxWidgetsWindowPlacement.h"

#include <wx/log.h>
#include <wx/stdpaths.h>
#include <wx/regex.h>

#include "SelectFile.h"
#include "ShuttleGui.h"

#include "VST3Utils.h"
#include "VST3ParametersWindow.h"
#include "VST3OptionsDialog.h"

#ifdef __WXMSW__
#include <shlobj.h>
#endif

#include "ConfigInterface.h"
#include "VST3Instance.h"
#include "VST3UIValidator.h"
#include "VST3Wrapper.h"

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

VST3Effect::~VST3Effect() = default;

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
   return RealtimeSince::Always;
}

bool VST3Effect::SupportsAutomation() const
{
   return true;
}

bool VST3Effect::SaveSettings(const EffectSettings& settings, CommandParameters& parms) const
{
   const auto& vst3settings = VST3Wrapper::GetSettings(settings);

   for(const auto& p : vst3settings.state)
      parms.Write(wxString::Format("%d", p.first), p.second);

   return true;
}

bool VST3Effect::LoadSettings(const CommandParameters& parms, EffectSettings& settings) const
{
   VST3EffectSettings temp;

   long idx;
   wxString key;
   temp.state.reserve(parms.GetNumberOfEntries());

   if(parms.GetFirstEntry(key, idx))
   {
      do
      {
         unsigned long paramId{0};
         double paramValue;
         if(key.ToULong(&paramId) && parms.Read(key, &paramValue))
            temp.state[paramId] = paramValue;
      } while(parms.GetNextEntry(key, idx));
   }
   //replicate the complete state, so that processor could
   //update its state too before the first processing pass
   temp.parameterChanges = temp.state;

   std::swap(VST3Wrapper::GetSettings(settings), temp);

   return true;
}

bool VST3Effect::LoadUserPreset(
   const RegistryPath& name, EffectSettings& settings) const
{
   if(auto lck = mCurrentDisplayEffect.lock())
      return lck->LoadUserPreset(name);
   return false;
}

bool VST3Effect::SaveUserPreset(
   const RegistryPath& name, const EffectSettings& settings) const
{
   if(auto lck = mCurrentDisplayEffect.lock())
      return lck->SaveUserPreset(name);
   return false;
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

bool VST3Effect::LoadFactoryPreset(int id, EffectSettings& settings) const
{
   if(id >= 0 && id < mFactoryPresets.size())
   {
      auto filename = wxFileName(GetFactoryPresetsPath(mEffectClassInfo), mFactoryPresets[id] + ".vstpreset");
      // To do: externalize state so const_cast isn't needed
      return const_cast<VST3Effect*>(this)->LoadPreset(filename.GetFullPath(), settings);
   }
   return true;
}

int VST3Effect::ShowClientInterface(wxWindow& parent, wxDialog& dialog,
   EffectUIValidator *, bool forceModal)
{
   if(!IsGraphicalUI())
   {
      //Restrict resize of the "plain" dialog
      dialog.SetMaxSize(dialog.GetSize());
      dialog.SetMinSize(dialog.GetSize());
   }
   if(forceModal)
      return dialog.ShowModal();

   dialog.Show();
   return 0;
}

bool VST3Effect::CloseUI()
{
   mParent = nullptr;
   mCurrentDisplayEffect.reset();
   return true;
}

std::shared_ptr<EffectInstance> VST3Effect::MakeInstance() const
{
   return std::make_shared<VST3Instance>(*this, *mModule, mEffectClassInfo.ID());
}

std::unique_ptr<EffectUIValidator> VST3Effect::PopulateUI(ShuttleGui& S,
   EffectInstance &instance, EffectSettingsAccess &access)
{
   bool useGUI { true };
   GetConfig(*this, PluginSettings::Shared, wxT("Options"),
            wxT("UseGUI"),
            useGUI,
            useGUI);

   auto vst3instance = dynamic_cast<VST3Instance*>(&instance);
   mCurrentDisplayEffect = std::dynamic_pointer_cast<VST3Instance>(vst3instance->shared_from_this());
   mParent = S.GetParent();

   return std::make_unique<VST3UIValidator>(mParent, vst3instance->GetWrapper(), *this, access, useGUI);
}

bool VST3Effect::CanExportPresets()
{
   return true;
}

void VST3Effect::ExportPresets(const EffectSettings& settings) const
{
   using namespace Steinberg;

   auto instance = mCurrentDisplayEffect.lock();
   if(!instance)
      return;

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

   auto dialogPlacement = wxWidgetsWindowPlacement { mParent };

   auto fileStream = owned(Vst::FileStream::open(path.c_str(), "wb"));
   if(!fileStream)
   {
      BasicUI::ShowMessageBox(
         //i18n-hint: VST3 preset export error
         XO("Cannot open file"),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
      return;
   }

   if (!instance->GetWrapper().SavePreset(fileStream))
   {
      BasicUI::ShowMessageBox(
         XO("Failed to save VST3 preset to file"),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
   }
}

void VST3Effect::ImportPresets(EffectSettings& settings)
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
      return;

   LoadPreset(path, settings);
}

bool VST3Effect::HasOptions()
{
   return true;
}

void VST3Effect::ShowOptions()
{
   VST3OptionsDialog dlg(mParent, *this);
   dlg.ShowModal();
   if(auto lck = mCurrentDisplayEffect.lock())
      lck->ReloadUserOptions();
}

bool VST3Effect::LoadPreset(const wxString& path, EffectSettings& settings)
{
   using namespace Steinberg;

   auto instance = mCurrentDisplayEffect.lock();
   if(!instance)
      return false;

   auto dialogPlacement = wxWidgetsWindowPlacement { mParent };

   auto fileStream = owned(Vst::FileStream::open(path.c_str(), "rb"));
   if(!fileStream)
   {
      BasicUI::ShowMessageBox(
         XO("Cannot open VST3 preset file %s").Format(path),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
      return false;
   }


   if (!instance->GetWrapper().LoadPreset(fileStream))
   {
      BasicUI::ShowMessageBox(
         XO("Unable to apply VST3 preset file %s").Format(path),
         BasicUI::MessageBoxOptions()
            .Caption(XO("Error"))
            .Parent(&dialogPlacement)
      );
      return false;
   }

   return true;
}

EffectSettings VST3Effect::MakeSettings() const
{
   return VST3Wrapper::MakeSettings();
}
