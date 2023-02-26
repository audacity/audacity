/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3EffectsModule.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/


#include "VST3EffectsModule.h"

#include <stdexcept>

#include <wx/stdpaths.h>
#include <wx/dir.h>
#include <wx/log.h>


#include "AudacityVst3HostApplication.h"
#include "ModuleManager.h"

#include "wxArrayStringEx.h"
#include "PluginInterface.h"
#include "PluginProvider.h"
#include "PluginProvider.h"
#include "VST3Effect.h"
#include "VST3Utils.h"
#include "VST3Wrapper.h"


DECLARE_PROVIDER_ENTRY(AudacityModule)
{
   // Create our effects module and register
   // Trust the module manager not to leak this
   return std::make_unique<VST3EffectsModule>();
}

DECLARE_BUILTIN_PROVIDER(VST3Builtin);

//Helper class used to find vst3 effects during folder traversal
class VST3PluginTraverser final : public wxDirTraverser
{
   std::function<void(const wxString&)> mOnPluginFound;
public:
   VST3PluginTraverser(std::function<void(const wxString&)> onPluginFound)
      : mOnPluginFound(std::move(onPluginFound))
   {
   }

   wxDirTraverseResult OnFile(const wxString& filename) override
   {
      if(filename.Matches("*.vst3"))
         mOnPluginFound(filename);
      return wxDIR_CONTINUE;
   }
   wxDirTraverseResult OnDir(const wxString& dirname) override
   {
      if(dirname.Matches("*.vst3"))
      {
         mOnPluginFound(dirname);
         return wxDIR_IGNORE;//do not look inside...
      }
      return wxDIR_CONTINUE;
   }
};

std::shared_ptr<VST3::Hosting::Module> VST3EffectsModule::GetModule(const wxString& path)
{
   const auto it = mModules.find(path);
   if(it != mModules.end())
   {
      if(auto lock = it->second.lock())
         return lock;
   }

   std::string moduleCreateError;
   //VST sdk provides platform-specific module loading routines as well,
   //implementation is conditionally included (see CMakeLists.txt)
   auto module = VST3::Hosting::Module::create(path.ToStdString(), moduleCreateError);
   if(!module)
      throw std::runtime_error(moduleCreateError.c_str());

   module->getFactory().setHostContext(&AudacityVst3HostApplication::Get());

   mModules[path] = module;
   return module;
}

PluginPath VST3EffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol VST3EffectsModule::GetSymbol() const
{
   return XO("VST3 Effects");
}

VendorSymbol VST3EffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString VST3EffectsModule::GetVersion() const
{
   return AUDACITY_VERSION_STRING;
}

TranslatableString VST3EffectsModule::GetDescription() const
{
   return XO("Adds the ability to use VST3 effects in Audacity.");
}

bool VST3EffectsModule::Initialize()
{
   return true;
}

void VST3EffectsModule::Terminate()
{
}

EffectFamilySymbol VST3EffectsModule::GetOptionalFamilySymbol()
{
   return VST3Effect::GetFamilySymbol();
}

const FileExtensions& VST3EffectsModule::GetFileExtensions()
{
   static const FileExtensions ext {
      { _T("vst3") }
   };
   return ext;
}

FilePath VST3EffectsModule::InstallPath()
{
#ifdef VST3_DEFAULT_INSTALL_PATH
   return FilePath { VST3_DEFAULT_INSTALL_PATH };
#else
   return {};
#endif
}

void VST3EffectsModule::AutoRegisterPlugins(PluginManagerInterface &)
{
}

PluginPaths
VST3EffectsModule::FindModulePaths(PluginManagerInterface &)
{
   //Note: The host recursively scans these folders at startup in this order (User/Global/Application).
   //https://developer.steinberg.help/display/VST/Plug-in+Locations

   FilePaths pathList;
#ifdef __WXMSW__
   // Windows specific VST3 search paths
   {
      wxString programFilesPath;
      if(wxGetEnv("programfiles", &programFilesPath))
         pathList.push_back(programFilesPath + "\\Common Files\\VST3");
   }
#elif __WXMAC__
   pathList.push_back("~/Library/Audio/Plug-ins/VST3/");
   pathList.push_back("/Library/Audio/Plug-ins/VST3/");
   pathList.push_back("/Network/Library/Audio/Plug-ins/VST3/");
#elif __WXGTK__
   pathList.push_back(wxGetHomeDir() + "/.vst3/");
   pathList.push_back("/usr/lib/vst3/");
   pathList.push_back("/usr/local/lib/vst3/");
#endif

   // bundled/app specific
   {
      auto path = wxFileName(wxStandardPaths::Get().GetExecutablePath());
#ifdef __WXGTK__
      path.AppendDir("vst3");
#else
      path.AppendDir("VST3");
#endif
      pathList.push_back(path.GetPath());
   }

   PluginPaths result;
   VST3PluginTraverser vst3PluginTraverser([&](const wxString& pluginPath){
      result.push_back(pluginPath);
   });

   for(const auto& path : pathList)
   {
      wxDir dir(path);
      if(dir.IsOpened())
         dir.Traverse(vst3PluginTraverser, wxEmptyString, wxDIR_DEFAULT);
   }
   return result;
}

unsigned VST3EffectsModule::DiscoverPluginsAtPath(const PluginPath& path, TranslatableString& errMsg,
   const RegistrationCallback& callback)
{
   try
   {
      auto module = GetModule(path);

      const auto moduleFactory = module->getFactory();
      auto nEffects { 0u };
      for(auto& classInfo : moduleFactory.classInfos())
      {
         if(classInfo.category() == kVstAudioEffectClass)
         {
            std::unique_ptr<VST3Effect> effect;
            try
            {
               effect = std::make_unique<VST3Effect>(module, classInfo);
               ++nEffects;
            }
            catch(std::exception& e)
            {
               wxLogError(
                  "Effect %s@%s cannot be loaded: %s", 
                  classInfo.name().c_str(),
                  path.c_str(),
                  e.what()
               );
            }
            catch(...)
            {
               wxLogError(
                  "Effect %s@%s cannot be loaded: unknown error",
                  classInfo.name().c_str(),
                  path.c_str()
               );
            }
            if(effect && callback)
               callback(this, effect.get());
         }
      }
      if(nEffects == 0u)
         throw std::runtime_error("no effects found");
      
      return nEffects;
   }
   catch(std::exception& e)
   {
      errMsg = XO("VST3 module error: %s").Format(e.what());
   }

   return 0u;
}

std::unique_ptr<ComponentInterface>
VST3EffectsModule::LoadPlugin(const PluginPath& pluginPath)
{
   try
   {
      wxString modulePath;
      std::string effectUIDString;

      if(!VST3Utils::ParsePluginPath(pluginPath, &modulePath, &effectUIDString))
         throw std::runtime_error("failed to parse plugin string");

      auto module = GetModule(modulePath);
      const auto pluginFactory = module->getFactory();
      for(const auto& classInfo : pluginFactory.classInfos())
      {
         if(effectUIDString == classInfo.ID().toString()) {
            auto result = std::make_unique<VST3Effect>(module, classInfo);
            return result;
         }
      }
      throw std::runtime_error("effect UID not found");
   }
   catch(std::exception& e)
   {
      wxLogError("VST3 Module was not loaded: %s", e.what());
   }
   return nullptr;
}

class VST3PluginValidator final : public PluginProvider::Validator
{
public:
   
   void Validate(ComponentInterface& component) override
   {
      if(auto vst3effect = dynamic_cast<VST3Effect*>(&component))
      {
         VST3Wrapper wrapper (
            *vst3effect->mModule,
            vst3effect->mEffectClassInfo
         );
         wrapper.InitializeComponents();
      }
      else
         throw std::runtime_error("Not a VST3Effect");
   }

};

std::unique_ptr<PluginProvider::Validator> VST3EffectsModule::MakeValidator() const
{
   return std::make_unique<VST3PluginValidator>();
}


bool VST3EffectsModule::CheckPluginExist(const PluginPath& path) const
{
   wxString modulePath;
   if(VST3Utils::ParsePluginPath(path, &modulePath, nullptr))
      return wxFileName::FileExists(modulePath) || wxFileName::DirExists(modulePath);

   return wxFileName::FileExists(path) || wxFileName::DirExists(path);
}
