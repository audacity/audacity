/**********************************************************************

  Audacity: A Digital Audio Editor

  PluginManager.cpp

  Leland Lucius

*******************************************************************//*!

\file PluginManager.cpp
\brief

************************************************************************//**
\class PluginManager
\brief PluginManager maintains a list of all plug ins.  That covers modules,
effects, generators, analysis-effects, commands.  It also has functions
for shared and private configs - which need to move out.
*****************************************************************************/


#include "PluginManager.h"



#include <algorithm>

#include <wx/log.h>
#include <wx/tokenzr.h>

#include "ModuleInterface.h"

#include "AudacityFileConfig.h"
#include "Internat.h" // for macro XO
#include "FileNames.h"
#include "MemoryX.h"
#include "ModuleManager.h"
#include "PlatformCompatibility.h"
#include "widgets/AudacityMessageBox.h"

///////////////////////////////////////////////////////////////////////////////
//
// Plugindescriptor
//
///////////////////////////////////////////////////////////////////////////////

PluginDescriptor::PluginDescriptor()
{
   mPluginType = PluginTypeNone;
   mEnabled = false;
   mValid = false;
   mInstance = nullptr;

   mEffectType = EffectTypeNone;
   mEffectInteractive = false;
   mEffectDefault = false;
   mEffectLegacy = false;
   mEffectRealtime = false;
   mEffectAutomatable = false;
}

PluginDescriptor::~PluginDescriptor()
{
}

PluginDescriptor &PluginDescriptor::operator =(PluginDescriptor &&) = default;

bool PluginDescriptor::IsInstantiated() const
{
   return mInstance != nullptr;
}

ComponentInterface *PluginDescriptor::GetInstance()
{
   if (!mInstance)
   {
      if (GetPluginType() == PluginTypeModule)
         mInstance = ModuleManager::Get().CreateProviderInstance(GetID(), GetPath());
      else
      {
         muInstance = ModuleManager::Get().CreateInstance(GetProviderID(), GetPath());
         mInstance = muInstance.get();
      }
   }

   return mInstance;
}

void PluginDescriptor::SetInstance(std::unique_ptr<ComponentInterface> instance)
{
   muInstance = std::move(instance);
   mInstance = muInstance.get();
}

PluginType PluginDescriptor::GetPluginType() const
{
   return mPluginType;
}

const PluginID & PluginDescriptor::GetID() const
{
   return mID;
}

const PluginID & PluginDescriptor::GetProviderID() const
{
   return mProviderID;
}

const PluginPath & PluginDescriptor::GetPath() const
{
   return mPath;
}

const ComponentInterfaceSymbol & PluginDescriptor::GetSymbol() const
{
   return mSymbol;
}

wxString PluginDescriptor::GetUntranslatedVersion() const
{
   return mVersion;
}

wxString PluginDescriptor::GetVendor() const
{
   return mVendor;
}

bool PluginDescriptor::IsEnabled() const
{
   return mEnabled;
}

bool PluginDescriptor::IsValid() const
{
   return mValid;
}

void PluginDescriptor::SetPluginType(PluginType type)
{
   mPluginType = type;
}

void PluginDescriptor::SetID(const PluginID & ID)
{
   mID = ID;
}

void PluginDescriptor::SetProviderID(const PluginID & providerID)
{
   mProviderID = providerID;
}

void PluginDescriptor::SetPath(const PluginPath & path)
{
   mPath = path;
}

void PluginDescriptor::SetSymbol(const ComponentInterfaceSymbol & symbol)
{
   mSymbol = symbol;
}

void PluginDescriptor::SetVersion(const wxString & version)
{
   mVersion = version;
}

void PluginDescriptor::SetVendor(const wxString & vendor)
{
   mVendor = vendor;
}

void PluginDescriptor::SetEnabled(bool enable)
{
   mEnabled = enable;
}

void PluginDescriptor::SetValid(bool valid)
{
   mValid = valid;
}

// Effects

wxString PluginDescriptor::GetEffectFamily() const
{
   return mEffectFamily;
}

EffectType PluginDescriptor::GetEffectType() const
{
   return mEffectType;
}

bool PluginDescriptor::IsEffectInteractive() const
{
   return mEffectInteractive;
}

bool PluginDescriptor::IsEffectDefault() const
{
   return mEffectDefault;
}

bool PluginDescriptor::IsEffectLegacy() const
{
   return mEffectLegacy;
}

bool PluginDescriptor::IsEffectRealtime() const
{
   return mEffectRealtime;
}

bool PluginDescriptor::IsEffectAutomatable() const
{
   return mEffectAutomatable;
}

void PluginDescriptor::SetEffectFamily(const wxString & family)
{
   mEffectFamily = family;
}

void PluginDescriptor::SetEffectType(EffectType type)
{
   mEffectType = type;
}

void PluginDescriptor::SetEffectInteractive(bool interactive)
{
   mEffectInteractive = interactive;
}

void PluginDescriptor::SetEffectDefault(bool dflt)
{
   mEffectDefault = dflt;
}

void PluginDescriptor::SetEffectLegacy(bool legacy)
{
   mEffectLegacy = legacy;
}

void PluginDescriptor::SetEffectRealtime(bool realtime)
{
   mEffectRealtime = realtime;
}

void PluginDescriptor::SetEffectAutomatable(bool automatable)
{
   mEffectAutomatable = automatable;
}

// Importer

const wxString & PluginDescriptor::GetImporterIdentifier() const
{
   return mImporterIdentifier;
}

void PluginDescriptor::SetImporterIdentifier(const wxString & identifier)
{
   mImporterIdentifier = identifier;
}

const FileExtensions & PluginDescriptor::GetImporterExtensions()
   const
{
   return mImporterExtensions;
}

void PluginDescriptor::SetImporterExtensions( FileExtensions extensions )
{
   mImporterExtensions = std::move( extensions );
}

///////////////////////////////////////////////////////////////////////////////
//
// PluginManager
//
///////////////////////////////////////////////////////////////////////////////

// Registry has the list of plug ins
#define REGVERKEY wxString(wxT("/pluginregistryversion"))
#define REGVERCUR wxString(wxT("1.1"))
#define REGROOT wxString(wxT("/pluginregistry/"))

// Settings has the values of the plug in settings.
#define SETVERKEY wxString(wxT("/pluginsettingsversion"))
#define SETVERCUR wxString(wxT("1.0"))
#define SETROOT wxString(wxT("/pluginsettings/"))

#define KEY_ID                         wxT("ID")
#define KEY_PATH                       wxT("Path")
#define KEY_SYMBOL                     wxT("Symbol")
#define KEY_NAME                       wxT("Name")
#define KEY_VENDOR                     wxT("Vendor")
#define KEY_VERSION                    wxT("Version")
#define KEY_DESCRIPTION                wxT("Description")
#define KEY_LASTUPDATED                wxT("LastUpdated")
#define KEY_ENABLED                    wxT("Enabled")
#define KEY_VALID                      wxT("Valid")
#define KEY_PROVIDERID                 wxT("ProviderID")
#define KEY_EFFECTTYPE                 wxT("EffectType")
#define KEY_EFFECTFAMILY               wxT("EffectFamily")
#define KEY_EFFECTDEFAULT              wxT("EffectDefault")
#define KEY_EFFECTINTERACTIVE          wxT("EffectInteractive")
#define KEY_EFFECTREALTIME             wxT("EffectRealtime")
#define KEY_EFFECTAUTOMATABLE          wxT("EffectAutomatable")
#define KEY_EFFECTTYPE_NONE            wxT("None")
#define KEY_EFFECTTYPE_ANALYZE         wxT("Analyze")
#define KEY_EFFECTTYPE_GENERATE        wxT("Generate")
#define KEY_EFFECTTYPE_PROCESS         wxT("Process")
#define KEY_EFFECTTYPE_TOOL            wxT("Tool")
#define KEY_EFFECTTYPE_HIDDEN          wxT("Hidden")
#define KEY_IMPORTERIDENT              wxT("ImporterIdent")
//#define KEY_IMPORTERFILTER             wxT("ImporterFilter")
#define KEY_IMPORTEREXTENSIONS         wxT("ImporterExtensions")

// ============================================================================
//
// PluginManagerInterface implementation
//
// ============================================================================

const PluginID &PluginManagerInterface::DefaultRegistrationCallback(
   ModuleInterface *provider, ComponentInterface *pInterface )
{
   EffectDefinitionInterface * pEInterface = dynamic_cast<EffectDefinitionInterface*>(pInterface);
   if( pEInterface )
      return PluginManager::Get().RegisterPlugin(provider, pEInterface, PluginTypeEffect);
   ComponentInterface * pCInterface = dynamic_cast<ComponentInterface*>(pInterface);
   if( pCInterface )
      return PluginManager::Get().RegisterPlugin(provider, pCInterface);
   static wxString empty;
   return empty;
}

const PluginID &PluginManagerInterface::AudacityCommandRegistrationCallback(
   ModuleInterface *provider, ComponentInterface *pInterface )
{
   ComponentInterface * pCInterface = dynamic_cast<ComponentInterface*>(pInterface);
   if( pCInterface )
      return PluginManager::Get().RegisterPlugin(provider, pCInterface);
   static wxString empty;
   return empty;
}

RegistryPath PluginManager::GetPluginEnabledSetting( const PluginID &ID ) const
{
   auto pPlugin = GetPlugin( ID );
   if ( pPlugin )
      return GetPluginEnabledSetting( *pPlugin );
   return {};
}

RegistryPath PluginManager::GetPluginEnabledSetting(
   const PluginDescriptor &desc ) const
{
   switch ( desc.GetPluginType() ) {
      case PluginTypeModule: {
         // Retrieve optional family symbol that was recorded in
         // RegisterPlugin() for the module
         auto family = desc.GetEffectFamily();
         if ( family.empty() ) // as for built-in effect and command modules
            return {};
         else
            return wxT('/') + family + wxT("/Enable");
      }
      case PluginTypeEffect:
         // do NOT use GetEffectFamily() for this descriptor, but instead,
         // delegate to the plugin descriptor of the provider, which may
         // be different (may be empty)
         return GetPluginEnabledSetting( desc.GetProviderID() );
      default:
         return {};
   }
}

bool PluginManager::IsPluginRegistered(
   const PluginPath &path, const TranslatableString *pName)
{
   for (auto &pair : mPlugins) {
      if (auto &descriptor = pair.second; descriptor.GetPath() == path) {
         if (pName)
            descriptor.SetSymbol(
               { descriptor.GetSymbol().Internal(), *pName });
         return true;
      }
   }
   return false;
}

const PluginID & PluginManager::RegisterPlugin(ModuleInterface *module)
{
   PluginDescriptor & plug = CreatePlugin(GetID(module), module, PluginTypeModule);
   plug.SetEffectFamily(module->GetOptionalFamilySymbol().Internal());

   plug.SetEnabled(true);
   plug.SetValid(true);

   return plug.GetID();
}

const PluginID & PluginManager::RegisterPlugin(ModuleInterface *provider, ComponentInterface *command)
{
   PluginDescriptor & plug = CreatePlugin(GetID(command), command, (PluginType)PluginTypeAudacityCommand);

   plug.SetProviderID(PluginManager::GetID(provider));

   plug.SetEnabled(true);
   plug.SetValid(true);

   return plug.GetID();
}

const PluginID & PluginManager::RegisterPlugin(ModuleInterface *provider, EffectDefinitionInterface *effect, int type)
{
   PluginDescriptor & plug = CreatePlugin(GetID(effect), effect, (PluginType)type);

   plug.SetProviderID(PluginManager::GetID(provider));

   plug.SetEffectType(effect->GetClassification());
   plug.SetEffectFamily(effect->GetFamily().Internal());
   plug.SetEffectInteractive(effect->IsInteractive());
   plug.SetEffectDefault(effect->IsDefault());
   plug.SetEffectRealtime(effect->SupportsRealtime());
   plug.SetEffectAutomatable(effect->SupportsAutomation());

   plug.SetEnabled(true);
   plug.SetValid(true);

   return plug.GetID();
}

void PluginManager::FindFilesInPathList(const wxString & pattern,
                                        const FilePaths & pathList,
                                        FilePaths & files,
                                        bool directories)
{
   
   wxLogNull nolog;

   // Why bother...
   if (pattern.empty())
   {
      return;
   }

   // TODO:  We REALLY need to figure out the "Audacity" plug-in path(s)

   FilePaths paths;

   // Add the "per-user" plug-ins directory
   {
      const wxFileName &ff = FileNames::PlugInDir();
      paths.push_back(ff.GetFullPath());
   }
 
   // Add the "Audacity" plug-ins directory
   wxFileName ff = PlatformCompatibility::GetExecutablePath();
#if defined(__WXMAC__)
   // Path ends for example in "Audacity.app/Contents/MacOSX"
   //ff.RemoveLastDir();
   //ff.RemoveLastDir();
   // just remove the MacOSX part.
   ff.RemoveLastDir();
#endif
   ff.AppendDir(wxT("plug-ins"));
   paths.push_back(ff.GetPath());

   // Weed out duplicates
   for (const auto &filePath : pathList)
   {
      ff = filePath;
      const wxString path{ ff.GetFullPath() };
      if (paths.Index(path, wxFileName::IsCaseSensitive()) == wxNOT_FOUND)
      {
         paths.push_back(path);
      }
   }

   // Find all matching files in each path
   for (size_t i = 0, cnt = paths.size(); i < cnt; i++)
   {
      ff = paths[i] + wxFILE_SEP_PATH + pattern;
      wxDir::GetAllFiles(ff.GetPath(), &files, ff.GetFullName(), directories ? wxDIR_DEFAULT : wxDIR_FILES);
   }

   return;
}

bool PluginManager::HasSharedConfigGroup(const PluginID & ID, const RegistryPath & group)
{
   return HasGroup(SharedGroup(ID, group));
}

bool PluginManager::GetSharedConfigSubgroups(const PluginID & ID, const RegistryPath & group, RegistryPaths & subgroups)
{
   return GetSubgroups(SharedGroup(ID, group), subgroups);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, wxString & value, const wxString & defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, int & value, int defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, bool & value, bool defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, float & value, float defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::GetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, double & value, double defval)
{
   return GetConfig(SharedKey(ID, group, key), value, defval);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const wxString & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const int & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const bool & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const float & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::SetSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const double & value)
{
   return SetConfig(SharedKey(ID, group, key), value);
}

bool PluginManager::RemoveSharedConfigSubgroup(const PluginID & ID, const RegistryPath & group)
{
   bool result = GetSettings()->DeleteGroup(SharedGroup(ID, group));
   if (result)
   {
      GetSettings()->Flush();
   }

   return result;
}

bool PluginManager::RemoveSharedConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key)
{
   bool result = GetSettings()->DeleteEntry(SharedKey(ID, group, key));
   if (result)
   {
      GetSettings()->Flush();
   }

   return result;
}

bool PluginManager::HasPrivateConfigGroup(const PluginID & ID, const RegistryPath & group)
{
   return HasGroup(PrivateGroup(ID, group));
}

bool PluginManager::GetPrivateConfigSubgroups(const PluginID & ID, const RegistryPath & group, RegistryPaths & subgroups)
{
   return GetSubgroups(PrivateGroup(ID, group), subgroups);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, wxString & value, const wxString & defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, int & value, int defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, bool & value, bool defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, float & value, float defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::GetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, double & value, double defval)
{
   return GetConfig(PrivateKey(ID, group, key), value, defval);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const wxString & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const int & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const bool & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const float & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::SetPrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key, const double & value)
{
   return SetConfig(PrivateKey(ID, group, key), value);
}

bool PluginManager::RemovePrivateConfigSubgroup(const PluginID & ID, const RegistryPath & group)
{
   bool result = GetSettings()->DeleteGroup(PrivateGroup(ID, group));
   if (result)
   {
      GetSettings()->Flush();
   }

   return result;
}

bool PluginManager::RemovePrivateConfig(const PluginID & ID, const RegistryPath & group, const RegistryPath & key)
{
   bool result = GetSettings()->DeleteEntry(PrivateKey(ID, group, key));
   if (result)
   {
      GetSettings()->Flush();
   }

   return result;
}

// ============================================================================
//
// PluginManager
//
// ============================================================================

// The one and only PluginManager
std::unique_ptr<PluginManager> PluginManager::mInstance{};

// ----------------------------------------------------------------------------
// Creation/Destruction
// ----------------------------------------------------------------------------

PluginManager::PluginManager()
{
   mSettings = NULL;
}

PluginManager::~PluginManager()
{
   // Ensure termination (harmless if already done)
   Terminate();
}

// ----------------------------------------------------------------------------
// PluginManager implementation
// ----------------------------------------------------------------------------

// ============================================================================
//
// Return reference to singleton
//
// (Thread-safe...no active threading during construction or after destruction)
// ============================================================================

PluginManager & PluginManager::Get()
{
   if (!mInstance)
   {
      mInstance.reset(safenew PluginManager);
   }

   return *mInstance;
}

void PluginManager::Initialize()
{
   // Always load the registry first
   Load();

   // And force load of setting to verify it's accessible
   GetSettings();

   auto &mm = ModuleManager::Get();
   mm.DiscoverProviders();
   for (const auto &[id, module] : mm.Providers()) {
      RegisterPlugin(module.get());
      // Allow the module to auto-register children
      module->AutoRegisterPlugins(*this);
   }

   // And finally check for updates
#ifndef EXPERIMENTAL_EFFECT_MANAGEMENT
   CheckForUpdates();
#else
   const bool kFast = true;
   CheckForUpdates( kFast );
#endif
}

void PluginManager::Terminate()
{
   // Get rid of all non-module plugins first
   PluginMap::iterator iter = mPlugins.begin();
   while (iter != mPlugins.end())
   {
      PluginDescriptor & plug = iter->second;
      if (plug.GetPluginType() == PluginTypeEffect)
      {
         mPlugins.erase(iter++);
         continue;
      }

      ++iter;
   }

   // Now get rid of the modules
   iter = mPlugins.begin();
   while (iter != mPlugins.end())
   {
      mPlugins.erase(iter++);
   }
}

bool PluginManager::DropFile(const wxString &fileName)
{
   auto &mm = ModuleManager::Get();
   const wxFileName src{ fileName };

   for (auto &plug : PluginsOfType(PluginTypeModule)) {
      auto module = static_cast<ModuleInterface *>
         (mm.CreateProviderInstance(plug.GetID(), plug.GetPath()));
      if (! module)
         continue;

      const auto &ff = module->InstallPath();
      const auto &extensions = module->GetFileExtensions();
      if ( !ff.empty() &&
          extensions.Index(src.GetExt(), false) != wxNOT_FOUND ) {
         TranslatableString errMsg;
         // Do dry-run test of the file format
         unsigned nPlugIns =
            module->DiscoverPluginsAtPath(fileName, errMsg, {});
         if (nPlugIns) {
            // File contents are good for this module, so check no others.
            // All branches of this block return true, even in case of
            // failure for other reasons, to signal that other drag-and-drop
            // actions should not be tried.

            // Find path to copy it
            wxFileName dst;
            dst.AssignDir( ff );
            dst.SetFullName( src.GetFullName() );
            if ( dst.Exists() ) {
               // Query whether to overwrite
               bool overwrite = (wxYES == ::AudacityMessageBox(
                  XO("Overwrite the plug-in file %s?")
                     .Format( dst.GetFullPath() ),
                  XO("Plug-in already exists"),
                  wxYES_NO ) );
               if ( !overwrite )
                  return true;
            }

            // Move the file or subtree
            bool copied = false;
            auto dstPath = dst.GetFullPath();
            if ( src.FileExists() )
               // A simple one-file plug-in
               copied = FileNames::DoCopyFile(
                  src.GetFullPath(), dstPath, true );
            else {
               // A sub-folder
               // such as for some VST packages
               // Recursive copy needed -- to do
               return true;
            }

            if (!copied) {
               ::AudacityMessageBox(
                  XO("Plug-in file is in use. Failed to overwrite") );
               return true;
            }

            // Register for real
            std::vector<PluginID> ids;
            std::vector<wxString> names;
            nPlugIns = module->DiscoverPluginsAtPath(dstPath, errMsg,
               [&](ModuleInterface *provider, ComponentInterface *ident)
                                                     -> const PluginID& {
                  // Register as by default, but also collecting the PluginIDs
                  // and names
                  auto &id = PluginManagerInterface::DefaultRegistrationCallback(
                        provider, ident);
                  ids.push_back(id);
                  names.push_back( ident->GetSymbol().Translation() );
                  return id;
               });
            if ( ! nPlugIns ) {
               // Unlikely after the dry run succeeded
               ::AudacityMessageBox(
                  XO("Failed to register:\n%s").Format( errMsg ) );
               return true;
            }

            // Ask whether to enable the plug-ins
            if (auto nIds = ids.size()) {
               auto message = XPC(
               /* i18n-hint A plug-in is an optional added program for a sound
                effect, or generator, or analyzer */
                  "Enable this plug-in?\n",
                  "Enable these plug-ins?\n",
                  0,
                  "plug-ins"
               )( nIds );
               for (const auto &name : names)
                  message.Join( Verbatim( name ), wxT("\n") );
               bool enable = (wxYES == ::AudacityMessageBox(
                  message,
                  XO("Enable new plug-ins"),
                  wxYES_NO ) );
               for (const auto &id : ids)
                  mPlugins[id].SetEnabled(enable);
               // Make changes to enabled status persist:
               this->Save();
            }

            return true;
         }
      }
   }
   
   return false;
}

void PluginManager::Load()
{
   // Create/Open the registry
   auto pRegistry = AudacityFileConfig::Create(
      {}, {}, FileNames::PluginRegistry());
   auto &registry = *pRegistry;

   // If this group doesn't exist then we have something that's not a registry.
   // We should probably warn the user, but it's pretty unlikely that this will happen.
   if (!registry.HasGroup(REGROOT))
   {
      // Must start over
      // This DeleteAll affects pluginregistry.cfg only, not audacity.cfg
      // That is, the memory of on/off states of effect (and generator,
      // analyzer, and tool) plug-ins
      registry.DeleteAll();
      registry.Flush();
      return;
   }

   // Check for a registry version that we can understand
   // TODO: Should also check for a registry file that is newer than
   // what we can understand.
   wxString regver = registry.Read(REGVERKEY);
   if (regver < REGVERCUR )
   {
      // Conversion code here, for when registry version changes.

      // We iterate through the effects, possibly updating their info.
      wxString groupName;
      long groupIndex;
      wxString group = GetPluginTypeString(PluginTypeEffect);
      wxString cfgPath = REGROOT + group + wxCONFIG_PATH_SEPARATOR;
      wxArrayString groupsToDelete;

      registry.SetPath(cfgPath);
      for (bool cont = registry.GetFirstGroup(groupName, groupIndex);
         cont;
         registry.SetPath(cfgPath),
         cont = registry.GetNextGroup(groupName, groupIndex))
      {
         registry.SetPath(groupName);
         wxString effectSymbol = registry.Read(KEY_SYMBOL, "");
         wxString effectVersion = registry.Read(KEY_VERSION, "");


         // For 2.3.0 the plugins we distribute have moved around.
         // So we upped the registry version number to 1.1.
         // These particular config edits were originally written to fix Bug 1914.
         if (regver <= "1.0") {
            // Nyquist prompt is a built-in that has moved to the tools menu.
            if (effectSymbol == NYQUIST_PROMPT_ID) {
               registry.Write(KEY_EFFECTTYPE, "Tool");
            // Old version of SDE was in Analyze menu.  Now it is in Tools.
            // We don't want both the old and the new.
            } else if ((effectSymbol == "Sample Data Export") && (effectVersion == "n/a")) {
               groupsToDelete.push_back(cfgPath + groupName);
            // Old version of SDI was in Generate menu.  Now it is in Tools.
            } else if ((effectSymbol == "Sample Data Import") && (effectVersion == "n/a")) {
               groupsToDelete.push_back(cfgPath + groupName);
            }
         }

      }
      // Doing the deletion within the search loop risked skipping some items,
      // hence the delayed delete.
      for (unsigned int i = 0; i < groupsToDelete.size(); i++) {
         registry.DeleteGroup(groupsToDelete[i]);
      }
      registry.SetPath("");
      registry.Write(REGVERKEY, REGVERCUR);
      // Updates done.  Make sure we read the updated data later.
      registry.Flush();
   }

   // Load all provider plugins first
   LoadGroup(&registry, PluginTypeModule);

   // Now the rest
   LoadGroup(&registry, PluginTypeEffect);
   LoadGroup(&registry, PluginTypeAudacityCommand );
   LoadGroup(&registry, PluginTypeExporter);
   LoadGroup(&registry, PluginTypeImporter);

   LoadGroup(&registry, PluginTypeStub);
   return;
}

void PluginManager::LoadGroup(FileConfig *pRegistry, PluginType type)
{
#ifdef __WXMAC__
   // Bug 1590: On Mac, we should purge the registry of Nyquist plug-ins
   // bundled with other versions of Audacity, assuming both versions
   // were properly installed in /Applications (or whatever it is called in
   // your locale)

   const auto fullExePath = PlatformCompatibility::GetExecutablePath();

   // Strip rightmost path components up to *.app
   wxFileName exeFn{ fullExePath };
   exeFn.SetEmptyExt();
   exeFn.SetName(wxString{});
   while(exeFn.GetDirCount() && !exeFn.GetDirs().back().EndsWith(".app"))
      exeFn.RemoveLastDir();

   const auto goodPath = exeFn.GetPath();

   if(exeFn.GetDirCount())
      exeFn.RemoveLastDir();
   const auto possiblyBadPath = exeFn.GetPath();

   auto AcceptPath = [&](const wxString &path) {
      if (!path.StartsWith(possiblyBadPath))
         // Assume it's not under /Applications
         return true;
      if (path.StartsWith(goodPath))
         // It's bundled with this executable
         return true;
      return false;
   };
#else
   auto AcceptPath = [](const wxString&){ return true; };
#endif

   wxString strVal;
   bool boolVal;
   wxString groupName;
   long groupIndex;
   wxString group = GetPluginTypeString(type);
   wxString cfgPath = REGROOT + group + wxCONFIG_PATH_SEPARATOR;

   pRegistry->SetPath(cfgPath);
   for (bool cont = pRegistry->GetFirstGroup(groupName, groupIndex);
        cont;
        pRegistry->SetPath(cfgPath),
        cont = pRegistry->GetNextGroup(groupName, groupIndex))
   {
      PluginDescriptor plug;

      pRegistry->SetPath(groupName);

      groupName = ConvertID(groupName);

      // Bypass group if the ID is already in use
      if (mPlugins.count(groupName))
         continue;

      // Set the ID and type
      plug.SetID(groupName);
      plug.SetPluginType(type);

      // Get the provider ID and bypass group if not found
      if (!pRegistry->Read(KEY_PROVIDERID, &strVal, wxEmptyString))
      {
         // Bypass group if the provider isn't valid
         if (!strVal.empty() && !mPlugins.count(strVal))
            continue;
      }
      plug.SetProviderID(PluginID(strVal));

      // Get the path (optional)
      pRegistry->Read(KEY_PATH, &strVal, wxEmptyString);
      if (!AcceptPath(strVal))
         // Ignore the obsolete path in the config file, during session,
         // but don't remove it from the file.  Maybe you really want to
         // switch back to the other version of Audacity and lose nothing.
         continue;
      plug.SetPath(strVal);

      /*
       // PRL: Ignore names  written in configs before 2.3.0!
       // use Internal string only!  Let the present version of Audacity map
       // that to a user-visible string.
      // Get the name and bypass group if not found
      if (!pRegistry->Read(KEY_NAME, &strVal))
      {
         continue;
      }
      plug.SetName(strVal);
       */

      // Get the symbol...Audacity 2.3.0 or later requires it
      // bypass group if not found
      // Note, KEY_SYMBOL started getting written to config files in 2.1.0.
      // KEY_NAME (now ignored) was written before that, but only for VST
      // effects.
      if (!pRegistry->Read(KEY_SYMBOL, &strVal))
         continue;

      // Related to Bug2778: config file only remembered an internal name,
      // so this symbol may not contain the correct TranslatableString.
      // See calls to IsPluginRegistered which can correct that.
      plug.SetSymbol(strVal);

      // Get the version and bypass group if not found
      if (!pRegistry->Read(KEY_VERSION, &strVal))
      {
         continue;
      }
      plug.SetVersion(strVal);

      // Get the vendor and bypass group if not found
      if (!pRegistry->Read(KEY_VENDOR, &strVal))
      {
         continue;
      }
      plug.SetVendor( strVal );

#if 0
      // This was done before version 2.2.2, but the value was not really used
      // But absence of a value will cause early versions to skip the group
      // Therefore we still write a blank to keep pluginregistry.cfg
      // backwards-compatible

      // Get the description and bypass group if not found
      if (!pRegistry->Read(KEY_DESCRIPTION, &strVal))
      {
         continue;
      }
#endif

      // Is it enabled...default to no if not found
      pRegistry->Read(KEY_ENABLED, &boolVal, false);
      plug.SetEnabled(boolVal);

      // Is it valid...default to no if not found
      pRegistry->Read(KEY_VALID, &boolVal, false);
      plug.SetValid(boolVal);

      switch (type)
      {
         case PluginTypeModule:
         {
            // Nothing to do here yet
         }
         break;

         case PluginTypeEffect:
         {
            // Get the effect type and bypass group if not found
            if (!pRegistry->Read(KEY_EFFECTTYPE, &strVal))
               continue;

            if (strVal == KEY_EFFECTTYPE_NONE)
               plug.SetEffectType(EffectTypeNone);
            else if (strVal == KEY_EFFECTTYPE_ANALYZE)
               plug.SetEffectType(EffectTypeAnalyze);
            else if (strVal == KEY_EFFECTTYPE_GENERATE)
               plug.SetEffectType(EffectTypeGenerate);
            else if (strVal == KEY_EFFECTTYPE_PROCESS)
               plug.SetEffectType(EffectTypeProcess);
            else if (strVal == KEY_EFFECTTYPE_TOOL)
               plug.SetEffectType(EffectTypeTool);
            else if (strVal == KEY_EFFECTTYPE_HIDDEN)
               plug.SetEffectType(EffectTypeHidden);
            else
               continue;

            // Get the effect family and bypass group if not found
            if (!pRegistry->Read(KEY_EFFECTFAMILY, &strVal))
            {
               continue;
            }
            plug.SetEffectFamily(strVal);

            // Is it a default (above the line) effect and bypass group if not found
            if (!pRegistry->Read(KEY_EFFECTDEFAULT, &boolVal))
            {
               continue;
            }
            plug.SetEffectDefault(boolVal);

            // Is it an interactive effect and bypass group if not found
            if (!pRegistry->Read(KEY_EFFECTINTERACTIVE, &boolVal))
            {
               continue;
            }
            plug.SetEffectInteractive(boolVal);

            // Is it a realtime capable effect and bypass group if not found
            if (!pRegistry->Read(KEY_EFFECTREALTIME, &boolVal))
            {
               continue;
            }
            plug.SetEffectRealtime(boolVal);

            // Does the effect support automation...bypass group if not found
            if (!pRegistry->Read(KEY_EFFECTAUTOMATABLE, &boolVal))
            {
               continue;
            }
            plug.SetEffectAutomatable(boolVal);
         }
         break;

         case PluginTypeImporter:
         {
            // Get the importer identifier and bypass group if not found
            if (!pRegistry->Read(KEY_IMPORTERIDENT, &strVal))
            {
               continue;
            }
            plug.SetImporterIdentifier(strVal);

            // Get the importer extensions and bypass group if not found
            if (!pRegistry->Read(KEY_IMPORTEREXTENSIONS, &strVal))
            {
               continue;
            }
            FileExtensions extensions;
            wxStringTokenizer tkr(strVal, wxT(":"));
            while (tkr.HasMoreTokens())
            {
               extensions.push_back(tkr.GetNextToken());
            }
            plug.SetImporterExtensions(extensions);
         }
         break;

         case PluginTypeStub:
         {
            // Nothing additional for stubs
         }
         break;

         // Not used by 2.1.1 or greater and should be removed after a few releases past 2.1.0.
         case PluginTypeNone:
         {
            // Used for stub groups
         }
         break;

         default:
         {
            continue;
         }
      }

      // Everything checked out...accept the plugin
      mPlugins[groupName] = std::move(plug);
   }

   return;
}

void PluginManager::Save()
{
   // Create/Open the registry
   auto pRegistry = AudacityFileConfig::Create(
      {}, {}, FileNames::PluginRegistry());
   auto &registry = *pRegistry;

   // Clear pluginregistry.cfg (not audacity.cfg)
   registry.DeleteAll();

   // Write the version string
   registry.Write(REGVERKEY, REGVERCUR);

   // Save the individual groups
   SaveGroup(&registry, PluginTypeEffect);
   SaveGroup(&registry, PluginTypeExporter);
   SaveGroup(&registry, PluginTypeAudacityCommand);
   SaveGroup(&registry, PluginTypeImporter);
   SaveGroup(&registry, PluginTypeStub);

   // Not used by 2.1.1 or greater, but must save to allow users to switch between 2.1.0
   // and 2.1.1+.  This should be removed after a few releases past 2.1.0.
   //SaveGroup(&registry, PluginTypeNone);

   // And now the providers
   SaveGroup(&registry, PluginTypeModule);

   // Just to be safe
   registry.Flush();
}

void PluginManager::SaveGroup(FileConfig *pRegistry, PluginType type)
{
   wxString group = GetPluginTypeString(type);
   for (auto &pair : mPlugins) {
      auto & plug = pair.second;

      if (plug.GetPluginType() != type)
      {
         continue;
      }

      pRegistry->SetPath(REGROOT + group + wxCONFIG_PATH_SEPARATOR + ConvertID(plug.GetID()));

      pRegistry->Write(KEY_PATH, plug.GetPath());

      // See comments with the corresponding load-time call to SetSymbol().
      pRegistry->Write(KEY_SYMBOL, plug.GetSymbol().Internal());

      // PRL:  Writing KEY_NAME which is no longer read, but older Audacity
      // versions expect to find it.
      pRegistry->Write(KEY_NAME, plug.GetSymbol().Msgid().MSGID());

      pRegistry->Write(KEY_VERSION, plug.GetUntranslatedVersion());
      pRegistry->Write(KEY_VENDOR, plug.GetVendor());
      // Write a blank -- see comments in LoadGroup:
      pRegistry->Write(KEY_DESCRIPTION, wxString{});
      pRegistry->Write(KEY_PROVIDERID, plug.GetProviderID());
      pRegistry->Write(KEY_ENABLED, plug.IsEnabled());
      pRegistry->Write(KEY_VALID, plug.IsValid());

      switch (type)
      {
         case PluginTypeModule:
         break;

         case PluginTypeEffect:
         {
            EffectType etype = plug.GetEffectType();
            wxString stype;
            if (etype == EffectTypeNone)
               stype = KEY_EFFECTTYPE_NONE;
            else if (etype == EffectTypeAnalyze)
               stype = KEY_EFFECTTYPE_ANALYZE;
            else if (etype == EffectTypeGenerate)
               stype = KEY_EFFECTTYPE_GENERATE;
            else if (etype == EffectTypeProcess)
               stype = KEY_EFFECTTYPE_PROCESS;
            else if (etype == EffectTypeTool)
               stype = KEY_EFFECTTYPE_TOOL;
            else if (etype == EffectTypeHidden)
               stype = KEY_EFFECTTYPE_HIDDEN;

            pRegistry->Write(KEY_EFFECTTYPE, stype);
            pRegistry->Write(KEY_EFFECTFAMILY, plug.GetEffectFamily());
            pRegistry->Write(KEY_EFFECTDEFAULT, plug.IsEffectDefault());
            pRegistry->Write(KEY_EFFECTINTERACTIVE, plug.IsEffectInteractive());
            pRegistry->Write(KEY_EFFECTREALTIME, plug.IsEffectRealtime());
            pRegistry->Write(KEY_EFFECTAUTOMATABLE, plug.IsEffectAutomatable());
         }
         break;

         case PluginTypeImporter:
         {
            pRegistry->Write(KEY_IMPORTERIDENT, plug.GetImporterIdentifier());
            const auto & extensions = plug.GetImporterExtensions();
            wxString strExt;
            for (size_t i = 0, cnt = extensions.size(); i < cnt; i++)
            {
               strExt += extensions[i] + wxT(":");
            }
            strExt.RemoveLast(1);
            pRegistry->Write(KEY_IMPORTEREXTENSIONS, strExt);
         }
         break;

         default:
         break;
      }
   }

   return;
}

// If bFast is true, do not do a full check.  Just check the ones
// that are quick to check.  Currently (Feb 2017) just Nyquist
// and built-ins.
void PluginManager::CheckForUpdates(bool bFast)
{
   ModuleManager & mm = ModuleManager::Get();
   wxArrayString pathIndex;
   for (auto &pair : mPlugins) {
      auto &plug = pair.second;

      // Bypass 2.1.0 placeholders...remove this after a few releases past 2.1.0
      if (plug.GetPluginType() != PluginTypeNone)
         pathIndex.push_back(plug.GetPath().BeforeFirst(wxT(';')));
   }

   // Check all known plugins to ensure they are still valid and scan for NEW ones.
   // 
   // All NEW plugins get a stub entry created that will remain in place until the
   // user enables or disables the plugin.
   //
   // Because we use the plugins "path" as returned by the providers, we can actually
   // have multiple providers report the same path since, at this point, they only
   // know that the path might possibly be one supported by the provider.
   //
   // When the user enables the plugin, each provider that reported it will be asked
   // to register the plugin.
   for (auto &pair : mPlugins) {
      auto &plug = pair.second;
      const PluginID & plugID = plug.GetID();
      const wxString & plugPath = plug.GetPath();
      PluginType plugType = plug.GetPluginType();

      // Bypass 2.1.0 placeholders...remove this after a few releases past 2.1.0
      if (plugType == PluginTypeNone)
      {
         continue;
      }

      if ( plugType == PluginTypeModule  )
      {
         if( bFast ) 
         {
            // Skip modules, when doing a fast refresh/check.
         } 
         else if (!mm.IsProviderValid(plugID, plugPath))
         {
            plug.SetEnabled(false);
            plug.SetValid(false);
         }
         else
         {
            // Collect plugin paths
            PluginPaths paths;
            if (auto provider = mm.CreateProviderInstance( plugID, plugPath ) )
               paths = provider->FindPluginPaths( *this );
            for (size_t i = 0, cnt = paths.size(); i < cnt; i++)
            {
               wxString path = paths[i].BeforeFirst(wxT(';'));;
               if ( ! make_iterator_range( pathIndex ).contains( path ) )
               {
                  PluginID ID = plugID + wxT("_") + path;
                  PluginDescriptor & plug2 = mPlugins[ID];  // This will create a NEW descriptor
                  plug2.SetPluginType(PluginTypeStub);
                  plug2.SetID(ID);
                  plug2.SetProviderID(plugID);
                  plug2.SetPath(path);
                  plug2.SetEnabled(false);
                  plug2.SetValid(false);
               }
            }
         }
      }
      else if (plugType != PluginTypeNone && plugType != PluginTypeStub)
      {
         plug.SetValid(mm.IsPluginValid(plug.GetProviderID(), plugPath, bFast));
         if (!plug.IsValid())
         {
            plug.SetEnabled(false);
         }
      }
   }

   Save();

   return;
}

// Here solely for the purpose of Nyquist Workbench until
// a better solution is devised.
const PluginID & PluginManager::RegisterPlugin(
   std::unique_ptr<EffectDefinitionInterface> effect, PluginType type)
{
   PluginDescriptor & plug =
      CreatePlugin(GetID(effect.get()), effect.get(), type);

   plug.SetEffectType(effect->GetType());
   plug.SetEffectFamily(effect->GetFamily().Internal());
   plug.SetEffectInteractive(effect->IsInteractive());
   plug.SetEffectDefault(effect->IsDefault());
   plug.SetEffectRealtime(effect->SupportsRealtime());
   plug.SetEffectAutomatable(effect->SupportsAutomation());

   plug.SetInstance(std::move(effect));
   plug.SetEffectLegacy(true);
   plug.SetEnabled(true);
   plug.SetValid(true);

   return plug.GetID();
}

void PluginManager::UnregisterPlugin(const PluginID & ID)
{
   mPlugins.erase(ID);
}

int PluginManager::GetPluginCount(PluginType type)
{
   return count_if(mPlugins.begin(), mPlugins.end(), [type](auto &pair){
      return pair.second.GetPluginType() == type; });
}

const PluginDescriptor *PluginManager::GetPlugin(const PluginID & ID) const
{
   if (auto iter = mPlugins.find(ID); iter == mPlugins.end())
      return nullptr;
   else
      return &iter->second;
}

void PluginManager::Iterator::Advance(bool incrementing)
{
   const auto end = mPm.mPlugins.end();
   if (incrementing && mIterator != end)
      ++mIterator;
   bool all = mPluginType == PluginTypeNone && mEffectType == EffectTypeNone;
   for (; mIterator != end; ++mIterator) {
      auto &plug = mIterator->second;
      if (!all && !(plug.IsValid() && plug.IsEnabled()))
         continue;
      auto plugType = plug.GetPluginType();
      if ((mPluginType == PluginTypeNone || (plugType & mPluginType)) &&
         (mEffectType == EffectTypeNone || plug.GetEffectType() == mEffectType)) {
         if (!all && (plugType & PluginTypeEffect)) {
            // This preference may be written by EffectsPrefs
            auto setting = mPm.GetPluginEnabledSetting( plug );
            if (!(setting.empty() || gPrefs->Read( setting, true )))
               continue;
         }
         // Pause iteration at this match
         break;
      }
   }
}

PluginManager::Iterator::Iterator(PluginManager &manager)
: mPm{ manager }
, mIterator{ manager.mPlugins.begin() }
{   
}

PluginManager::Iterator::Iterator(PluginManager &manager, int type)
: mPm{ manager }
, mIterator{ manager.mPlugins.begin() }
, mPluginType{ type }
{
   Advance(false);
}

PluginManager::Iterator::Iterator(PluginManager &manager, EffectType type)
: mPm{ manager }
, mIterator{ manager.mPlugins.begin() }
, mEffectType{ type }
{
   Advance(false);
}

auto PluginManager::Iterator::operator ++() -> Iterator &
{
   Advance(true);
   return *this;
}

bool PluginManager::IsPluginEnabled(const PluginID & ID)
{
   if (auto iter = mPlugins.find(ID); iter == mPlugins.end())
      return false;
   else
      return iter->second.IsEnabled();
}

void PluginManager::EnablePlugin(const PluginID & ID, bool enable)
{
   if (auto iter = mPlugins.find(ID); iter == mPlugins.end())
      return;
   else
      iter->second.SetEnabled(enable);
}

const ComponentInterfaceSymbol & PluginManager::GetSymbol(const PluginID & ID)
{
   if (auto iter = mPlugins.find(ID); iter == mPlugins.end()) {
      static ComponentInterfaceSymbol empty;
      return empty;
   }
   else
      return iter->second.GetSymbol();
}

ComponentInterface *PluginManager::GetInstance(const PluginID & ID)
{
   if (auto iter = mPlugins.find(ID); iter == mPlugins.end())
      return nullptr;
   else {
      auto &plug = iter->second;

      // If not dealing with legacy effects, make sure the provider is loaded
      if (!plug.IsEffectLegacy())
      {
         const PluginID & prov = plug.GetProviderID();
         if (auto iter2 = mPlugins.find(prov); iter2 == mPlugins.end())
            return nullptr;
         else
            iter2->second.GetInstance();
      }

      return plug.GetInstance();
   }
}

PluginID PluginManager::GetID(ModuleInterface *module)
{
   return ModuleManager::GetID(module);
}

PluginID PluginManager::GetID(ComponentInterface *command)
{
   return wxString::Format(wxT("%s_%s_%s_%s_%s"),
                           GetPluginTypeString(PluginTypeAudacityCommand),
                           wxEmptyString,
                           command->GetVendor().Internal(),
                           command->GetSymbol().Internal(),
                           command->GetPath());
}

PluginID PluginManager::GetID(EffectDefinitionInterface *effect)
{
   return wxString::Format(wxT("%s_%s_%s_%s_%s"),
                           GetPluginTypeString(PluginTypeEffect),
                           effect->GetFamily().Internal(),
                           effect->GetVendor().Internal(),
                           effect->GetSymbol().Internal(),
                           effect->GetPath());
}

// This string persists in configuration files
// So config compatibility will break if it is changed across Audacity versions
wxString PluginManager::GetPluginTypeString(PluginType type)
{
   wxString str;

   switch (type)
   {
   default:
   case PluginTypeNone:
      str = wxT("Placeholder");
      break;
   case PluginTypeStub:
      str = wxT("Stub");
      break;
   case PluginTypeEffect:
      str = wxT("Effect");
      break;
   case PluginTypeAudacityCommand:
      str = wxT("Generic");
      break;
   case PluginTypeExporter:
      str = wxT("Exporter");
      break;
   case PluginTypeImporter:
      str = wxT("Importer");
      break;
   case PluginTypeModule:
      str = ModuleManager::GetPluginTypeString();
      break;
   }

   return str;
}

PluginDescriptor & PluginManager::CreatePlugin(const PluginID & id,
                                               ComponentInterface *ident,
                                               PluginType type)
{
   // This will either create a NEW entry or replace an existing entry
   PluginDescriptor & plug = mPlugins[id];

   plug.SetPluginType(type);

   plug.SetID(id);
   plug.SetPath(ident->GetPath());
   plug.SetSymbol(ident->GetSymbol());
   plug.SetVendor(ident->GetVendor().Internal());
   plug.SetVersion(ident->GetVersion());

   return plug;
}

FileConfig *PluginManager::GetSettings()
{
   if (!mSettings)
   {
      mSettings =
         AudacityFileConfig::Create({}, {}, FileNames::PluginSettings());

      // Check for a settings version that we can understand
      if (mSettings->HasEntry(SETVERKEY))
      {
         wxString setver = mSettings->Read(SETVERKEY, SETVERKEY);
         if (setver < SETVERCUR )
         {
            // This is where we'd put in conversion code when the
            // settings version changes.
            //
            // Should also check for a settings file that is newer than
            // what we can understand.
         }
      }
      else
      {
         // Make sure is has a version string
         mSettings->Write(SETVERKEY, SETVERCUR);
         mSettings->Flush();
      }
   }

   return mSettings.get();
}

bool PluginManager::HasGroup(const RegistryPath & group)
{
   auto settings = GetSettings();

   bool res = settings->HasGroup(group);
   if (res)
   {
      // The group exists, but empty groups aren't considered valid
      wxString oldPath = settings->GetPath();
      settings->SetPath(group);
      res = settings->GetNumberOfEntries() || settings->GetNumberOfGroups();
      settings->SetPath(oldPath);
   }

   return res;
}

bool PluginManager::GetSubgroups(const RegistryPath & group, RegistryPaths & subgroups)
{
   if (group.empty() || !HasGroup(group))
   {
      return false;
   }

   wxString path = GetSettings()->GetPath();
   GetSettings()->SetPath(group);

   wxString name;
   long index = 0;
   if (GetSettings()->GetFirstGroup(name, index))
   {
      do
      {
         subgroups.push_back(name);
      } while (GetSettings()->GetNextGroup(name, index));
   }

   GetSettings()->SetPath(path);

   return true;
}

bool PluginManager::GetConfig(const RegistryPath & key, int & value, int defval)
{
   bool result = false;

   if (!key.empty())
   {
      result = GetSettings()->Read(key, &value, defval);
   }

   return result;
}

bool PluginManager::GetConfig(const RegistryPath & key, wxString & value, const wxString & defval)
{
   bool result = false;

   if (!key.empty())
   {
      wxString wxval;

      result = GetSettings()->Read(key, &wxval, defval);

      value = wxval;
   }

   return result;
}

bool PluginManager::GetConfig(const RegistryPath & key, bool & value, bool defval)
{
   bool result = false;

   if (!key.empty())
   {
      result = GetSettings()->Read(key, &value, defval);
   }

   return result;
}

bool PluginManager::GetConfig(const RegistryPath & key, float & value, float defval)
{
   bool result = false;

   if (!key.empty())
   {
      double dval = 0.0;

      result = GetSettings()->Read(key, &dval, (double) defval);

      value = (float) dval;
   }

   return result;
}

bool PluginManager::GetConfig(const RegistryPath & key, double & value, double defval)
{
   bool result = false;

   if (!key.empty())
   {
      result = GetSettings()->Read(key, &value, defval);
   }

   return result;
}

bool PluginManager::SetConfig(const RegistryPath & key, const wxString & value)
{
   bool result = false;

   if (!key.empty())
   {
      wxString wxval = value;
      result = GetSettings()->Write(key, wxval);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

bool PluginManager::SetConfig(const RegistryPath & key, const int & value)
{
   bool result = false;

   if (!key.empty())
   {
      result = GetSettings()->Write(key, value);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

bool PluginManager::SetConfig(const RegistryPath & key, const bool & value)
{
   bool result = false;

   if (!key.empty())
   {
      result = GetSettings()->Write(key, value);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

bool PluginManager::SetConfig(const RegistryPath & key, const float & value)
{
   bool result = false;

   if (!key.empty())
   {
      result = GetSettings()->Write(key, value);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

bool PluginManager::SetConfig(const RegistryPath & key, const double & value)
{
   bool result = false;

   if (!key.empty())
   {
      result = GetSettings()->Write(key, value);
      if (result)
      {
         result = GetSettings()->Flush();
      }
   }

   return result;
}

/* Return value is a key for lookup in a config file */
RegistryPath PluginManager::SettingsPath(const PluginID & ID, bool shared)
{
   // All the strings reported by PluginDescriptor and used in this function
   // persist in the plugin settings configuration file, so they should not
   // be changed across Audacity versions, or else compatibility of the
   // configuration files will break.

   if (auto iter = mPlugins.find(ID); iter == mPlugins.end())
      return {};
   else {
      const PluginDescriptor & plug = iter->second;
      
      wxString id = GetPluginTypeString(plug.GetPluginType()) +
                    wxT("_") +
                    plug.GetEffectFamily() + // is empty for non-Effects
                    wxT("_") +
                    plug.GetVendor() +
                    wxT("_") +
                    (shared ? wxString{} : plug.GetSymbol().Internal());

      return SETROOT +
             ConvertID(id) +
             wxCONFIG_PATH_SEPARATOR +
             (shared ? wxT("shared") : wxT("private")) +
             wxCONFIG_PATH_SEPARATOR;
   }
}

/* Return value is a key for lookup in a config file */
RegistryPath PluginManager::SharedGroup(const PluginID & ID, const RegistryPath & group)
{
   wxString path = SettingsPath(ID, true);

   wxFileName ff(group);
   if (!ff.GetName().empty())
   {
      path += ff.GetFullPath(wxPATH_UNIX) + wxCONFIG_PATH_SEPARATOR;
   }

   return path;
}

/* Return value is a key for lookup in a config file */
RegistryPath PluginManager::SharedKey(const PluginID & ID, const RegistryPath & group, const RegistryPath & key)
{
   auto path = SharedGroup(ID, group);
   if (path.empty())
   {
      return path;
   }

   return path + key;
}

/* Return value is a key for lookup in a config file */
RegistryPath PluginManager::PrivateGroup(const PluginID & ID, const RegistryPath & group)
{
   auto path = SettingsPath(ID, false);

   wxFileName ff(group);
   if (!ff.GetName().empty())
   {
      path += ff.GetFullPath(wxPATH_UNIX) + wxCONFIG_PATH_SEPARATOR;
   }

   return path;
}

/* Return value is a key for lookup in a config file */
RegistryPath PluginManager::PrivateKey(const PluginID & ID, const RegistryPath & group, const RegistryPath & key)
{
   auto path = PrivateGroup(ID, group);
   if (path.empty())
   {
      return path;
   }

   return path + key;
}

// Sanitize the ID...not the best solution, but will suffice until this
// is converted to XML.  We use base64 encoding to preserve case.
wxString PluginManager::ConvertID(const PluginID & ID)
{
   if (ID.StartsWith(wxT("base64:")))
   {
      wxString id = ID.Mid(7);
      ArrayOf<char> buf{ id.length() / 4 * 3 };
      id =  wxString::FromUTF8(buf.get(), b64decode(id, buf.get()));
      return id;
   }

   const wxCharBuffer & buf = ID.ToUTF8();
   return wxT("base64:") + b64encode(buf, strlen(buf));
}

////////////////////////////////////////////////////////////////////////////////
// Base64 en/decoding
//
// Original routines marked as public domain and found at:
//
// http://en.wikibooks.org/wiki/Algorithm_implementation/Miscellaneous/Base64
//
////////////////////////////////////////////////////////////////////////////////

// Lookup table for encoding
const static wxChar cset[] = wxT("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/");
const static char padc = wxT('=');

wxString PluginManager::b64encode(const void *in, int len)
{
   unsigned char *p = (unsigned char *) in;
   wxString out;

   unsigned long temp;
   for (int i = 0; i < len / 3; i++)
   {
      temp  = (*p++) << 16; //Convert to big endian
      temp += (*p++) << 8;
      temp += (*p++);
      out += cset[(temp & 0x00FC0000) >> 18];
      out += cset[(temp & 0x0003F000) >> 12];
      out += cset[(temp & 0x00000FC0) >> 6];
      out += cset[(temp & 0x0000003F)];
   }

   switch (len % 3)
   {
      case 1:
         temp  = (*p++) << 16; //Convert to big endian
         out += cset[(temp & 0x00FC0000) >> 18];
         out += cset[(temp & 0x0003F000) >> 12];
         out += padc;
         out += padc;
      break;

      case 2:
         temp  = (*p++) << 16; //Convert to big endian
         temp += (*p++) << 8;
         out += cset[(temp & 0x00FC0000) >> 18];
         out += cset[(temp & 0x0003F000) >> 12];
         out += cset[(temp & 0x00000FC0) >> 6];
         out += padc;
      break;
   }

   return out;
}

int PluginManager::b64decode(const wxString &in, void *out)
{
   int len = in.length();
   unsigned char *p = (unsigned char *) out;

   if (len % 4)  //Sanity check
   {
      return 0;
   }

   int padding = 0;
   if (len)
   {
      if (in[len - 1] == padc)
      {
         padding++;
      }

      if (in[len - 2] == padc)
      {
         padding++;
      }
   }

   //const char *a = in.mb_str();
   //Setup a vector to hold the result
   unsigned long temp = 0; //Holds decoded quanta
   int i = 0;
   while (i < len)
   {
      for (int quantumPosition = 0; quantumPosition < 4; quantumPosition++)
      {
         unsigned char c = in[i];
         temp <<= 6;

         if (c >= 0x41 && c <= 0x5A)
         {
            temp |= c - 0x41;
         }
         else if (c >= 0x61 && c <= 0x7A)
         {
            temp |= c - 0x47;
         }
         else if (c >= 0x30 && c <= 0x39)
         {
            temp |= c + 0x04;
         }
         else if (c == 0x2B)
         {
            temp |= 0x3E;
         }
         else if (c == 0x2F)
         {
            temp |= 0x3F;
         }
         else if (c == padc)
         {
            switch (len - i)
            {
               case 1: //One pad character
                  *p++ = (temp >> 16) & 0x000000FF;
                  *p++ = (temp >> 8) & 0x000000FF;
                  return p - (unsigned char *) out;
               case 2: //Two pad characters
                  *p++ = (temp >> 10) & 0x000000FF;
                  return p - (unsigned char *) out;
            }
         }
         i++;
      }
      *p++ = (temp >> 16) & 0x000000FF;
      *p++ = (temp >> 8) & 0x000000FF;
      *p++ = temp & 0x000000FF;
   }

   return p - (unsigned char *) out;
}

// This is defined out-of-line here, to keep ComponentInterface free of other
// #include directives.
TranslatableString ComponentInterface::GetName()
{
   return GetSymbol().Msgid();
}
