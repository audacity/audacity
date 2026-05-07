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

#include "au3-basic-ui/BasicUI.h"
#include "au3-components/PluginProvider.h"

#include "au3-strings/Internat.h" // for macro XO
#include "au3-files/FileNames.h"
#include "au3-utility/MemoryX.h"
#include "ModuleManager.h"
#include "au3-files/PlatformCompatibility.h"
#include "au3-strings/Base64.h"
#include "au3-utility/Variant.h"

///////////////////////////////////////////////////////////////////////////////
//
// PluginManager
//
///////////////////////////////////////////////////////////////////////////////

// Registry has the list of plug ins
#define REGROOT wxString(wxT("/pluginregistry/"))
#define REGCUSTOMPATHS wxString(wxT("/providercustompaths"))

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

const PluginID& PluginManagerInterface::DefaultRegistrationCallback(
    PluginProvider* provider, ComponentInterface* pInterface)
{
    if (auto effectDefinitionInterface = dynamic_cast<EffectDefinitionInterface*>(pInterface)) {
        return PluginManager::Get().RegisterPlugin(provider, effectDefinitionInterface, PluginTypeEffect);
    }
    return PluginManager::Get().RegisterPlugin(provider, pInterface);
}

const PluginID& PluginManagerInterface::AudacityCommandRegistrationCallback(
    PluginProvider* provider, ComponentInterface* pInterface)
{
    return PluginManager::Get().RegisterPlugin(provider, pInterface);
}

RegistryPath PluginManager::GetPluginEnabledSetting(const PluginID& ID) const
{
    auto pPlugin = GetPlugin(ID);
    if (pPlugin) {
        return GetPluginEnabledSetting(*pPlugin);
    }
    return {};
}

RegistryPath PluginManager::GetPluginEnabledSetting(
    const PluginDescriptor& desc) const
{
    switch (desc.GetPluginType()) {
    case PluginTypeModule: {
        // Retrieve optional family symbol that was recorded in
        // RegisterPlugin() for the module
        auto family = desc.GetEffectFamily();
        if (family.empty()) {  // as for built-in effect and command modules
            return {};
        } else {
            return wxT('/') + family + wxT("/Enable");
        }
    }
    case PluginTypeEffect:
        // do NOT use GetEffectFamily() for this descriptor, but instead,
        // delegate to the plugin descriptor of the provider, which may
        // be different (may be empty)
        return GetPluginEnabledSetting(desc.GetProviderID());
    default:
        return {};
    }
}

bool PluginManager::IsPluginRegistered(
    const PluginPath& path, const TranslatableString* pName)
{
    for (auto& pair : mRegisteredPlugins) {
        if (auto& descriptor = pair.second; descriptor.GetPath() == path) {
            if (pName) {
                descriptor.SetSymbol(
                    { descriptor.GetSymbol().Internal(), *pName });
            }
            return true;
        }
    }
    return false;
}

bool PluginManager::IsPluginLoaded(const wxString& ID) const
{
    return mLoadedInterfaces.find(ID) != mLoadedInterfaces.end();
}

void PluginManager::RegisterPlugin(PluginDescriptor&& desc)
{
    mRegisteredPlugins[desc.GetID()] = std::move(desc);
}

const PluginID& PluginManager::RegisterPlugin(PluginProvider* provider)
{
    PluginDescriptor& plug
        =CreatePlugin(GetID(provider), provider, PluginTypeModule);
    plug.SetEffectFamily(provider->GetOptionalFamilySymbol().Internal());

    plug.SetEnabled(true);
    plug.SetValid(true);

    return plug.GetID();
}

const PluginID& PluginManager::RegisterPlugin(
    PluginProvider* provider, ComponentInterface* command)
{
    PluginDescriptor& plug = CreatePlugin(GetID(command), command, (PluginType)PluginTypeAudacityCommand);

    plug.SetProviderID(PluginManager::GetID(provider));

    plug.SetEnabled(true);
    plug.SetValid(true);

    return plug.GetID();
}

const PluginID& PluginManager::RegisterPlugin(
    PluginProvider* provider, EffectDefinitionInterface* effect, int type)
{
    PluginDescriptor& plug = CreatePlugin(GetID(effect), effect, (PluginType)type);

    plug.SetProviderID(PluginManager::GetID(provider));

    plug.SetEffectType(effect->GetClassification());
    plug.SetDescription(effect->GetDescription().Translation());
    plug.SetEffectFamily(effect->GetFamily().Internal());
    plug.SetEffectGroup(effect->GetGroup());
    plug.SetEffectInteractive(effect->IsInteractive());
    plug.SetEffectDefault(effect->IsDefault());
    plug.SetRealtimeSupport(effect->RealtimeSupport());
    plug.SetParamsAreInputAgnostic(effect->ParamsAreInputAgnostic());
    plug.SetEffectAutomatable(effect->SupportsAutomation());

    plug.SetEnabled(true);
    plug.SetValid(true);

    return plug.GetID();
}

void PluginManager::FindFilesInPathList(const wxString& pattern,
                                        const FilePaths& pathList,
                                        FilePaths& files,
                                        bool directories)
{
    wxLogNull nolog;

    // Why bother...
    if (pattern.empty()) {
        return;
    }

    // TODO:  We REALLY need to figure out the "Audacity" plug-in path(s)

    FilePaths paths;

    // Add the "per-user" plug-ins directory
    {
        const wxFileName& ff = FileNames::PlugInDir();
        paths.push_back(ff.GetFullPath());
    }

    // Add the "Audacity" plug-ins directory
    wxFileName ff = wxString { PlatformCompatibility::GetExecutablePath() };
#if defined(__WXMAC__)
    // Path ends for example in "Audacity.app/Contents/MacOSX"
    //ff.RemoveLastDir();
    //ff.RemoveLastDir();
    // just remove the MacOSX part.
    ff.RemoveLastDir();
#endif
    ff.AppendDir(wxT("nyquist-plug-ins"));
    paths.push_back(ff.GetPath());

    // Weed out duplicates
    for (const auto& filePath : pathList) {
        ff = filePath;
        const wxString path{ ff.GetFullPath() };
        if (paths.Index(path, wxFileName::IsCaseSensitive()) == wxNOT_FOUND) {
            paths.push_back(path);
        }
    }

    // Find all matching files in each path
    for (size_t i = 0, cnt = paths.size(); i < cnt; i++) {
        ff = paths[i] + wxFILE_SEP_PATH + pattern;
        wxDir::GetAllFiles(ff.GetPath(), &files, ff.GetFullName(), directories ? wxDIR_DEFAULT : wxDIR_FILES);
    }

    return;
}

bool PluginManager::HasConfigGroup(ConfigurationType type, const PluginID& ID,
                                   const RegistryPath& group)
{
    return HasGroup(Group(type, ID, group));
}

bool PluginManager::GetConfigSubgroups(ConfigurationType type,
                                       const PluginID& ID, const RegistryPath& group, RegistryPaths& subgroups)
{
    return GetSubgroups(Group(type, ID, group), subgroups);
}

bool PluginManager::HasConfigValue(ConfigurationType type, const PluginID& ID,
                                   const RegistryPath& group, const RegistryPath& key)
{
    return HasConfigValue(Key(type, ID, group, key));
}

bool PluginManager::GetConfigValue(ConfigurationType type, const PluginID& ID,
                                   const RegistryPath& group, const RegistryPath& key,
                                   ConfigReference var, ConfigConstReference defval)
{
    return GetConfigValue(Key(type, ID, group, key), var, defval);
}

bool PluginManager::SetConfigValue(ConfigurationType type, const PluginID& ID,
                                   const RegistryPath& group, const RegistryPath& key,
                                   ConfigConstReference value)
{
    return SetConfigValue(Key(type, ID, group, key), value);
}

bool PluginManager::RemoveConfigSubgroup(ConfigurationType type,
                                         const PluginID& ID, const RegistryPath& group)
{
    bool result = GetSettings()->DeleteGroup(Group(type, ID, group));
    if (result) {
        GetSettings()->Flush();
    }

    return result;
}

bool PluginManager::RemoveConfig(ConfigurationType type, const PluginID& ID,
                                 const RegistryPath& group, const RegistryPath& key)
{
    bool result = GetSettings()->DeleteEntry(Key(type, ID, group, key));
    if (result) {
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
}

// ----------------------------------------------------------------------------
// PluginManager implementation
// ----------------------------------------------------------------------------

static PluginManager::ConfigFactory sFactory;

// ============================================================================
//
// Return reference to singleton
//
// (Thread-safe...no active threading during construction or after destruction)
// ============================================================================

PluginManager& PluginManager::Get()
{
    if (!mInstance) {
        mInstance.reset(safenew PluginManager);
    }

    return *mInstance;
}

void PluginManager::Initialize(ConfigFactory factory)
{
    sFactory = move(factory);

    // And force load of setting to verify it's accessible
    GetSettings();
}

void PluginManager::Terminate()
{
    if (mSettings) {
        mSettings->Flush();
        mSettings.reset();
    }

    mLoadedInterfaces.clear();
}

bool PluginManager::DropFile(const wxString& fileName)
{
    using namespace BasicUI;
    auto& mm = ModuleManager::Get();
    const wxFileName src{ fileName };

    for (auto& plug : PluginsOfType(PluginTypeModule)) {
        auto module = static_cast<PluginProvider*>
                      (mm.CreateProviderInstance(plug.GetID(), plug.GetPath()));
        if (!module) {
            continue;
        }

        const auto& ff = module->InstallPath();
        const auto& extensions = module->GetFileExtensions();
        if (!ff.empty()
            && extensions.Index(src.GetExt(), false) != wxNOT_FOUND) {
            TranslatableString errMsg;
            // Do dry-run test of the file format
            unsigned nPlugIns
                =module->DiscoverPluginsAtPath(fileName, errMsg, {});
            if (nPlugIns) {
                // File contents are good for this module, so check no others.
                // All branches of this block return true, even in case of
                // failure for other reasons, to signal that other drag-and-drop
                // actions should not be tried.

                // Find path to copy it
                wxFileName dst;
                dst.AssignDir(ff);
                dst.SetFullName(src.GetFullName());
                if (dst.Exists()) {
                    // Query whether to overwrite
                    bool overwrite = (MessageBoxResult::Yes == ShowMessageBox(
                                          XO("Overwrite the plug-in file %s?")
                                          .Format(dst.GetFullPath()),
                                          MessageBoxOptions {}
                                          .Caption(XO("Plug-in already exists"))
                                          .ButtonStyle(Button::YesNo)));
                    if (!overwrite) {
                        return true;
                    }
                }

                // Move the file or subtree
                bool copied = false;
                auto dstPath = dst.GetFullPath();
                if (src.FileExists()) {
                    // A simple one-file plug-in
                    copied = FileNames::DoCopyFile(
                        src.GetFullPath(), dstPath, true);
                } else {
                    // A sub-folder
                    // such as for some VST packages
                    // Recursive copy needed -- to do
                    return true;
                }

                if (!copied) {
                    ShowMessageBox(
                        XO("Plug-in file is in use. Failed to overwrite"));
                    return true;
                }

                // Register for real
                std::vector<PluginID> ids;
                std::vector<wxString> names;
                nPlugIns = module->DiscoverPluginsAtPath(dstPath, errMsg,
                                                         [&](PluginProvider* provider, ComponentInterface* ident)
                                                         -> const PluginID& {
                    // Register as by default, but also collecting the PluginIDs
                    // and names
                    auto& id = PluginManagerInterface::DefaultRegistrationCallback(
                        provider, ident);
                    ids.push_back(id);
                    names.push_back(ident->GetSymbol().Translation());
                    return id;
                });
                if (!nPlugIns) {
                    // Unlikely after the dry run succeeded
                    ShowMessageBox(
                        XO("Failed to register:\n%s").Format(errMsg));
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
                        )(nIds);
                    for (const auto& name : names) {
                        message.Join(Verbatim(name), wxT("\n"));
                    }
                    bool enable = (MessageBoxResult::Yes == ShowMessageBox(
                                       message,
                                       MessageBoxOptions {}
                                       .Caption(XO("Enable new plug-ins"))
                                       .ButtonStyle(Button::YesNo)));
                    for (const auto& id : ids) {
                        mRegisteredPlugins[id].SetEnabled(enable);
                    }
                    // Make changes to enabled status persist:
                    this->NotifyPluginsChanged();
                }

                return true;
            }
        }
    }

    return false;
}

void PluginManager::NotifyPluginsChanged()
{
    Publisher<PluginsChangedMessage>::Publish({});
}

PluginPaths PluginManager::ReadCustomPaths(const PluginProvider& provider)
{
    auto group = mSettings->BeginGroup(REGCUSTOMPATHS);
    const auto key = GetID(&provider);
    const auto paths = mSettings->Read(key, wxString {});
    const auto wxarr = wxSplit(paths, ';');
    return PluginPaths(wxarr.begin(), wxarr.end());
}

void PluginManager::StoreCustomPaths(const PluginProvider& provider, const PluginPaths& paths)
{
    auto group = mSettings->BeginGroup(REGCUSTOMPATHS);
    const auto key = GetID(&provider);
    wxArrayString wxarr;
    std::copy(paths.begin(), paths.end(), std::back_inserter(wxarr));
    mSettings->Write(key, wxJoin(wxarr, ';'));
}

// Here solely for the purpose of Nyquist Workbench until
// a better solution is devised.
const PluginID& PluginManager::RegisterPlugin(
    std::unique_ptr<EffectDefinitionInterface> effect, PluginType type)
{
    PluginDescriptor& plug
        =CreatePlugin(GetID(effect.get()), effect.get(), type);

    plug.SetEffectType(effect->GetType());
    plug.SetEffectFamily(effect->GetFamily().Internal());
    plug.SetEffectInteractive(effect->IsInteractive());
    plug.SetEffectDefault(effect->IsDefault());
    plug.SetRealtimeSupport(effect->RealtimeSupport());
    plug.SetEffectAutomatable(effect->SupportsAutomation());

    plug.SetEffectLegacy(true);
    plug.SetEnabled(true);
    plug.SetValid(true);

    mLoadedInterfaces[plug.GetID()] = std::move(effect);

    return plug.GetID();
}

void PluginManager::UnregisterPlugin(const PluginID& ID)
{
    mRegisteredPlugins.erase(ID);
    mLoadedInterfaces.erase(ID);
}

int PluginManager::GetPluginCount(PluginType type)
{
    return count_if(mRegisteredPlugins.begin(), mRegisteredPlugins.end(), [type](auto& pair){
        return pair.second.GetPluginType() == type;
    });
}

const PluginDescriptor* PluginManager::GetPlugin(const PluginID& ID) const
{
    if (auto iter = mRegisteredPlugins.find(ID); iter != mRegisteredPlugins.end()) {
        return &iter->second;
    }

    auto iter2 = make_iterator_range(mEffectPluginsCleared)
                 .find_if([&ID](const PluginDescriptor& plug) {
        return plug.GetID() == ID;
    });
    if (iter2 != mEffectPluginsCleared.end()) {
        return &(*iter2);
    }

    return nullptr;
}

void PluginManager::Iterator::Advance(bool incrementing)
{
    const auto end = mPm.mRegisteredPlugins.end();
    if (incrementing && mIterator != end) {
        ++mIterator;
    }
    bool all = mPluginType == PluginTypeNone && mEffectType == EffectTypeNone;
    for (; mIterator != end; ++mIterator) {
        auto& plug = mIterator->second;
        if (!all && !(plug.IsValid() && plug.IsEnabled())) {
            continue;
        }
        auto plugType = plug.GetPluginType();
        if ((mPluginType == PluginTypeNone || (plugType & mPluginType))
            && (mEffectType == EffectTypeNone || plug.GetEffectType() == mEffectType)) {
            if (!all && (plugType & PluginTypeEffect)) {
                // This preference may be written by EffectsPrefs
                auto setting = mPm.GetPluginEnabledSetting(plug);
                if (!(setting.empty() || gPrefs->Read(setting, true))) {
                    continue;
                }
            }
            // Pause iteration at this match
            break;
        }
    }
}

PluginManager::Iterator::Iterator(PluginManager& manager)
    : mPm{manager}
    , mIterator{manager.mRegisteredPlugins.begin()}
{
}

PluginManager::Iterator::Iterator(PluginManager& manager, int type)
    : mPm{manager}
    , mIterator{manager.mRegisteredPlugins.begin()}
    , mPluginType{type}
{
    Advance(false);
}

PluginManager::Iterator::Iterator(PluginManager& manager, EffectType type)
    : mPm{manager}
    , mIterator{manager.mRegisteredPlugins.begin()}
    , mEffectType{type}
{
    Advance(false);
}

auto PluginManager::Iterator::operator ++() -> Iterator
&
{
    Advance(true);
    return *this;
}

bool PluginManager::IsPluginEnabled(const PluginID& ID)
{
    if (auto iter = mRegisteredPlugins.find(ID); iter == mRegisteredPlugins.end()) {
        return false;
    } else {
        return iter->second.IsEnabled();
    }
}

void PluginManager::EnablePlugin(const PluginID& ID, bool enable)
{
    if (auto iter = mRegisteredPlugins.find(ID); iter == mRegisteredPlugins.end()) {
        return;
    } else {
        iter->second.SetEnabled(enable);
    }
}

const ComponentInterfaceSymbol&
PluginManager::GetSymbol(const PluginID& ID) const
{
    if (auto iter = mRegisteredPlugins.find(ID); iter == mRegisteredPlugins.end()) {
        static ComponentInterfaceSymbol empty;
        return empty;
    } else {
        return iter->second.GetSymbol();
    }
}

TranslatableString PluginManager::GetName(const PluginID& ID) const
{
    return GetSymbol(ID).Msgid();
}

CommandID PluginManager::GetCommandIdentifier(const PluginID& ID) const
{
    const auto name = GetSymbol(ID).Internal();
    return EffectDefinitionInterface::GetSquashedName(name);
}

const PluginID&
PluginManager::GetByCommandIdentifier(const CommandID& strTarget)
{
    static PluginID empty;
    if (strTarget.empty()) {     // set GetCommandIdentifier to wxT("") to not show an
                                 // effect in Batch mode
        return empty;
    }

    // Effects OR Generic commands...
    for (auto& plug :
         PluginsOfType(PluginTypeEffect | PluginTypeAudacityCommand)) {
        auto& ID = plug.GetID();
        if (GetCommandIdentifier(ID) == strTarget) {
            return ID;
        }
    }
    return empty;
}

ComponentInterface* PluginManager::Load(const PluginID& ID)
{
    if (auto it = mLoadedInterfaces.find(ID); it != mLoadedInterfaces.end()) {
        return it->second.get();
    }

    if (auto it = mRegisteredPlugins.find(ID); it != mRegisteredPlugins.end()) {
        auto& desc = it->second;
        if (desc.GetPluginType() == PluginTypeModule) {
            //it's very likely that this code path is not used
            return ModuleManager::Get().CreateProviderInstance(desc.GetID(), desc.GetPath());
        }

        if (auto provider = ModuleManager::Get().CreateProviderInstance(desc.GetProviderID(), wxEmptyString)) {
            auto pluginInterface = provider->LoadPlugin(desc.GetPath());
            auto result = pluginInterface.get();
            mLoadedInterfaces[desc.GetID()] = std::move(pluginInterface);
            return result;
        }
    }
    return nullptr;
}

void PluginManager::ClearEffectPlugins()
{
    mEffectPluginsCleared.clear();

    for ( auto it = mRegisteredPlugins.cbegin(); it != mRegisteredPlugins.cend();) {
        const auto& desc = it->second;
        const auto type = desc.GetPluginType();

        if (type == PluginTypeEffect || type == PluginTypeStub) {
            mEffectPluginsCleared.push_back(desc);
            it = mRegisteredPlugins.erase(it);
        } else {
            ++it;
        }
    }

    // Repeat what usually happens at startup
    // This prevents built-in plugins to appear in the plugin validation list
    for (auto& [_, provider] : ModuleManager::Get().Providers()) {
        provider->AutoRegisterPlugins(*this);
    }

    // Remove auto registered plugins from "cleared" list
    for ( auto it = mEffectPluginsCleared.begin(); it != mEffectPluginsCleared.end();) {
        if (mRegisteredPlugins.find(it->GetID()) != mRegisteredPlugins.end()) {
            it = mEffectPluginsCleared.erase(it);
        } else {
            ++it;
        }
    }
}

std::map<wxString, std::vector<wxString> > PluginManager::CheckPluginUpdates()
{
    wxArrayString pathIndex;
    for (auto& pair : mRegisteredPlugins) {
        auto& plug = pair.second;

        // Bypass 2.1.0 placeholders...remove this after a few releases past 2.1.0
        if (plug.GetPluginType() != PluginTypeNone) {
            pathIndex.push_back(plug.GetPath().BeforeFirst(wxT(';')));
        }
    }

    // Scan for NEW ones.
    //
    // Because we use the plugins "path" as returned by the providers, we can actually
    // have multiple providers report the same path since, at this point, they only
    // know that the path might possibly be one supported by the provider.
    //
    // When the user enables the plugin, each provider that reported it will be asked
    // to register the plugin.

    auto& moduleManager = ModuleManager::Get();
    std::map<wxString, std::vector<wxString> > newPaths;
    for (auto& [id, provider] : moduleManager.Providers()) {
        const auto paths = provider->FindModulePaths(*this);
        for (const auto& path : paths) {
            const auto modulePath = path.BeforeFirst(';');
            if (!make_iterator_range(pathIndex).contains(modulePath)
                || make_iterator_range(mEffectPluginsCleared).any_of([&modulePath](const PluginDescriptor& plug) {
                return plug.GetPath().BeforeFirst(wxT(';')) == modulePath;
            })
                ) {
                newPaths[modulePath].push_back(id);
            }
        }
    }

    return newPaths;
}

PluginID PluginManager::GetID(const PluginProvider* provider)
{
    return ModuleManager::GetID(provider);
}

PluginID PluginManager::GetID(const ComponentInterface* command)
{
    return wxString::Format(wxT("%s_%s_%s_%s_%s"),
                            GetPluginTypeString(PluginTypeAudacityCommand),
                            wxEmptyString,
                            command->GetVendor().Internal(),
                            command->GetSymbol().Internal(),
                            command->GetPath());
}

PluginID PluginManager::OldGetID(const EffectDefinitionInterface* effect)
{
    return wxString::Format(wxT("%s_%s_%s_%s_%s"),
                            GetPluginTypeString(PluginTypeEffect),
                            effect->GetFamily().Internal(),
                            effect->GetVendor().Internal(),
                            effect->GetSymbol().Internal(),
                            effect->GetPath());
}

Identifier PluginManager::GetEffectNameFromID(const PluginID& ID)
{
    auto strings = wxSplit(ID, '_');
    if (strings.size() == 5) {
        return strings[3];
    }
    return {};
}

// This string persists in configuration files
// So config compatibility will break if it is changed across Audacity versions
wxString PluginManager::GetPluginTypeString(PluginType type)
{
    wxString str;

    switch (type) {
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

bool PluginManager::IsPluginAvailable(const PluginDescriptor& plug)
{
    const auto& providerID = plug.GetProviderID();
    auto provider = ModuleManager::Get().CreateProviderInstance(providerID, wxEmptyString);

    if (provider == nullptr) {
        wxLogWarning("Unable to find a provider for '%s'", providerID);
        return false;
    }

    if (provider->CheckPluginExist(plug.GetPath()) == false) {
        wxLogWarning("Plugin '%s' does not exist", plug.GetID());
        return false;
    }

    return true;
}

PluginDescriptor& PluginManager::CreatePlugin(const PluginID& id,
                                              ComponentInterface* ident,
                                              PluginType type)
{
    // This will either create a NEW entry or replace an existing entry
    PluginDescriptor& plug = mRegisteredPlugins[id];

    plug.SetPluginType(type);

    plug.SetID(id);
    plug.SetPath(ident->GetPath());
    plug.SetSymbol(ident->GetSymbol());
    plug.SetVendor(ident->GetVendor().Internal());
    plug.SetVersion(ident->GetVersion());

    return plug;
}

audacity::BasicSettings* PluginManager::GetSettings()
{
    if (!mSettings) {
        mSettings = sFactory(FileNames::PluginSettings());

        // Check for a settings version that we can understand
        if (mSettings->HasEntry(SETVERKEY)) {
            wxString setver = mSettings->Read(SETVERKEY, SETVERKEY);
            if (setver < SETVERCUR) {
                // This is where we'd put in conversion code when the
                // settings version changes.
                //
                // Should also check for a settings file that is newer than
                // what we can understand.
            }
        } else {
            // Make sure is has a version string
            mSettings->Write(SETVERKEY, SETVERCUR);
            mSettings->Flush();
        }
    }

    return mSettings.get();
}

bool PluginManager::HasGroup(const RegistryPath& groupName)
{
    auto settings = GetSettings();

    if (!settings->HasGroup(groupName)) {
        return false;
    }

    auto group = settings->BeginGroup(groupName);
    return !settings->GetChildGroups().empty() || !settings->GetChildKeys().empty();
}

bool PluginManager::GetSubgroups(const RegistryPath& groupName, RegistryPaths& subgroups)
{
    if (groupName.empty() || !HasGroup(groupName)) {
        return false;
    }

    auto group = GetSettings()->BeginGroup(groupName);
    for (const auto& name : GetSettings()->GetChildGroups()) {
        subgroups.push_back(name);
    }

    return true;
}

bool PluginManager::HasConfigValue(const RegistryPath& key)
{
    return GetSettings()->Exists(key);
}

template<typename T> class TD;

bool PluginManager::GetConfigValue(
    const RegistryPath& key, ConfigReference var, ConfigConstReference defval)
{
    using namespace Variant;
    if (key.empty()) {
        return false;
    }
    const auto visitor = [&](const auto var){
        const auto pVar = &var.get();
        // precondition is that defval wraps same type as var
        using Type = typename decltype(var)::type;
        const auto pDefval
            =std::get_if<std::reference_wrapper<const Type> >(&defval);
        //TD<decltype(pDefval)> defType;
        //return true;
        return GetSettings()->Read(key, pVar, pDefval->get());
    };
    return Visit(visitor, var);
}

bool PluginManager::SetConfigValue(
    const RegistryPath& key, ConfigConstReference value)
{
    using namespace Variant;
    if (key.empty()) {
        return false;
    }
    const auto visitor = [&](const auto value){
        return GetSettings()->Write(key, value.get()) && GetSettings()->Flush();
    };
    return Visit(visitor, value);
}

/* Return value is a key for lookup in a config file */
RegistryPath PluginManager::SettingsPath(
    ConfigurationType type, const PluginID& ID)
{
    bool shared = (type == ConfigurationType::Shared);

    // All the strings reported by PluginDescriptor and used in this function
    // persist in the plugin settings configuration file, so they should not
    // be changed across Audacity versions, or else compatibility of the
    // configuration files will break.

    if (auto iter = mRegisteredPlugins.find(ID); iter == mRegisteredPlugins.end()) {
        return {};
    } else {
        const PluginDescriptor& plug = iter->second;

        wxString id = GetPluginTypeString(plug.GetPluginType())
                      + wxT("_")
                      + plug.GetEffectFamily()     // is empty for non-Effects
                      + wxT("_")
                      + plug.GetVendor()
                      + wxT("_")
                      + (shared ? wxString{} : plug.GetSymbol().Internal());

        return SETROOT
               + ConvertID(id)
               + wxCONFIG_PATH_SEPARATOR
               + (shared ? wxT("shared") : wxT("private"))
               + wxCONFIG_PATH_SEPARATOR;
    }
}

/* Return value is a key for lookup in a config file */
RegistryPath PluginManager::Group(ConfigurationType type,
                                  const PluginID& ID, const RegistryPath& group)
{
    auto path = SettingsPath(type, ID);

    wxFileName ff(group);
    if (!ff.GetName().empty()) {
        path += ff.GetFullPath(wxPATH_UNIX) + wxCONFIG_PATH_SEPARATOR;
    }

    return path;
}

/* Return value is a key for lookup in a config file */
RegistryPath PluginManager::Key(ConfigurationType type, const PluginID& ID,
                                const RegistryPath& group, const RegistryPath& key)
{
    auto path = Group(type, ID, group);
    if (path.empty()) {
        return path;
    }

    return path + key;
}

// Sanitize the ID...not the best solution, but will suffice until this
// is converted to XML.  We use base64 encoding to preserve case.
wxString PluginManager::ConvertID(const PluginID& ID)
{
    if (ID.StartsWith(wxT("base64:"))) {
        wxString id = ID.Mid(7);
        ArrayOf<char> buf{ id.length() / 4 * 3 };
        id =  wxString::FromUTF8(buf.get(), Base64::Decode(id, buf.get()));
        return id;
    }

    const wxCharBuffer& buf = ID.ToUTF8();
    return wxT("base64:") + Base64::Encode(buf, strlen(buf));
}

// This is defined out-of-line here, to keep ComponentInterface free of other
// #include directives.
TranslatableString ComponentInterface::GetName() const
{
    return GetSymbol().Msgid();
}
