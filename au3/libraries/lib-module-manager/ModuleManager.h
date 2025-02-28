/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleManager.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MODULEMANAGER_H__
#define __AUDACITY_MODULEMANAGER_H__

#include "IteratorX.h"
#include <functional>
#include <map>
#include <memory>
#include <vector>
#include <wx/string.h>

#include "Identifier.h"

class wxArrayString;
class wxDynamicLibrary;
class ComponentInterface;
class PluginProvider;
class wxWindow;
using PluginID = wxString;
class TranslatableString;

//
// Module Manager
//
// wxPluginManager would be MUCH better, but it's an "undocumented" framework.
//

#include "ModuleConstants.h"

typedef int (* fnModuleDispatch)(ModuleDispatchTypes type);

class Module
{
public:
    Module(const FilePath& name);
    virtual ~Module();

    void ShowLoadFailureError(const wxString& Error);
    bool Load(wxString& deferredErrorMessage);
    void Unload();
    bool HasDispatch() { return mDispatch != NULL; }
    int Dispatch(ModuleDispatchTypes type);
    void* GetSymbol(const wxString& name);
    const FilePath& GetName() const { return mName; }

private:
    const FilePath mName;
    std::unique_ptr<wxDynamicLibrary> mLib;
    fnModuleDispatch mDispatch;
};

class MODULE_MANAGER_API PluginProviderUniqueHandle final
{
    std::unique_ptr<PluginProvider> mPtr;
public:
    PluginProviderUniqueHandle() = default;
    explicit PluginProviderUniqueHandle(std::unique_ptr<PluginProvider> ptr)
        : mPtr(std::move(ptr)) { }
    ~PluginProviderUniqueHandle();

    PluginProviderUniqueHandle(PluginProviderUniqueHandle&&) = default;
    PluginProviderUniqueHandle& operator=(PluginProviderUniqueHandle&&) = default;

    PluginProviderUniqueHandle(const PluginProviderUniqueHandle&) = delete;
    PluginProviderUniqueHandle& operator=(const PluginProviderUniqueHandle&) = delete;

    PluginProvider* get() noexcept { return mPtr.get(); }
    const PluginProvider* get() const noexcept { return mPtr.get(); }

    PluginProvider* operator->() noexcept { return mPtr.get(); }
    const PluginProvider* operator->() const noexcept { return mPtr.get(); }
};

using PluginProviderHandlesMap = std::map<wxString, PluginProviderUniqueHandle>;

class MODULE_MANAGER_API ModuleManager final
{
public:

    // -------------------------------------------------------------------------
    // ModuleManager implementation
    // -------------------------------------------------------------------------

    static ModuleManager& Get();

    // This string persists in configuration files
    // So config compatibility will break if it is changed across Audacity versions
    static wxString GetPluginTypeString();

    static PluginID GetID(const PluginProvider* provider);

private:
    static void FindModules(FilePaths& files);
    using DelayedErrors
        =std::vector< std::pair< std::unique_ptr<Module>, wxString > >;
    static void TryLoadModules(
        const FilePaths& files, FilePaths& decided, DelayedErrors& errors);

public:
    void Initialize();
    int Dispatch(ModuleDispatchTypes type);

    // PluginManager use
    // Can be called before Initialize()
    bool DiscoverProviders();

    // Supports range-for iteration
    auto Providers() const
    { return make_iterator_range(mProviders.cbegin(), mProviders.cend()); }

    auto Providers()
    { return make_iterator_range(mProviders.begin(), mProviders.end()); }

    bool RegisterEffectPlugin(const PluginID& provider, const PluginPath& path, TranslatableString& errMsg);

    PluginProvider* CreateProviderInstance(
        const PluginID& provider, const PluginPath& path);
    std::unique_ptr<ComponentInterface>
    LoadPlugin(const PluginID& provider, const PluginPath& path);

    bool IsProviderValid(const PluginID& provider, const PluginPath& path);
    bool CheckPluginExist(const PluginID& providerId, const PluginPath& path);

private:
    // I'm a singleton class
    ModuleManager();
    ~ModuleManager();
    ModuleManager(const ModuleManager&) = delete;
    ModuleManager& operator=(const ModuleManager&) = delete;

    void InitializeBuiltins();

    friend std::unique_ptr<ModuleManager> std::make_unique<ModuleManager>();

    friend std::default_delete<ModuleManager>;
    static std::unique_ptr<ModuleManager> mInstance;

    // Providers can each report availability of any number of Plug-Ins
    // identified by "paths", and are also factories of ComponentInterface
    // objects for each path
    PluginProviderHandlesMap mProviders;

    // Other libraries that receive notifications of events described by
    // ModuleDispatchTypes:
    std::vector<std::unique_ptr<Module> > mModules;
};

// ----------------------------------------------------------------------------
// A factory of PluginProvider objects
// ----------------------------------------------------------------------------
using PluginProviderFactory = std::unique_ptr<PluginProvider>(*)();

MODULE_MANAGER_API
void RegisterProviderFactory(PluginProviderFactory factory);
MODULE_MANAGER_API
void UnregisterProviderFactory(PluginProviderFactory factory);

// Guarantee the registry exists before any registrations, so it will
// be destroyed only after the un-registrations
static struct Init {
    Init() { RegisterProviderFactory(nullptr); }
} sInitBuiltinModules;

#endif /* __AUDACITY_MODULEMANAGER_H__ */
