/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleManager.cpp

  Dominic Mazzoni
  James Crook


*******************************************************************//*!

\file ModuleManager.cpp
\brief Based on LoadLadspa, this code loads pluggable Audacity
extension modules.  It also has the code to
invoke a function returning a replacement window,
i.e. an alternative to the usual interface, for Audacity.

*//*******************************************************************/

#include "ModuleManager.h"
#include "PluginProvider.h"

#include "BasicUI.h"

#include <wx/dynlib.h>
#include <wx/log.h>
#include <wx/filename.h>

#include "FileNames.h"
#include "MemoryX.h"

#include "PluginInterface.h"

#include "Prefs.h"
#include "ModuleSettings.h"

#define initFnName      "ExtensionModuleInit"
#define versionFnName   "GetVersionString"

//typedef wxString (*tVersionFn)();
typedef wxChar*(* tVersionFn)();

Module::Module(const FilePath& name)
    : mName{name}
{
    mLib = std::make_unique<wxDynamicLibrary>();
    mDispatch = NULL;
}

Module::~Module()
{
    // DV: The current Registry code makes unloading of the modules
    // impossible. The order in which static objects are destroyed
    // may result in the Registry instance being destroyed after the ModuleManager.
    // The way Audacity is currently implemented, it is not possible to
    // guarantee that the ModuleManager instance is initialized before
    // any of the Registry instances.
    if (mLib != nullptr && mLib->IsLoaded()) {
        mLib->Detach();
    }
}

static BasicUI::MessageBoxResult DoMessageBox(const TranslatableString& msg)
{
    using namespace BasicUI;
    return ShowMessageBox(msg,
                          MessageBoxOptions {}.Caption(XO("Module Unsuitable")));
}

void Module::ShowLoadFailureError(const wxString& Error)
{
    auto ShortName = wxFileName(mName).GetName();
    DoMessageBox(
        XO("Unable to load the \"%s\" module.\n\nError: %s")
        .Format(ShortName, Error));
    wxLogMessage(wxT("Unable to load the module \"%s\". Error: %s"), mName, Error);
}

bool Module::Load(wxString& deferredErrorMessage)
{
    deferredErrorMessage.clear();
    // Will this ever happen???
    if (mLib->IsLoaded()) {
        if (mDispatch) {
            return true;
        }

        // Any messages should have already been generated the first time it was loaded.
        return false;
    }

    auto ShortName = wxFileName(mName).GetName();

    if (!mLib->Load(mName, wxDL_NOW | wxDL_QUIET | wxDL_GLOBAL)) {
        // For this failure path, only, there is a possibility of retrial
        // after some other dependency of this module is loaded.  So the
        // error is not immediately reported.
        deferredErrorMessage = wxString(wxSysErrorMsg());
        return false;
    }

    // Check version string matches.  (For now, they must match exactly)
    tVersionFn versionFn = (tVersionFn)(mLib->GetSymbol(wxT(versionFnName)));
    if (versionFn == NULL) {
        DoMessageBox(
            XO("The module \"%s\" does not provide a version string.\n\nIt will not be loaded.")
            .Format(ShortName));
        wxLogMessage(wxT("The module \"%s\" does not provide a version string. It will not be loaded."), mName);
        mLib->Unload();
        return false;
    }

    wxString moduleVersion = versionFn();
    if (moduleVersion != AUDACITY_VERSION_STRING) {
        DoMessageBox(
            XO("The module \"%s\" is matched with Audacity version \"%s\".\n\nIt will not be loaded.")
            .Format(ShortName, moduleVersion));
        wxLogMessage(wxT("The module \"%s\" is matched with Audacity version \"%s\". It will not be loaded."), mName, moduleVersion);
        mLib->Unload();
        return false;
    }

    mDispatch = (fnModuleDispatch)mLib->GetSymbol(wxT(ModuleDispatchName));
    if (!mDispatch) {
        // Module does not provide a dispatch function.
        return true;
    }

    // However if we do have it and it does not work,
    // then the module is bad.
    bool res = ((mDispatch(ModuleInitialize)) != 0);
    if (res) {
        return true;
    }

    mDispatch = NULL;

    DoMessageBox(
        XO("The module \"%s\" failed to initialize.\n\nIt will not be loaded.")
        .Format(ShortName));
    wxLogMessage(wxT("The module \"%s\" failed to initialize.\nIt will not be loaded."), mName);
    mLib->Unload();

    return false;
}

// This isn't yet used?
void Module::Unload()
{
    if (mLib->IsLoaded()) {
        if (mDispatch) {
            mDispatch(ModuleTerminate);
        }
    }

    mLib->Unload();
}

int Module::Dispatch(ModuleDispatchTypes type)
{
    if (mLib->IsLoaded()) {
        if (mDispatch != NULL) {
            return mDispatch(type);
        }
    }

    return 0;
}

void* Module::GetSymbol(const wxString& name)
{
    return mLib->GetSymbol(name);
}

// ============================================================================
//
// ModuleManager
//
// ============================================================================

// The one and only ModuleManager
std::unique_ptr<ModuleManager> ModuleManager::mInstance{};

// Give builtin providers a means to identify themselves
using BuiltinProviderList = std::vector<PluginProviderFactory>;
namespace {
BuiltinProviderList& builtinProviderList()
{
    static BuiltinProviderList theList;
    return theList;
}
}

void RegisterProviderFactory(PluginProviderFactory pluginProviderFactory)
{
    auto& list = builtinProviderList();
    if (pluginProviderFactory) {
        list.push_back(std::move(pluginProviderFactory));
    }
}

void UnregisterProviderFactory(PluginProviderFactory pluginProviderFactory)
{
    auto& list = builtinProviderList();
    auto end = list.end(), iter = std::find(list.begin(), end, pluginProviderFactory);
    if (iter != end) {
        list.erase(iter);
    }
}

// ----------------------------------------------------------------------------
// Creation/Destruction
// ----------------------------------------------------------------------------

ModuleManager::ModuleManager()
{
}

ModuleManager::~ModuleManager()
{
    mProviders.clear();
    builtinProviderList().clear();
}

// static
void ModuleManager::FindModules(FilePaths& files)
{
    const auto& audacityPathList = FileNames::AudacityPathList();
    FilePaths pathList;
    wxString pathVar;

    // Code from LoadLadspa that might be useful in load modules.
    pathVar = wxGetenv(wxT("AUDACITY_MODULES_PATH"));
    if (!pathVar.empty()) {
        FileNames::AddMultiPathsToPathList(pathVar, pathList);
    }

    for (const auto& path : audacityPathList) {
        wxString prefix = path + wxFILE_SEP_PATH;
        FileNames::AddUniquePathToPathList(prefix + wxT("modules"),
                                           pathList);
        if (files.size()) {
            break;
        }
    }

   #if defined(__WXMSW__)
    FileNames::FindFilesInPathList(wxT("*.dll"), pathList, files);
   #else
    FileNames::FindFilesInPathList(wxT("*.so"), pathList, files);
   #endif
}

void ModuleManager::TryLoadModules(
    const FilePaths& files, FilePaths& decided, DelayedErrors& errors)
{
    FilePaths checked;
    wxString saveOldCWD = ::wxGetCwd();
    auto cleanup = finally([&]{ ::wxSetWorkingDirectory(saveOldCWD); });
    for (const auto& file : files) {
        // As a courtesy to some modules that might be bridges to
        // open other modules, we set the current working
        // directory to be the module's directory.
        auto prefix = ::wxPathOnly(file);
        ::wxSetWorkingDirectory(prefix);

        // Only process the first module encountered in the
        // defined search sequence.
        wxString ShortName = wxFileName(file).GetName();
        if (checked.Index(ShortName, false) != wxNOT_FOUND) {
            continue;
        }
        checked.Add(ShortName);

        // Skip if a previous pass through this function decided it already
        if (decided.Index(ShortName, false) != wxNOT_FOUND) {
            continue;
        }

        int iModuleStatus = ModuleSettings::GetModuleStatus(file);
        if (iModuleStatus == kModuleDisabled) {
            continue;
        }
        if (iModuleStatus == kModuleFailed) {
            continue;
        }
        // New module?  You have to go and explicitly enable it.
        if (iModuleStatus == kModuleNew) {
            // To ensure it is noted in config file and so
            // appears on modules page.
            ModuleSettings::SetModuleStatus(file, kModuleNew);
            continue;
        }

        if (iModuleStatus == kModuleAsk) {
            // JKC: I don't like prompting for the plug-ins individually
            // I think it would be better to show the module prefs page,
            // and let the user decide for each one.
            auto msg = XO("Module \"%s\" found.").Format(ShortName);
            msg += XO("\n\nOnly use modules from trusted sources");
            const TranslatableStrings buttons{
                XO("Yes"), XO("No"),
            }; // could add a button here for 'yes and remember that', and put it into the cfg file.  Needs more thought.
            int action = BasicUI::ShowMultiDialog(msg,
                                                  XO("Audacity Module Loader"),
                                                  buttons,
                                                  "",
                                                  XO("Try and load this module?"),
                                                  false);
            // If we're not prompting always, accept the answer permanently
            if (iModuleStatus == kModuleNew) {
                iModuleStatus = (action == 1) ? kModuleDisabled : kModuleEnabled;
                ModuleSettings::SetModuleStatus(file, iModuleStatus);
            }
            if (action == 1) { // "No"
                decided.Add(ShortName);
                continue;
            }
        }
        // Before attempting to load, we set the state to bad.
        // That way, if we crash, we won't try again.
        ModuleSettings::SetModuleStatus(file, kModuleFailed);

        wxString Error;
        auto umodule = std::make_unique<Module>(file);
        if (umodule->Load(Error)) {  // it will get rejected if there are version problems
            decided.Add(ShortName);
            auto module = umodule.get();

            if (!module->HasDispatch()) {
                auto ShortName = wxFileName(file).GetName();
                DoMessageBox(
                    XO("The module \"%s\" does not provide any of the required functions.\n\nIt will not be loaded.")
                    .Format(ShortName));
                wxLogMessage(wxT("The module \"%s\" does not provide any of the required functions. It will not be loaded."), file);
                module->Unload();
            } else {
                Get().mModules.push_back(std::move(umodule));

                // Loaded successfully, restore the status.
                ModuleSettings::SetModuleStatus(file, iModuleStatus);
            }
        } else if (!Error.empty()) {
            // Module is not yet decided in this pass.
            // Maybe it depends on another which has not yet been loaded.
            // But don't take the kModuleAsk path again in a later pass.
            ModuleSettings::SetModuleStatus(file, kModuleEnabled);
            errors.emplace_back(std::move(umodule), Error);
        }
    }
}

// static
void ModuleManager::Initialize()
{
    FilePaths files;
    FindModules(files);

    FilePaths decided;
    DelayedErrors errors;
    size_t numDecided = 0;

    // Multiple passes give modules multiple chances to load in case they
    // depend on some other module not yet loaded
    do {
        numDecided = decided.size();
        errors.clear();
        TryLoadModules(files, decided, errors);
    }while (errors.size() && numDecided < decided.size());

    // Only now show accumulated errors of modules that failed to load
    for ( const auto& pair : errors ) {
        auto& pModule = pair.first;
        pModule->ShowLoadFailureError(pair.second);
        ModuleSettings::SetModuleStatus(pModule->GetName(), kModuleFailed);
    }
}

// static
int ModuleManager::Dispatch(ModuleDispatchTypes type)
{
    for (const auto& module: mModules) {
        module->Dispatch(type);
    }
    return 0;
}

PluginProviderUniqueHandle::~PluginProviderUniqueHandle()
{
    if (mPtr) {
        mPtr->Terminate();
        //No profit in comparison to calling/performing PluginProvider::Terminate
        //from a destructor of the PluginProvider, since we don't offer any
        //options to deal with errors...
        //
        //Example:
        //try {
        //   provider->Terminate();
        //}
        //catch(e) {
        //   if(Dialog::ShowError("... Are you sure?") != Dialog::ResultOk)
        //      //other providers might have been terminated by that time,
        //      //so it might be a better option to repeatedly ask "Try again"/"Continue"
        //      return;
        //}
        //provider.reset();//no errors, or user confirmed deletion
    }
}

// ============================================================================
//
// Return reference to singleton
//
// (Thread-safe...no active threading during construction or after destruction)
// ============================================================================
ModuleManager& ModuleManager::Get()
{
    if (!mInstance) {
        mInstance = std::make_unique<ModuleManager>();
    }

    return *mInstance;
}

wxString ModuleManager::GetPluginTypeString()
{
    return L"Module";
}

PluginID ModuleManager::GetID(const PluginProvider* provider)
{
    return wxString::Format(wxT("%s_%s_%s_%s_%s"),
                            GetPluginTypeString(),
                            wxEmptyString,
                            provider->GetVendor().Internal(),
                            provider->GetSymbol().Internal(),
                            provider->GetPath());
}

bool ModuleManager::DiscoverProviders()
{
    InitializeBuiltins();

// The commented out code loads modules whether or not they are enabled.
// none of our modules is a 'provider' of effects, so this code commented out.
#if 0
    FilePaths provList;
    FilePaths pathList;

    // Code from LoadLadspa that might be useful in load modules.
    wxString pathVar = wxString::FromUTF8(getenv("AUDACITY_MODULES_PATH"));

    if (!pathVar.empty()) {
        FileNames::AddMultiPathsToPathList(pathVar, pathList);
    } else {
        FileNames::AddUniquePathToPathList(FileNames::ModulesDir(), pathList);
    }

#if defined(__WXMSW__)
    FileNames::FindFilesInPathList(wxT("*.dll"), pathList, provList);
#elif defined(__WXMAC__)
    FileNames::FindFilesInPathList(wxT("*.dylib"), pathList, provList);
#else
    FileNames::FindFilesInPathList(wxT("*.so"), pathList, provList);
#endif

    for ( const auto& path : provList ) {
        LoadModule(path);
    }
#endif

    return true;
}

void ModuleManager::InitializeBuiltins()
{
    for (const auto& pluginProviderFactory : builtinProviderList()) {
        auto pluginProvider = pluginProviderFactory();

        if (pluginProvider && pluginProvider->Initialize()) {
            PluginProviderUniqueHandle handle { std::move(pluginProvider) };

            auto id = GetID(handle.get());

            // Need to remember it
            mProviders[id] = std::move(handle);
        }
    }
}

bool ModuleManager::RegisterEffectPlugin(const PluginID& providerID, const PluginPath& path, TranslatableString& errMsg)
{
    errMsg = {};
    if (mProviders.find(providerID) == mProviders.end()) {
        return false;
    }

    auto nFound = mProviders[providerID]->DiscoverPluginsAtPath(path, errMsg, PluginManagerInterface::DefaultRegistrationCallback);

    return nFound > 0;
}

PluginProvider* ModuleManager::CreateProviderInstance(const PluginID& providerID,
                                                      const PluginPath& path)
{
    if (path.empty() && mProviders.find(providerID) != mProviders.end()) {
        return mProviders[providerID].get();
    }

    return nullptr;
}

std::unique_ptr<ComponentInterface> ModuleManager::LoadPlugin(
    const PluginID& providerID, const PluginPath& path)
{
    if (auto iter = mProviders.find(providerID);
        iter == mProviders.end()) {
        return nullptr;
    } else {
        return iter->second->LoadPlugin(path);
    }
}

bool ModuleManager::CheckPluginExist(const PluginID& providerId, const PluginPath& path)
{
    if (mProviders.find(providerId) == mProviders.end()) {
        return false;
    }

    return mProviders[providerId]->CheckPluginExist(path);
}

bool ModuleManager::IsProviderValid(const PluginID& WXUNUSED(providerID),
                                    const PluginPath& path)
{
    // Builtin modules do not have a path
    if (path.empty()) {
        return true;
    }

    wxFileName lib(path);
    if (lib.FileExists() || lib.DirExists()) {
        return true;
    }

    return false;
}
