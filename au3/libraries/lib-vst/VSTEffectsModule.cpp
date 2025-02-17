/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffectsModule.cpp

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.cpp


*//********************************************************************/
#include "VSTEffectsModule.h"

#include "ModuleManager.h"
#include "wxArrayStringEx.h"

#if defined(__WXMSW__)
#include <Windows.h>
#include <shlwapi.h>
#endif

#include <wx/tokenzr.h>
#include <wx/utils.h>

// ============================================================================
//
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
//
// ============================================================================
DECLARE_PROVIDER_ENTRY(AudacityModule)
{
    // Create our effects module and register
    // Trust the module manager not to leak this
    return std::make_unique<VSTEffectsModule>();
}

// ============================================================================
//
// Register this as a builtin module
//
// We also take advantage of the fact that wxModules are initialized before
// the wxApp::OnInit() method is called.  We check to see if Audacity was
// executed to scan a VST effect in a different process.
//
// ============================================================================
DECLARE_BUILTIN_PROVIDER(VSTBuiltin);

///////////////////////////////////////////////////////////////////////////////
///
/// Auto created at program start up, this initialises VST.
///
///////////////////////////////////////////////////////////////////////////////

VSTEffectsModule::VSTEffectsModule()
{
}

VSTEffectsModule::~VSTEffectsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath VSTEffectsModule::GetPath() const
{
    return {};
}

ComponentInterfaceSymbol VSTEffectsModule::GetSymbol() const
{
    return XO("VST Effects");
}

VendorSymbol VSTEffectsModule::GetVendor() const
{
    return XO("The Audacity Team");
}

wxString VSTEffectsModule::GetVersion() const
{
    // This "may" be different if this were to be maintained as a separate DLL
    return AUDACITY_VERSION_STRING;
}

TranslatableString VSTEffectsModule::GetDescription() const
{
    return XO("Adds the ability to use VST effects in Audacity.");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

bool VSTEffectsModule::Initialize()
{
    // Nothing to do here
    return true;
}

void VSTEffectsModule::Terminate()
{
    // Nothing to do here
    return;
}

bool VSTEffectsModule::SupportsCustomModulePaths() const
{
    return true;
}

EffectFamilySymbol VSTEffectsModule::GetOptionalFamilySymbol()
{
    return VSTPLUGINTYPE;
}

const FileExtensions& VSTEffectsModule::GetFileExtensions()
{
    static FileExtensions result{ { _T("vst") } };
    return result;
}

FilePath VSTEffectsModule::InstallPath()
{
    // Not yet ready for VST drag-and-drop...
    // return FileNames::PlugInDir();

    return {};
}

void VSTEffectsModule::AutoRegisterPlugins(PluginManagerInterface&)
{
}

PluginPaths VSTEffectsModule::FindModulePaths(PluginManagerInterface& pm)
{
    FilePaths pathList;
    FilePaths files;

    // Check for the VST_PATH environment variable
    wxString vstpath = wxString::FromUTF8(getenv("VST_PATH"));
    if (!vstpath.empty()) {
        wxStringTokenizer tok(vstpath, wxPATH_SEP);
        while (tok.HasMoreTokens())
        {
            pathList.push_back(tok.GetNextToken());
        }
    }

    const auto AddCustomPaths = [](PluginManagerInterface& pm, VSTEffectsModule& module, FilePaths& pathList)
    {
        const auto customPaths = pm.ReadCustomPaths(module);
        std::copy(customPaths.begin(), customPaths.end(), std::back_inserter(pathList));
    };

#if defined(__WXMAC__)
#define VSTPATH wxT("/Library/Audio/Plug-Ins/VST")

    // Look in ~/Library/Audio/Plug-Ins/VST and /Library/Audio/Plug-Ins/VST
    pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + VSTPATH);
    pathList.push_back(VSTPATH);

    AddCustomPaths(pm, *this, pathList);

    // Recursively search all paths for Info.plist files.  This will identify all
    // bundles.
    pm.FindFilesInPathList(wxT("Info.plist"), pathList, files, true);

    // Remove the 'Contents/Info.plist' portion of the names
    for (size_t i = 0; i < files.size(); i++) {
        files[i] = wxPathOnly(wxPathOnly(files[i]));
        if (!files[i].EndsWith(wxT(".vst"))) {
            files.erase(files.begin() + i--);
        }
    }

#elif defined(__WXMSW__)

    TCHAR dpath[MAX_PATH];
    TCHAR tpath[MAX_PATH];
    DWORD len;

    // Try HKEY_CURRENT_USER registry key first
    len = WXSIZEOF(tpath);
    if (SHRegGetUSValue(wxT("Software\\VST"),
                        wxT("VSTPluginsPath"),
                        NULL,
                        tpath,
                        &len,
                        FALSE,
                        NULL,
                        0) == ERROR_SUCCESS) {
        tpath[len] = 0;
        dpath[0] = 0;
        ExpandEnvironmentStrings(tpath, dpath, WXSIZEOF(dpath));
        pathList.push_back(dpath);
    }

    // Then try HKEY_LOCAL_MACHINE registry key
    len = WXSIZEOF(tpath);
    if (SHRegGetUSValue(wxT("Software\\VST"),
                        wxT("VSTPluginsPath"),
                        NULL,
                        tpath,
                        &len,
                        TRUE,
                        NULL,
                        0) == ERROR_SUCCESS) {
        tpath[len] = 0;
        dpath[0] = 0;
        ExpandEnvironmentStrings(tpath, dpath, WXSIZEOF(dpath));
        pathList.push_back(dpath);
    }

    // Add the default path last
    dpath[0] = 0;
    ExpandEnvironmentStrings(wxT("%ProgramFiles%\\Steinberg\\VSTPlugins"),
                             dpath,
                             WXSIZEOF(dpath));
    pathList.push_back(dpath);

    dpath[0] = 0;
    ExpandEnvironmentStrings(wxT("%COMMONPROGRAMFILES%\\VST2"),
                             dpath,
                             WXSIZEOF(dpath));
    pathList.push_back(dpath);

    AddCustomPaths(pm, *this, pathList);

    // Recursively scan for all DLLs
    pm.FindFilesInPathList(wxT("*.dll"), pathList, files, true);

#else

    // Nothing specified in the VST_PATH environment variable...provide defaults
    if (vstpath.empty()) {
        // We add this "non-default" one
        pathList.push_back(wxT(LIBDIR) wxT("/vst"));

        // These are the defaults used by other hosts
        pathList.push_back(wxT("/usr/lib/vst"));
        pathList.push_back(wxT("/usr/local/lib/vst"));
        pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + wxT(".vst"));
    }

    AddCustomPaths(pm, *this, pathList);

    // Recursively scan for all shared objects
    pm.FindFilesInPathList(wxT("*.so"), pathList, files, true);

#endif

    return { files.begin(), files.end() };
}

unsigned VSTEffectsModule::DiscoverPluginsAtPath(
    const PluginPath& path, TranslatableString& errMsg,
    const RegistrationCallback& callback)
{
    VSTEffectBase effect(path);
    if (effect.InitializePlugin()) {
        auto effectIDs = effect.GetEffectIDs();
        if (effectIDs.empty()) {
            //Each VST plugin path in Audacity should have id(index) part in it
            effectIDs.push_back(0);
        }

        for (auto id : effectIDs) {
            //Subsequent VSTEffect::Load may seem like overhead, but we need
            //to initialize EffectDefinitionInterface part, which includes
            //properly formatted plugin path
            VSTEffectBase subeffect(wxString::Format("%s;%d", path, id));
            subeffect.Load();
            if (callback) {
                callback(this, &subeffect);
            }
        }
        return effectIDs.size();
    }
    errMsg = XO("Could not load the library");
    return 0;
}

std::unique_ptr<ComponentInterface>
VSTEffectsModule::LoadPlugin(const PluginPath& path)
{
    // Acquires a resource for the application.
    // For us, the ID is simply the path to the effect
    auto result = Factory::Call(path);
    if (!result->InitializePlugin()) {
        result.reset();
    }
    return result;
}

bool VSTEffectsModule::CheckPluginExist(const PluginPath& path) const
{
    const auto modulePath = path.BeforeFirst(wxT(';'));
    return wxFileName::FileExists(modulePath) || wxFileName::DirExists(modulePath);
}
