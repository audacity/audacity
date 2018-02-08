/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadCommands.cpp

  Dominic Mazzoni
  James Crook

/**************************************************************************//**
\class BuiltinCommandsModule
\brief Internal module to auto register all built in commands.  It is closely
modelled on BuiltinEffectsModule
********************************************************************************/

#include "../Audacity.h"
#include "../Prefs.h"

#include "LoadCommands.h"
#include "../MemoryX.h"

#include "../effects/EffectManager.h"
#include "Demo.h"
#include "../Experimental.h"
#include "../commands/ScreenshotCommand.h"
#include "../commands/CompareAudioCommand.h"
#include "../commands/SetTrackInfoCommand.h"
#include "../commands/GetTrackInfoCommand.h"
#include "../commands/SelectCommand.h"
#include "../commands/PreferenceCommands.h"
#include "../commands/GetInfoCommand.h"
#include "../commands/HelpCommand.h"
#include "../commands/ImportExportCommands.h"
#include "../commands/OpenSaveCommands.h"

//
// Define the list of COMMANDs that will be autoregistered and how to instantiate each
//
#define COMMAND_LIST \
   COMMAND( DEMO,              DemoCommand, () )           \
   COMMAND( SCREENSHOT,        ScreenshotCommand,   () )   \
   COMMAND( COMPARE_AUDIO,     CompareAudioCommand, () )   \
   COMMAND( SET_TRACK_INFO,    SetTrackInfoCommand, () )   \
   COMMAND( SELECT,            SelectCommand, () )         \
   COMMAND( SELECT_TIME,       SelectTimeCommand, () )     \
   COMMAND( SELECT_TRACKS,     SelectTracksCommand, () )   \
   COMMAND( GET_PREFERENCE,    GetPreferenceCommand, () )  \
   COMMAND( SET_PREFERENCE,    SetPreferenceCommand, () )  \
   COMMAND( GET_INFO,          GetInfoCommand, () )        \
   COMMAND( HELP,              HelpCommand, () )           \
   COMMAND( IMPORT,            ImportCommand, () )         \
   COMMAND( EXPORT,            ExportCommand, () )         \
   COMMAND( OPEN_PROJECT,      OpenProjectCommand, () )    \
   COMMAND( SAVE_PROJECT,      SaveProjectCommand, () )    \

   // GET_TRACK_INFO subsumed by GET_INFO
   //COMMAND( GET_TRACK_INFO,    GetTrackInfoCommand, () )   


//
// Define the list of COMMANDs that do not get autoregistered
//
#define EXCLUDE_LIST \


//
// Define the COMMAND() macro to generate enum names
//
#define COMMAND(n, i, args) ENUM_ ## n,

//
// Create the enum for the list of COMMANDs (will be used in a switch statement)
//
enum
{
   COMMAND_LIST
   EXCLUDE_LIST
};

//
// Redefine COMMAND() to add the COMMAND's name to an array
//
#undef COMMAND
#define COMMAND(n, i, args) n ## _PLUGIN_SYMBOL,

//
// Create the COMMAND name array
//
static const wxChar *kCOMMANDNames[] =
{
   COMMAND_LIST
};

/*
//
// Create the COMMAND name array of excluded COMMANDs
//
static const wxChar *kExcludedNames[] =
{
   EXCLUDE_LIST
};
*/
//
// Redefine COMMAND() to generate a case statement for the lookup switch
//
#undef COMMAND
#define COMMAND(n, i, args) case ENUM_ ## n: return std::make_unique<i> args;

// ============================================================================
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
// ============================================================================
DECLARE_MODULE_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return safenew BuiltinCommandsModule(moduleManager, path);
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_MODULE(BuiltinsCommandBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// BuiltinCommandsModule
//
///////////////////////////////////////////////////////////////////////////////

BuiltinCommandsModule::BuiltinCommandsModule(ModuleManagerInterface *moduleManager,
                                           const wxString *path)
{
   mModMan = moduleManager;
   if (path)
   {
      mPath = *path;
   }
}

BuiltinCommandsModule::~BuiltinCommandsModule()
{
   mPath.Clear();
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString BuiltinCommandsModule::GetPath()
{
   return mPath;
}

wxString BuiltinCommandsModule::GetSymbol()
{
   return XO("Builtin Commands");
}

wxString BuiltinCommandsModule::GetName()
{
   return XO("Builtin Commands");
}

wxString BuiltinCommandsModule::GetVendor()
{
   return XO("The Audacity Team");
}

wxString BuiltinCommandsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return AUDACITY_VERSION_STRING;
}

wxString BuiltinCommandsModule::GetDescription()
{
   return _("Provides builtin commands to Audacity");
}

// ============================================================================
// ModuleInterface implementation
// ============================================================================

bool BuiltinCommandsModule::Initialize()
{
   for (size_t i = 0; i < WXSIZEOF(kCOMMANDNames); i++)
   {
      wxLogDebug("Adding %s", kCOMMANDNames[i] );
      mNames.Add(wxString(BUILTIN_GENERIC_COMMAND_PREFIX) + kCOMMANDNames[i]);
   }

/*
   for (size_t i = 0; i < WXSIZEOF(kExcludedNames); i++)
   {
      mNames.Add(wxString(BUILTIN_COMMAND_PREFIX) + kExcludedNames[i]);
   }
*/

   return true;
}

void BuiltinCommandsModule::Terminate()
{
   // Nothing to do here
   return;
}

bool BuiltinCommandsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   wxString ignoredErrMsg;
   for (size_t i = 0; i < WXSIZEOF(kCOMMANDNames); i++)
   {
      wxString path(wxString(BUILTIN_GENERIC_COMMAND_PREFIX) + kCOMMANDNames[i]);

      if (!pm.IsPluginRegistered(path))
      {
         // No checking of error ?
         // Uses Generic Registration, not Default.
         // Registers as TypeGeneric, not TypeEffect.
         DiscoverPluginsAtPath(path, ignoredErrMsg,
            PluginManagerInterface::GenericRegistrationCallback);
      }
   }

   // We still want to be called during the normal registration process
   return false;
}

wxArrayString BuiltinCommandsModule::FindPluginPaths(PluginManagerInterface & WXUNUSED(pm))
{
   return mNames;
}

unsigned BuiltinCommandsModule::DiscoverPluginsAtPath(
   const wxString & path, wxString &errMsg,
   const RegistrationCallback &callback)
{
   errMsg.clear();
   auto Command = Instantiate(path);
   if (Command)
   {
      if (callback){
         callback(this, Command.get());
      }
      return 1;
   }

   errMsg = _("Unknown built-in command name");
   return 0;
}

bool BuiltinCommandsModule::IsPluginValid(const wxString & path, bool bFast)
{
   // bFast is unused as checking in the list is fast.
   bFast;
   return mNames.Index(path) != wxNOT_FOUND;
}

IdentInterface *BuiltinCommandsModule::CreateInstance(const wxString & path)
{
   // Acquires a resource for the application.
   // Safety of this depends on complementary calls to DeleteInstance on the module manager side.
   return Instantiate(path).release();
}

void BuiltinCommandsModule::DeleteInstance(IdentInterface *instance)
{
   // Releases the resource.
   std::unique_ptr < AudacityCommand > {
      dynamic_cast<AudacityCommand *>(instance)
   };
}

// ============================================================================
// BuiltinCommandsModule implementation
// ============================================================================

std::unique_ptr<AudacityCommand> BuiltinCommandsModule::Instantiate(const wxString & path)
{
   wxASSERT(path.StartsWith(BUILTIN_GENERIC_COMMAND_PREFIX));
   wxASSERT(mNames.Index(path) != wxNOT_FOUND);

   switch (mNames.Index(path))
   {
      COMMAND_LIST;
      EXCLUDE_LIST;
   }

   return nullptr;
}
