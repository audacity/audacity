/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadCommands.cpp

  Dominic Mazzoni
  James Crook

**************************************************************************//**
\class BuiltinCommandsModule
\brief Internal module to auto register all built in commands.  It is closely
modelled on BuiltinEffectsModule
*****************************************************************************/

#include "../Audacity.h"
#include "LoadCommands.h"

#include "../Prefs.h"

#include "../MemoryX.h"

#include "../effects/EffectManager.h"
#include "Demo.h"
#include "../commands/MessageCommand.h"
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
#include "../commands/SetLabelCommand.h"
#include "../commands/SetEnvelopeCommand.h"
#include "../commands/SetClipCommand.h"
#include "../commands/SetProjectCommand.h"
#include "../commands/DragCommand.h"

//
// Define the list of COMMANDs that will be autoregistered and how to instantiate each
//
#define COMMAND_LIST \
   COMMAND( DEMO,                DemoCommand, () )             \
   COMMAND( MESSAGE,             MessageCommand,   () )        \
   COMMAND( SCREENSHOT,          ScreenshotCommand,   () )     \
   COMMAND( DRAG,                DragCommand, () )             \
   COMMAND( COMPARE_AUDIO,       CompareAudioCommand, () )     \
   COMMAND( SET_TRACK,           SetTrackCommand, () )         \
   COMMAND( SET_TRACK_STATUS,    SetTrackStatusCommand, () )   \
   COMMAND( SET_TRACK_AUDIO,     SetTrackAudioCommand, () )    \
   COMMAND( SET_TRACK_VISUALS,   SetTrackVisualsCommand, () )  \
   COMMAND( SET_ENVELOPE,        SetEnvelopeCommand, () )      \
   COMMAND( SET_CLIP,            SetClipCommand, () )          \
   COMMAND( SET_LABEL,           SetLabelCommand, () )         \
   COMMAND( SET_PROJECT,         SetProjectCommand, () )       \
   COMMAND( SELECT,              SelectCommand, () )           \
   COMMAND( SELECT_TIME,         SelectTimeCommand, () )       \
   COMMAND( SELECT_FREQUENCIES,  SelectFrequenciesCommand, () )\
   COMMAND( SELECT_TRACKS,       SelectTracksCommand, () )     \
   COMMAND( GET_PREFERENCE,      GetPreferenceCommand, () )    \
   COMMAND( SET_PREFERENCE,      SetPreferenceCommand, () )    \
   COMMAND( GET_INFO,            GetInfoCommand, () )          \
   COMMAND( HELP,                HelpCommand, () )             \
   COMMAND( IMPORT,              ImportCommand, () )           \
   COMMAND( EXPORT,              ExportCommand, () )           \
   COMMAND( OPEN_PROJECT,        OpenProjectCommand, () )      \
   COMMAND( SAVE_PROJECT,        SaveProjectCommand, () )      \

   // GET_TRACK_INFO subsumed by GET_INFO
   //COMMAND( GET_TRACK_INFO,    GetTrackInfoCommand, () )   
   // SELECT_TIME and SELECT_TRACKS subsumed by SELECT
   //COMMAND( SELECT_TIME,       SelectTimeCommand, () )     
   //COMMAND( SELECT_TRACKS,     SelectTracksCommand, () )   


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
#define COMMAND(n, i, args) results.push_back( (n ## _PLUGIN_SYMBOL).Internal() );

//
// Create the COMMAND name array
//
static const std::vector<wxString> kCOMMANDNames()
{
   std::vector<wxString> results;
   COMMAND_LIST;
   return results;
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
   mPath.clear();
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath BuiltinCommandsModule::GetPath()
{
   return mPath;
}

ComponentInterfaceSymbol BuiltinCommandsModule::GetSymbol()
{
   return XO("Builtin Commands");
}

VendorSymbol BuiltinCommandsModule::GetVendor()
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
   const auto &names = kCOMMANDNames();
   for (const auto &name : names)
   {
      //wxLogDebug("Adding %s", name );
      mNames.push_back(wxString(BUILTIN_GENERIC_COMMAND_PREFIX) + name);
   }

/*
   for (size_t i = 0; i < WXSIZEOF(kExcludedNames); i++)
   {
      mNames.push_back(wxString(BUILTIN_COMMAND_PREFIX) + kExcludedNames[i]);
   }
*/

   return true;
}

void BuiltinCommandsModule::Terminate()
{
   // Nothing to do here
   return;
}

const FileExtensions &BuiltinCommandsModule::GetFileExtensions()
{
   static FileExtensions empty;
   return empty;
}

bool BuiltinCommandsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   wxString ignoredErrMsg;
   const auto &names = kCOMMANDNames();
   for (const auto &name : names)
   {
      wxString path(wxString(BUILTIN_GENERIC_COMMAND_PREFIX) + name);

      if (!pm.IsPluginRegistered(path))
      {
         // No checking of error ?
         // Uses Generic Registration, not Default.
         // Registers as TypeGeneric, not TypeEffect.
         DiscoverPluginsAtPath(path, ignoredErrMsg,
            PluginManagerInterface::AudacityCommandRegistrationCallback);
      }
   }

   // We still want to be called during the normal registration process
   return false;
}

PluginPaths BuiltinCommandsModule::FindPluginPaths(PluginManagerInterface & WXUNUSED(pm))
{
   return mNames;
}

unsigned BuiltinCommandsModule::DiscoverPluginsAtPath(
   const PluginPath & path, wxString &errMsg,
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

bool BuiltinCommandsModule::IsPluginValid(const PluginPath & path, bool bFast)
{
   // bFast is unused as checking in the list is fast.
   static_cast<void>(bFast); // avoid unused variable warning
   return make_iterator_range( mNames ).contains( path );
}

ComponentInterface *BuiltinCommandsModule::CreateInstance(const PluginPath & path)
{
   // Acquires a resource for the application.
   // Safety of this depends on complementary calls to DeleteInstance on the module manager side.
   return Instantiate(path).release();
}

void BuiltinCommandsModule::DeleteInstance(ComponentInterface *instance)
{
   // Releases the resource.
   std::unique_ptr < AudacityCommand > {
      dynamic_cast<AudacityCommand *>(instance)
   };
}

// ============================================================================
// BuiltinCommandsModule implementation
// ============================================================================

std::unique_ptr<AudacityCommand> BuiltinCommandsModule::Instantiate(const PluginPath & path)
{
   wxASSERT(path.StartsWith(BUILTIN_GENERIC_COMMAND_PREFIX));
   auto index = make_iterator_range( mNames ).index( path );
   wxASSERT( index != wxNOT_FOUND );

   switch ( index )
   {
      COMMAND_LIST;
      EXCLUDE_LIST;
   }

   return nullptr;
}
