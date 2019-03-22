/**********************************************************************

  Audacity: A Digital Audio Editor

  MacroCommands.cpp

  Dominic Mazzoni
  James Crook

********************************************************************//*!

\class MacroCommands
\brief Maintains the list of commands for batch/macro 
processing.  See also MacrosWindow and ApplyMacroDialog.

*//*******************************************************************/


#include "Audacity.h" // for USE_* macros
#include "BatchCommands.h"

#include <wx/defs.h>
#include <wx/dir.h>
#include <wx/filedlg.h>
#include <wx/textfile.h>

#include "AudacityApp.h"
#include "Project.h"
#include "commands/CommandManager.h"
#include "effects/EffectManager.h"
#include "FileNames.h"
#include "Internat.h"
#include "Menus.h"
#include "PluginManager.h"
#include "Prefs.h"
#include "Shuttle.h"
#include "export/ExportFLAC.h"
#include "export/ExportMP3.h"
#include "export/ExportOGG.h"
#include "export/ExportPCM.h"

#include "Theme.h"
#include "AllThemeResources.h"

#include "Track.h"
#include "widgets/ErrorDialog.h"

#include "commands/CommandFunctors.h"
#include "commands/CommandContext.h"

// KLUDGE: All commands should be on the same footing
// however, for historical reasons we distinguish between
//    - Effects (which are looked up in effects lists)
//    - Menu commands (which are held in command manager)
//    - Specials (which we deal with specially here)
enum eCommandType { CtEffect, CtMenu, CtSpecial };

// TIDY-ME: Not currently translated,
// but there are issues to address if we do.
// CLEANSPEECH remnant
static const std::pair<const wxChar*, CommandID> SpecialCommands[] = {
   // Use translations of the first members, some other day.
   // For 2.2.2 we'll get them into the catalog at least.

   { XO("No Action"),            wxT("NoAction") },

   // { wxT("Import"), wxT("Import") },   // non-functioning
   /* i18n-hint: before is adverb; MP3 names an audio file format */
   { XO("Export as MP3 56k before"), wxT("ExportMP3_56k_before") },

   /* i18n-hint: after is adverb; MP3 names an audio file format */
   { XO("Export as MP3 56k after"),  wxT("ExportMP3_56k_after") },

   /* i18n-hint: FLAC names an audio file format */
   { XO("Export as FLAC"),          wxT("ExportFLAC") },

// MP3 OGG and WAV already handled by menu items.
#if 0
   /* i18n-hint: MP3 names an audio file format */
   { XO("Export as MP3"),           wxT("ExportMP3") },

   /* i18n-hint: Ogg names an audio file format */
   { XO("Export as Ogg"),           wxT("ExportOgg") },

   /* i18n-hint: WAV names an audio file format */
   { XO("Export as WAV"),           wxT("ExportWAV") },
#endif
};
// end CLEANSPEECH remnant

MacroCommands::MacroCommands()
{
   ResetMacro();

   auto names = GetNames();
   auto defaults = GetNamesOfDefaultMacros();

   for( size_t i = 0;i<defaults.size();i++){
      wxString name = defaults[i];
      if ( ! make_iterator_range( names ).contains(name) ) {
         AddMacro(name);
         RestoreMacro(name);
         WriteMacro(name);
      }
   }
}

static const wxString MP3Conversion = XO("MP3 Conversion");
static const wxString FadeEnds      = XO("Fade Ends");
static const wxString SelectToEnds  = XO("Select to Ends");


wxArrayStringEx MacroCommands::GetNamesOfDefaultMacros()
{
   return {
      GetCustomTranslation( MP3Conversion ) ,
      GetCustomTranslation( FadeEnds ) ,
      //Don't add this one anymore, as there is a new menu command for it.
      //GetCustomTranslation( SelectToEnds ) ,
   };
}

void MacroCommands::RestoreMacro(const wxString & name)
{
// TIDY-ME: Effects change their name with localisation.
// Commands (at least currently) don't.  Messy.
   ResetMacro();
   if (name == GetCustomTranslation( MP3Conversion ) ){
        AddToMacro( wxT("Normalize") );
        AddToMacro( wxT("ExportMP3") );
   } else if (name == GetCustomTranslation( FadeEnds ) ){
        AddToMacro( wxT("Select"), wxT("Start=\"0\" End=\"1\"") );
        AddToMacro( wxT("FadeIn") );
        AddToMacro( wxT("Select"), wxT("Start=\"0\" End=\"1\" RelativeTo=\"ProjectEnd\"") );
        AddToMacro( wxT("FadeOut") );
        AddToMacro( wxT("Select"), wxT("Start=\"0\" End=\"0\"") );
   } else if (name == GetCustomTranslation( SelectToEnds ) ){
        AddToMacro( wxT("SelCursorEnd") );
        AddToMacro( wxT("SelStartCursor") );
   } 
}

CommandID MacroCommands::GetCommand(int index)
{
   if (index < 0 || index >= (int)mCommandMacro.size()) {
      return wxT("");
   }

   return mCommandMacro[index];
}

wxString MacroCommands::GetParams(int index)
{
   if (index < 0 || index >= (int)mParamsMacro.size()) {
      return wxT("");
   }

   return mParamsMacro[index];
}

int MacroCommands::GetCount()
{
   return (int)mCommandMacro.size();
}

bool MacroCommands::ReadMacro(const wxString & macro)
{
   // Clear any previous macro
   ResetMacro();

   // Build the filename
   wxFileName name(FileNames::MacroDir(), macro, wxT("txt"));

   // Set the file name
   wxTextFile tf(name.GetFullPath());

   // Open and check
   tf.Open();
   if (!tf.IsOpened()) {
      // wxTextFile will display any errors
      return false;
   }

   // Load commands from the file
   int lines = tf.GetLineCount();
   if (lines > 0) {
      for (int i = 0; i < lines; i++) {

         // Find the command name terminator...ingore line if not found
         int splitAt = tf[i].Find(wxT(':'));
         if (splitAt < 0) {
            continue;
         }

         // Parse and clean
         wxString cmd = tf[i].Left(splitAt).Strip(wxString::both);
         wxString parm = tf[i].Mid(splitAt + 1).Strip(wxString::trailing);

         // Add to lists
         mCommandMacro.push_back(cmd);
         mParamsMacro.push_back(parm);
      }
   }

   // Done with the file
   tf.Close();

   return true;
}


bool MacroCommands::WriteMacro(const wxString & macro)
{
   // Build the filename
   wxFileName name(FileNames::MacroDir(), macro, wxT("txt"));

   // Set the file name
   wxTextFile tf(name.GetFullPath());

   // Create the file (Create() doesn't leave the file open)
   if (!tf.Exists()) {
      tf.Create();
   }

   // Open it
   tf.Open();

   if (!tf.IsOpened()) {
      // wxTextFile will display any errors
      return false;
   }

   // Start with a clean slate
   tf.Clear();

   // Copy over the commands
   int lines = mCommandMacro.size();
   for (int i = 0; i < lines; i++) {
      tf.AddLine(mCommandMacro[i] + wxT(":") + mParamsMacro[ i ]);
   }

   // Write the macro
   tf.Write();

   // Done with the file
   tf.Close();

   return true;
}

bool MacroCommands::AddMacro(const wxString & macro)
{
   // Build the filename
   wxFileName name(FileNames::MacroDir(), macro, wxT("txt"));

   // Set the file name
   wxTextFile tf(name.GetFullPath());

   // Create it..Create will display errors
   return tf.Create();
}

bool MacroCommands::DeleteMacro(const wxString & macro)
{
   // Build the filename
   wxFileName name(FileNames::MacroDir(), macro, wxT("txt"));

   // Delete it...wxRemoveFile will display errors
   auto result = wxRemoveFile(name.GetFullPath());

   // Delete any legacy chain that it shadowed
   auto oldPath = wxFileName{ FileNames::LegacyChainDir(), macro, wxT("txt") };
   wxRemoveFile(oldPath.GetFullPath()); // Don't care about this return value

   return result;
}

bool MacroCommands::RenameMacro(const wxString & oldmacro, const wxString & newmacro)
{
   // Build the filenames
   wxFileName oname(FileNames::MacroDir(), oldmacro, wxT("txt"));
   wxFileName nname(FileNames::MacroDir(), newmacro, wxT("txt"));

   // Rename it...wxRenameFile will display errors
   return wxRenameFile(oname.GetFullPath(), nname.GetFullPath());
}

// Gets all commands that are valid for this mode.
MacroCommandsCatalog::MacroCommandsCatalog( const AudacityProject *project )
{
   if (!project)
      return;

   // CLEANSPEECH remnant
   Entries commands;
   for( const auto &command : SpecialCommands )
      commands.push_back( {
         { command.second, GetCustomTranslation( command.first ) },
         _("Special Command")
      } );

   // end CLEANSPEECH remnant

   PluginManager & pm = PluginManager::Get();
   EffectManager & em = EffectManager::Get();
   {
      const PluginDescriptor *plug = pm.GetFirstPlugin(PluginTypeEffect|PluginTypeAudacityCommand);
      while (plug)
      {
         auto command = em.GetCommandIdentifier(plug->GetID());
         if (!command.empty())
            commands.push_back( {
               { command, plug->GetSymbol().Translation() },
               plug->GetPluginType() == PluginTypeEffect ?
                  _("Effect") : _("Menu Command (With Parameters)")
            } );
         plug = pm.GetNextPlugin(PluginTypeEffect|PluginTypeAudacityCommand);
      }
   }

   auto mManager = project->GetCommandManager();
   wxArrayString mLabels;
   CommandIDs mNames;
   std::vector<bool> vHasDialog;
   mLabels.clear();
   mNames.clear();
   mManager->GetAllCommandLabels(mLabels, vHasDialog, true);
   mManager->GetAllCommandNames(mNames, true);

   const bool english = wxGetLocale()->GetCanonicalName().StartsWith(wxT("en"));

   for(size_t i=0; i<mNames.size(); i++) {
      wxString label = mLabels[i];
      if( !vHasDialog[i] ){
         label.Replace( "&", "" );
         bool suffix;
         if (!english)
            suffix = false;
         else {
            // We'll disambiguate if the squashed name is short and shorter than the internal name.
            // Otherwise not.
            // This means we won't have repetitive items like "Cut (Cut)" 
            // But we will show important disambiguation like "All (SelectAll)" and "By Date (SortByDate)"
            // Disambiguation is no longer essential as the details box will show it.
            // PRL:  I think this reasoning applies only when locale is English.
            // For other locales, show the (CamelCaseCodeName) always.  Or, never?
            wxString squashed = label;
            squashed.Replace( " ", "" );

            suffix = squashed.length() < wxMin( 18, mNames[i].length());
         }

         if( suffix )
            label = label + " (" + mNames[i] + ")";

         commands.push_back(
            {
               {
                  mNames[i], // Internal name.
                  label // User readable name
               },
               _("Menu Command (No Parameters)")
            }
         );
      }
   }

   // Sort commands by their user-visible names.
   // PRL:  What exactly should happen if first members of pairs are not unique?
   // I'm not sure, but at least I can sort stably for a better defined result,
   // keeping specials before effects and menu items, and lastly commands.
   auto less =
      [](const Entry &a, const Entry &b)
         { return a.name.Translated() < b.name.Translated(); };
   std::stable_sort(commands.begin(), commands.end(), less);

   // Now uniquify by friendly name
   auto equal =
      [](const Entry &a, const Entry &b)
         { return a.name.Translated() == b.name.Translated(); };
   std::unique_copy(
      commands.begin(), commands.end(), std::back_inserter(mCommands), equal);
}

// binary search
auto MacroCommandsCatalog::ByFriendlyName( const wxString &friendlyName ) const
   -> Entries::const_iterator
{
   const auto less = [](const Entry &entryA, const Entry &entryB)
      { return entryA.name.Translated() < entryB.name.Translated(); };
   auto range = std::equal_range(
      begin(), end(), Entry{ { {}, friendlyName }, {} }, less
   );
   if (range.first != range.second) {
      wxASSERT_MSG( range.first + 1 == range.second,
                    "Non-unique user-visible command name" );
      return range.first;
   }
   else
      return end();
}

// linear search
auto MacroCommandsCatalog::ByCommandId( const CommandID &commandId ) const
   -> Entries::const_iterator
{
   // Maybe this too should have a uniqueness check?
   return std::find_if( begin(), end(),
      [&](const Entry &entry)
         { return entry.name.Internal() == commandId; });
}



wxString MacroCommands::GetCurrentParamsFor(const CommandID & command)
{
   const PluginID & ID =
      EffectManager::Get().GetEffectByIdentifier(command);
   if (ID.empty())
   {
      return wxEmptyString;   // effect not found.
   }

   return EffectManager::Get().GetEffectParameters(ID);
}

wxString MacroCommands::PromptForParamsFor(const CommandID & command, const wxString & params, wxWindow *parent)
{
   const PluginID & ID =
      EffectManager::Get().GetEffectByIdentifier(command);
   if (ID.empty())
   {
      return wxEmptyString;   // effect not found
   }

   wxString res = params;

   auto cleanup = EffectManager::Get().SetBatchProcessing(ID);

   if (EffectManager::Get().SetEffectParameters(ID, params))
   {
      if (EffectManager::Get().PromptUser(ID, parent))
      {
         res = EffectManager::Get().GetEffectParameters(ID);
      }
   }

   return res;
}

wxString MacroCommands::PromptForPresetFor(const CommandID & command, const wxString & params, wxWindow *parent)
{
   const PluginID & ID =
      EffectManager::Get().GetEffectByIdentifier(command);
   if (ID.empty())
   {
      return wxEmptyString;   // effect not found.
   }

   wxString preset = EffectManager::Get().GetPreset(ID, params, parent);

   // Preset will be empty if the user cancelled the dialog, so return the original
   // parameter value.
   if (preset.empty())
   {
      return params;
   }

   return preset;
}

double MacroCommands::GetEndTime()
{
   AudacityProject *project = GetActiveProject();
   if( project == NULL )
   {
      //AudacityMessageBox( _("No project to process!") );
      return -1.0;
   }
   TrackList * tracks = project->GetTracks();
   if( tracks == NULL )
   {
      //AudacityMessageBox( _("No tracks to process!") );
      return -1.0;
   }

   double endTime = tracks->GetEndTime();
   return endTime;
}

bool MacroCommands::IsMono()
{
   AudacityProject *project = GetActiveProject();
   if( project == NULL )
   {
      //AudacityMessageBox( _("No project and no Audio to process!") );
      return false;
   }

   TrackList * tracks = project->GetTracks();
   if( tracks == NULL )
   {
      //AudacityMessageBox( _("No tracks to process!") );
      return false;
   }

   return ( tracks->Any() - &Track::IsLeader ).empty();
}

wxString MacroCommands::BuildCleanFileName(const FilePath &fileName,
   const FileExtension &extension)
{
   const wxFileName newFileName{ fileName };
   wxString justName = newFileName.GetName();
   wxString pathName = newFileName.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR);
   const auto cleanedString = _("cleaned");

   if (justName.empty()) {
      wxDateTime now = wxDateTime::Now();
      int year = now.GetYear();
      wxDateTime::Month month = now.GetMonth();
      wxString monthName = now.GetMonthName(month);
      int dom = now.GetDay();
      int hour = now.GetHour();
      int minute = now.GetMinute();
      int second = now.GetSecond();
      justName = wxString::Format(wxT("%d-%s-%02d-%02d-%02d-%02d"),
           year, monthName, dom, hour, minute, second);

//      SetName(cleanedFileName);
//      bool isStereo;
//      double endTime = project->mTracks->GetEndTime();
//      double startTime = 0.0;
      //OnSelectAll();
      pathName = FileNames::FindDefaultPath(FileNames::Operation::Export);
      ::AudacityMessageBox(wxString::Format(_("Export recording to %s\n/%s/%s%s"),
            pathName, cleanedString, justName, extension),
         _("Export recording"),
         wxOK | wxCENTRE);
      pathName += wxFileName::GetPathSeparator();
   }
   wxString cleanedName = pathName;
   cleanedName += cleanedString;
   bool flag  = ::wxFileName::FileExists(cleanedName);
   if (flag == true) {
      ::AudacityMessageBox(_("Cannot create directory 'cleaned'. \nFile already exists that is not a directory"));
      return wxString{};
   }
   ::wxFileName::Mkdir(cleanedName, 0777, wxPATH_MKDIR_FULL); // make sure it exists

   cleanedName += wxFileName::GetPathSeparator();
   cleanedName += justName;
   cleanedName += extension;

   return cleanedName;
}

// TODO Move this out of Batch Commands
bool MacroCommands::WriteMp3File( const wxString & Name, int bitrate )
{  //check if current project is mono or stereo
   unsigned numChannels = 2;
   if (IsMono()) {
      numChannels = 1;
   }

   double endTime = GetEndTime();
   if( endTime <= 0.0f )
      return false;
   AudacityProject *project = GetActiveProject();
   if( bitrate <=0 )
   {
      // 'No' bitrate given, use the current default.
      // Use Mp3Stereo to control if export is to a stereo or mono file
      return mExporter.Process(project, numChannels, wxT("MP3"), Name, false, 0.0, endTime);
   }


   bool rc;
   long prevBitRate = gPrefs->Read(wxT("/FileFormats/MP3Bitrate"), 128);
   gPrefs->Write(wxT("/FileFormats/MP3Bitrate"), bitrate);
   int prevMode = gPrefs->Read(wxT("/FileFormats/MP3RateMode"), MODE_CBR);
   gPrefs->Write(wxT("/FileFormats/MP3RateMode"), MODE_CBR);

   auto cleanup = finally( [&] {
      gPrefs->Write(wxT("/FileFormats/MP3Bitrate"), prevBitRate);
      gPrefs->Write(wxT("/FileFormats/MP3RateMode"), prevMode);
      gPrefs->Flush();
   } );

   // Use Mp3Stereo to control if export is to a stereo or mono file
   rc = mExporter.Process(project, numChannels, wxT("MP3"), Name, false, 0.0, endTime);
   return rc;
}

// TIDY-ME: Get rid of special commands and make them part of the
// 'menu' system (but not showing on the menu)
//
// ======= IMPORTANT ========
// Special Commands are a KLUDGE whilst we wait for a better system to handle the menu
// commands from batch mode.
//
// Really we should be using a similar (or same) system to that used for effects
// so that parameters can be passed to the commands.  Many of the menu
// commands take a selection as their parameter.
//
// If you find yourself adding lots of existing commands from the menus here, STOP
// and think again.
// ======= IMPORTANT ========
// CLEANSPEECH remnant
bool MacroCommands::ApplySpecialCommand(
   int WXUNUSED(iCommand), const wxString &friendlyCommand,
   const CommandID & command, const wxString & params)
{
   if (ReportAndSkip(friendlyCommand, params))
      return true;

   AudacityProject *project = GetActiveProject();

   unsigned numChannels = 1;    //used to switch between mono and stereo export
   if (IsMono()) {
      numChannels = 1;  //export in mono
   } else {
      numChannels = 2;  //export in stereo
   }

   wxString filename;
   wxString extension; // required for correct message
   if (command == wxT("ExportWAV"))
      extension = wxT(".wav");
   else if (command == wxT("ExportOgg"))
      extension = wxT(".ogg");
   else if (command == wxT("ExportFLAC"))
      extension = wxT(".flac");
   else extension = wxT(".mp3");

   if (mFileName.empty()) {
      filename = BuildCleanFileName(project->GetFileName(), extension);
   }
   else {
      filename = BuildCleanFileName(mFileName, extension);
   }

   // We have a command index, but we don't use it!
   // TODO: Make this special-batch-command code use the menu item code....
   // FIXME: TRAP_ERR No error reporting on write file failure in batch mode.
   if (command == wxT("NoAction")) {
      return true;
   } else if (!mFileName.empty() && command == wxT("Import")) {
      // historically this was in use, now ignored if there
      return true;
   } else if (command == wxT("ExportMP3_56k_before")) {
#if defined(__WXMSW__)
      filename.Replace(wxT("cleaned\\"), wxT("cleaned\\MasterBefore_"), false);
#else
      filename.Replace(wxT("cleaned/"), wxT("cleaned/MasterBefore_"), false);
#endif
      return WriteMp3File(filename, 56);
   } else if (command == wxT("ExportMP3_56k_after")) {
#if defined(__WXMSW__)
      filename.Replace(wxT("cleaned\\"), wxT("cleaned\\MasterAfter_"), false);
#else
      filename.Replace(wxT("cleaned/"), wxT("cleaned/MasterAfter_"), false);
#endif
      return WriteMp3File(filename, 56);
   } else if (command == wxT("ExportMP3")) {
      return WriteMp3File(filename, 0); // 0 bitrate means use default/current
   } else if (command == wxT("ExportWAV")) {
      filename.Replace(wxT(".mp3"), wxT(".wav"), false);
      double endTime = GetEndTime();
      if (endTime <= 0.0f) {
         return false;
      }
      return mExporter.Process(project, numChannels, wxT("WAV"), filename, false, 0.0, endTime);
   } else if (command == wxT("ExportOgg")) {
#ifdef USE_LIBVORBIS
      filename.Replace(wxT(".mp3"), wxT(".ogg"), false);
      double endTime = GetEndTime();
      if (endTime <= 0.0f) {
         return false;
      }
      return mExporter.Process(project, numChannels, wxT("OGG"), filename, false, 0.0, endTime);
#else
      AudacityMessageBox(_("Ogg Vorbis support is not included in this build of Audacity"));
      return false;
#endif
   } else if (command == wxT("ExportFLAC")) {
#ifdef USE_LIBFLAC
      filename.Replace(wxT(".mp3"), wxT(".flac"), false);
      double endTime = GetEndTime();
      if (endTime <= 0.0f) {
         return false;
      }
      return mExporter.Process(project, numChannels, wxT("FLAC"), filename, false, 0.0, endTime);
#else
      AudacityMessageBox(_("FLAC support is not included in this build of Audacity"));
      return false;
#endif
   }
   AudacityMessageBox(
      wxString::Format(_("Command %s not implemented yet"), friendlyCommand));
   return false;
}
// end CLEANSPEECH remnant

bool MacroCommands::ApplyEffectCommand(
   const PluginID & ID, const wxString &friendlyCommand,
   const CommandID & command, const wxString & params,
   const CommandContext & Context)
{
   static_cast<void>(command);//compiler food.

   //Possibly end processing here, if in batch-debug
   if( ReportAndSkip(friendlyCommand, params))
      return true;

   const PluginDescriptor *plug = PluginManager::Get().GetPlugin(ID);
   if (!plug)
      return false;

   AudacityProject *project = GetActiveProject();

   // FIXME: for later versions may want to not select-all in batch mode.
   // IF nothing selected, THEN select everything
   // (most effects require that you have something selected).
   if( plug->GetPluginType() != PluginTypeAudacityCommand )
      project->SelectAllIfNone();

   bool res = false;

   auto cleanup = EffectManager::Get().SetBatchProcessing(ID);

   // transfer the parameters to the effect...
   if (EffectManager::Get().SetEffectParameters(ID, params))
   {
      if( plug->GetPluginType() == PluginTypeAudacityCommand )
         // and apply the effect...
         res = PluginActions::DoAudacityCommand(ID,
            Context,
            PluginActions::kConfigured |
            PluginActions::kSkipState |
            PluginActions::kDontRepeatLast);
      else
         // and apply the effect...
         res = PluginActions::DoEffect(ID,
            Context,
            PluginActions::kConfigured |
            PluginActions::kSkipState |
            PluginActions::kDontRepeatLast);
   }

   return res;
}

bool MacroCommands::ApplyCommand( const wxString &friendlyCommand,
   const CommandID & command, const wxString & params,
   CommandContext const * pContext)
{

   unsigned int i;
   // Test for a special command.
   // CLEANSPEECH remnant
   for( i = 0; i < sizeof(SpecialCommands)/sizeof(*SpecialCommands); ++i ) {
      if( command.IsSameAs( SpecialCommands[i].second, false) )
         return ApplySpecialCommand( i, friendlyCommand, command, params );
   }
   // end CLEANSPEECH remnant

   // Test for an effect.
   const PluginID & ID =
      EffectManager::Get().GetEffectByIdentifier( command );
   if (!ID.empty())
   {
      if( pContext )
         return ApplyEffectCommand(
            ID, friendlyCommand, command, params, *pContext);
      const CommandContext context(  *GetActiveProject() );
      return ApplyEffectCommand(
         ID, friendlyCommand, command, params, context);
   }

   AudacityProject *project = GetActiveProject();
   CommandManager * pManager = project->GetCommandManager();
   if( pContext ){
      if( pManager->HandleTextualCommand( command, *pContext, AlwaysEnabledFlag, AlwaysEnabledFlag ) )
         return true;
      pContext->Status( wxString::Format(
         _("Your batch command of %s was not recognized."), friendlyCommand ));
      return false;
   }
   else
   {
      const CommandContext context(  *GetActiveProject() );
      if( pManager->HandleTextualCommand( command, context, AlwaysEnabledFlag, AlwaysEnabledFlag ) )
         return true;
   }

   AudacityMessageBox(
      wxString::Format(
      _("Your batch command of %s was not recognized."), friendlyCommand ));

   return false;
}

bool MacroCommands::ApplyCommandInBatchMode( const wxString &friendlyCommand,
   const CommandID & command, const wxString &params,
   CommandContext const * pContext)
{
   AudacityProject *project = GetActiveProject();
   // Recalc flags and enable items that may have become enabled.
   GetMenuManager(*project).UpdateMenus(*project, false);
   // enter batch mode...
   bool prevShowMode = project->GetShowId3Dialog();
   project->mBatchMode++;
   auto cleanup = finally( [&] {
      // exit batch mode...
      project->SetShowId3Dialog(prevShowMode);
      project->mBatchMode--;
   } );

   return ApplyCommand( friendlyCommand, command, params, pContext );
}

static int MacroReentryCount = 0;
// ApplyMacro returns true on success, false otherwise.
// Any error reporting to the user in setting up the macro
// has already been done.
bool MacroCommands::ApplyMacro(
   const MacroCommandsCatalog &catalog, const wxString & filename)
{
   // Check for reentrant ApplyMacro commands.
   // We'll allow 1 level of reentry, but not more.
   // And we treat ignoring deeper levels as a success.
   if( MacroReentryCount > 1 )
      return true;

   // Restore the reentry counter (to zero) when we exit.
   auto cleanup1 = valueRestorer( MacroReentryCount);
   MacroReentryCount++;

   mFileName = filename;

   AudacityProject *proj = GetActiveProject();
   bool res = false;
   auto cleanup2 = finally( [&] {
      if (!res) {
         if(proj) {
            // Macro failed or was cancelled; revert to the previous state
            proj->RollbackState();
         }
      }
   } );

   mAbort = false;

   size_t i = 0;
   for (; i < mCommandMacro.size(); i++) {
      const auto &command = mCommandMacro[i];
      auto iter = catalog.ByCommandId(command);
      auto friendly = (iter == catalog.end())
         ? command // Expose internal name to user, in default of a better one!
         : iter->name.Translated();
      if (!ApplyCommandInBatchMode(friendly, command, mParamsMacro[i]) || mAbort)
         break;
   }

   res = (i == mCommandMacro.size());
   if (!res)
      return false;

   mFileName.Empty();

   // Macro was successfully applied; save the NEW project state
   wxString longDesc, shortDesc;
   wxString name = gPrefs->Read(wxT("/Batch/ActiveMacro"), wxEmptyString);
   if (name.empty())
   {
      /* i18n-hint: active verb in past tense */
      longDesc = _("Applied Macro");
      shortDesc = _("Apply Macro");
   }
   else
   {
      /* i18n-hint: active verb in past tense */
      longDesc = wxString::Format(_("Applied Macro '%s'"), name);
      shortDesc = wxString::Format(_("Apply '%s'"), name);
   }

   if (!proj)
      return false;
   if( MacroReentryCount <= 1 )
      proj->PushState(longDesc, shortDesc);
   return true;
}

// AbortBatch() allows a premature terminatation of a batch.
void MacroCommands::AbortBatch()
{
   mAbort = true;
}

void MacroCommands::AddToMacro(const CommandID &command, int before)
{
   AddToMacro(command, GetCurrentParamsFor(command), before);
}

void MacroCommands::AddToMacro(const CommandID &command, const wxString &params, int before)
{
   if (before == -1) {
      before = (int)mCommandMacro.size();
   }

   mCommandMacro.insert(mCommandMacro.begin() + before, command);
   mParamsMacro.insert(mParamsMacro.begin() + before, params);
}

void MacroCommands::DeleteFromMacro(int index)
{
   if (index < 0 || index >= (int)mCommandMacro.size()) {
      return;
   }

   mCommandMacro.erase( mCommandMacro.begin() + index );
   mParamsMacro.erase( mParamsMacro.begin() + index );
}

void MacroCommands::ResetMacro()
{
   mCommandMacro.clear();
   mParamsMacro.clear();
}

// ReportAndSkip() is a diagnostic function that avoids actually
// applying the requested effect if in batch-debug mode.
bool MacroCommands::ReportAndSkip(
   const wxString & friendlyCommand, const wxString & params)
{
   int bDebug;
   gPrefs->Read(wxT("/Batch/Debug"), &bDebug, false);
   if( bDebug == 0 )
      return false;

   //TODO: Add a cancel button to these, and add the logic so that we can abort.
   if( !params.empty() )
   {
      AudacityMessageBox( wxString::Format(_("Apply %s with parameter(s)\n\n%s"),friendlyCommand, params),
         _("Test Mode"));
   }
   else
   {
      AudacityMessageBox( wxString::Format(_("Apply %s"), friendlyCommand),
         _("Test Mode"));
   }
   return true;
}

void MacroCommands::MigrateLegacyChains()
{
   static bool done = false;
   if (!done) {
      // Check once per session at most

      // Copy chain files from the old Chains into the new Macros directory,
      // but only if like-named files are not already present in Macros.

      // Leave the old copies in place, in case a user wants to go back to
      // an old Audacity version.  They will have their old chains intact, but
      // won't have any edits they made to the copy that now lives in Macros
      // which old Audacity will not read.

      const auto oldDir = FileNames::LegacyChainDir();
      FilePaths files;
      wxDir::GetAllFiles(oldDir, &files, wxT("*.txt"), wxDIR_FILES);

      // add a dummy path component to be overwritten by SetFullName
      wxFileName newDir{ FileNames::MacroDir(), wxT("x") };

      for (const auto &file : files) {
         auto name = wxFileName{file}.GetFullName();
         newDir.SetFullName(name);
         const auto newPath = newDir.GetFullPath();
         if (!wxFileExists(newPath))
            FileNames::CopyFile(file, newPath);
      }
      done = true;
   }
   // To do:  use std::once
}

wxArrayString MacroCommands::GetNames()
{
   MigrateLegacyChains();

   wxArrayString names;
   FilePaths files;
   wxDir::GetAllFiles(FileNames::MacroDir(), &files, wxT("*.txt"), wxDIR_FILES);
   size_t i;

   wxFileName ff;
   for (i = 0; i < files.size(); i++) {
      ff = (files[i]);
      names.push_back(ff.GetName());
   }

   std::sort( names.begin(), names.end() );

   return names;
}

bool MacroCommands::IsFixed(const wxString & name)
{
   auto defaults = GetNamesOfDefaultMacros();
   if( make_iterator_range( defaults ).contains( name ) )
      return true;
   return false;
}

void MacroCommands::Split(const wxString & str, wxString & command, wxString & param)
{
   int splitAt;

   command.Empty();
   param.Empty();

   if (str.empty()) {
      return;
   }

   splitAt = str.Find(wxT(':'));
   if (splitAt < 0) {
      return;
   }

   command = str.Mid(0, splitAt);
   param = str.Mid(splitAt + 1);

   return;
}

wxString MacroCommands::Join(const wxString & command, const wxString & param)
{
   return command + wxT(": ") + param;
}
