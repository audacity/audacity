/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchCommands.cpp

  Dominic Mazzoni
  James Crook

********************************************************************//*!

\class BatchCommands
\brief Maintains the chain of commands used in batch processing.
See also BatchCommandDialog and BatchProcessDialog.

*//*******************************************************************/


#include "Audacity.h"
#include "BatchCommands.h"

#include <wx/defs.h>
#include <wx/dir.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>
#include <wx/textfile.h>

#include "Project.h"
#include "commands/CommandManager.h"
#include "effects/EffectManager.h"
#include "FileNames.h"
#include "Internat.h"
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

// KLUDGE: All commands should be on the same footing
// however, for historical reasons we distinguish between
//    - Effects (which are looked up in effects lists)
//    - Menu commands (which are held in command manager)
//    - Specials (which we deal with specially here)
enum eCommandType { CtEffect, CtMenu, CtSpecial };

// TIDY-ME: Not currently translated,
// but there are issues to address if we do.
// CLEANSPEECH remnant
static wxString SpecialCommands[] = {
   wxT("NoAction"),
   // wxT("Import"),   // non-functioning
   wxT("ExportMP3_56k_before"),
   wxT("ExportMP3_56k_after"),
   wxT("ExportFLAC"),
   wxT("ExportMP3"),
   wxT("ExportOgg"),
   wxT("ExportWAV")
};
// end CLEANSPEECH remnant

static const wxString MP3Conversion = wxT("MP3 Conversion");

BatchCommands::BatchCommands()
{
   ResetChain();

   wxArrayString names = GetNames();

   if (names.Index(MP3Conversion) == wxNOT_FOUND) {
      AddChain(MP3Conversion);
      RestoreChain(MP3Conversion);
      WriteChain(MP3Conversion);
   }
}

wxString BatchCommands::GetCommand(int index)
{
   if (index < 0 || index >= (int)mCommandChain.GetCount()) {
      return wxT("");
   }

   return mCommandChain[index];
}

wxString BatchCommands::GetParams(int index)
{
   if (index < 0 || index >= (int)mParamsChain.GetCount()) {
      return wxT("");
   }

   return mParamsChain[index];
}

int BatchCommands::GetCount()
{
   return (int)mCommandChain.GetCount();
}

bool BatchCommands::ReadChain(const wxString & chain)
{
   // Clear any previous chain
   ResetChain();

   // Build the filename
   wxFileName name(FileNames::ChainDir(), chain, wxT("txt"));

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

         // Backward compatibility for old Chain scripts
         // Please comment the version of audacity these are introduced in, so
         // that old ones can easily be removed once users have had a chance to
         // migrate
         if (cmd == wxT("SaveMP3_56k_before"))
            cmd = wxT("ExportMP3_56k_before");
         else if (cmd == wxT("SaveMP3_56k_after"))
            cmd = wxT("ExportMP3_56k_after");
         else if (cmd == wxT("ExportFlac"))
            cmd = wxT("ExportFLAC");
         else if (cmd == wxT("ExportMp3"))
            cmd = wxT("ExportMP3");
         else if (cmd == wxT("ExportWav"))
            cmd = wxT("ExportWAV");
         else if (cmd == wxT("Compressor") && (parm.find(wxT("DecayTime")) != parm.npos))
            parm.Replace(wxT("DecayTime"), wxT("ReleaseTime"), NULL);   // 2.0.6

         // Add to lists
         mCommandChain.Add(cmd);
         mParamsChain.Add(parm);
      }
   }

   // Done with the file
   tf.Close();

   return true;
}


bool BatchCommands::WriteChain(const wxString & chain)
{
   // Build the filename
   wxFileName name(FileNames::ChainDir(), chain, wxT("txt"));

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
   int lines = mCommandChain.GetCount();
   for (int i = 0; i < lines; i++) {
      // restore deprecated commands in chain script
      if (mCommandChain[i] == wxT("ExportMP3_56k_before"))
         mCommandChain[i] = wxT("SaveMP3_56k_before");
      else if (mCommandChain[i] == wxT("ExportMP3_56k_after"))
         mCommandChain[i] = wxT("SaveMP3_56k_after");
      else if (mCommandChain[i] == wxT("ExportFLAC"))
         mCommandChain[i] = wxT("ExportFlac");
      else if (mCommandChain[i] == wxT("ExportMP3"))
         mCommandChain[i] = wxT("ExportMp3");
      else if (mCommandChain[i] == wxT("ExportWAV"))
         mCommandChain[i] = wxT("ExportWav");

      tf.AddLine(mCommandChain[i] + wxT(":") + mParamsChain[ i ]);
   }

   // Write the chain
   tf.Write();

   // Done with the file
   tf.Close();

   return true;
}

bool BatchCommands::AddChain(const wxString & chain)
{
   // Build the filename
   wxFileName name(FileNames::ChainDir(), chain, wxT("txt"));

   // Set the file name
   wxTextFile tf(name.GetFullPath());

   // Create it..Create will display errors
   return tf.Create();
}

bool BatchCommands::DeleteChain(const wxString & chain)
{
   // Build the filename
   wxFileName name(FileNames::ChainDir(), chain, wxT("txt"));

   // Delete it...wxRemoveFile will display errors
   return wxRemoveFile(name.GetFullPath());
}

bool BatchCommands::RenameChain(const wxString & oldchain, const wxString & newchain)
{
   // Build the filenames
   wxFileName oname(FileNames::ChainDir(), oldchain, wxT("txt"));
   wxFileName nname(FileNames::ChainDir(), newchain, wxT("txt"));

   // Rename it...wxRenameFile will display errors
   return wxRenameFile(oname.GetFullPath(), nname.GetFullPath());
}

void BatchCommands::SetWavToMp3Chain() // a function per default chain?  This is flawed design!  MJS
{
   ResetChain();

   AddToChain( wxT("Normalize") );
   AddToChain( wxT("ExportMP3") );
}

// Gets all commands that are valid for this mode.
wxArrayString BatchCommands::GetAllCommands()
{
   wxArrayString commands;
   wxString command;
   commands.Clear();

   AudacityProject *project = GetActiveProject();
   if (!project)
   {
      return commands;
   }

   unsigned int i;

   // CLEANSPEECH remnant
   for(i=0;i<sizeof(SpecialCommands)/sizeof(SpecialCommands[0]);i++)
   {
      commands.Add( SpecialCommands[i] );
   }
   // end CLEANSPEECH remnant

   PluginManager & pm = PluginManager::Get();
   EffectManager & em = EffectManager::Get();
   const PluginDescriptor *plug = pm.GetFirstPlugin(PluginTypeEffect);
   while (plug)
   {
      command = em.GetEffectIdentifier(plug->GetID());
      if (!command.IsEmpty())
      {
         commands.Add(command);
      }
      plug = pm.GetNextPlugin(PluginTypeEffect);
   }

   commands.Sort();

   /* This is for later in development: include the menu commands.
         CommandManager * mManager = project->GetCommandManager();
         wxArrayString mNames;
         mNames.Clear();
         mManager->GetAllCommandNames(mNames, false);
         for(i=0; i<mNames.GetCount(); i++) {
            commands.Add( mNames[i] );
         }
   */
   return commands;
}


wxString BatchCommands::GetCurrentParamsFor(const wxString & command)
{
   const PluginID & ID = EffectManager::Get().GetEffectByIdentifier(command);
   if (ID.empty())
   {
      return wxEmptyString;   // effect not found.
   }

   return EffectManager::Get().GetEffectParameters(ID);
}

wxString BatchCommands::PromptForParamsFor(const wxString & command, const wxString & params, wxWindow *parent)
{
   const PluginID & ID = EffectManager::Get().GetEffectByIdentifier(command);
   if (ID.empty())
   {
      return wxEmptyString;   // effect not found
   }

   wxString res = params;

   EffectManager::Get().SetBatchProcessing(ID, true);

   if (EffectManager::Get().SetEffectParameters(ID, params))
   {
      if (EffectManager::Get().PromptUser(ID, parent))
      {
         res = EffectManager::Get().GetEffectParameters(ID);
      }
   }

   EffectManager::Get().SetBatchProcessing(ID, false);

   return res;
}

wxString BatchCommands::PromptForPresetFor(const wxString & command, const wxString & params, wxWindow *parent)
{
   const PluginID & ID = EffectManager::Get().GetEffectByIdentifier(command);
   if (ID.empty())
   {
      return wxEmptyString;   // effect not found.
   }

   wxString preset = EffectManager::Get().GetPreset(ID, params, parent);

   // Preset will be empty if the user cancelled the dialog, so return the original
   // parameter value.
   if (preset.IsEmpty())
   {
      return params;
   }

   return preset;
}

double BatchCommands::GetEndTime()
{
   AudacityProject *project = GetActiveProject();
   if( project == NULL )
   {
      //wxMessageBox( wxT("No project to process!") );
      return -1.0;
   }
   TrackList * tracks = project->GetTracks();
   if( tracks == NULL )
   {
      //wxMessageBox( wxT("No tracks to process!") );
      return -1.0;
   }

   double endTime = tracks->GetEndTime();
   return endTime;
}

bool BatchCommands::IsMono()
{
   AudacityProject *project = GetActiveProject();
   if( project == NULL )
   {
      //wxMessageBox( wxT("No project and no Audio to process!") );
      return false;
   }

   TrackList * tracks = project->GetTracks();
   if( tracks == NULL )
   {
      //wxMessageBox( wxT("No tracks to process!") );
      return false;
   }

   TrackListIterator iter(tracks);
   Track *t = iter.First();
   bool mono = true;
   while (t) {
      if (t->GetLinked()) {
         mono = false;
         break;
      }
      t = iter.Next();
   }

   return mono;
}

wxString BatchCommands::BuildCleanFileName(const wxString &fileName, const wxString &extension)
{
   const wxFileName newFileName{ fileName };
   wxString justName = newFileName.GetName();
   wxString pathName = newFileName.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR);

   if (justName == wxT("")) {
      wxDateTime now = wxDateTime::Now();
      int year = now.GetYear();
      wxDateTime::Month month = now.GetMonth();
      wxString monthName = now.GetMonthName(month);
      int dom = now.GetDay();
      int hour = now.GetHour();
      int minute = now.GetMinute();
      int second = now.GetSecond();
      justName = wxString::Format(wxT("%d-%s-%02d-%02d-%02d-%02d"),
           year, monthName.c_str(), dom, hour, minute, second);

//      SetName(cleanedFileName);
//      bool isStereo;
//      double endTime = project->mTracks->GetEndTime();
//      double startTime = 0.0;
      //OnSelectAll();
      pathName = gPrefs->Read(wxT("/DefaultOpenPath"), ::wxGetCwd());
      ::wxMessageBox(wxString::Format(wxT("Export recording to %s\n/cleaned/%s%s"),
                                      pathName.c_str(), justName.c_str(), extension.c_str()),
                     wxT("Export recording"),
                  wxOK | wxCENTRE);
      pathName += wxT("/");
   }
   wxString cleanedName = pathName;
   cleanedName += wxT("cleaned");
   bool flag  = ::wxFileName::FileExists(cleanedName);
   if (flag == true) {
      ::wxMessageBox(wxT("Cannot create directory 'cleaned'. \nFile already exists that is not a directory"));
      return wxT("");
   }
   ::wxFileName::Mkdir(cleanedName, 0777, wxPATH_MKDIR_FULL); // make sure it exists

   cleanedName += wxT("/");
   cleanedName += justName;
   cleanedName += extension;
   wxGetApp().AddFileToHistory(cleanedName);

   return cleanedName;
}

bool BatchCommands::WriteMp3File( const wxString & Name, int bitrate )
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
   // Use Mp3Stereo to control if export is to a stereo or mono file
   rc = mExporter.Process(project, numChannels, wxT("MP3"), Name, false, 0.0, endTime);
   gPrefs->Write(wxT("/FileFormats/MP3Bitrate"), prevBitRate);
   gPrefs->Flush();
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
bool BatchCommands::ApplySpecialCommand(int WXUNUSED(iCommand), const wxString & command,const wxString & params)
{
   if (ReportAndSkip(command, params))
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

   if (mFileName.IsEmpty()) {
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
   } else if (!mFileName.IsEmpty() && command == wxT("Import")) {
      // historically this was in use, now ignored if there
      return true;
   } else if (command == wxT("ExportMP3_56k_before")) {
      filename.Replace(wxT("cleaned/"), wxT("cleaned/MasterBefore_"), false);
      return WriteMp3File(filename, 56);
   } else if (command == wxT("ExportMP3_56k_after")) {
      filename.Replace(wxT("cleaned/"), wxT("cleaned/MasterAfter_"), false);
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
      wxMessageBox(_("Ogg Vorbis support is not included in this build of Audacity"));
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
      wxMessageBox(_("FLAC support is not included in this build of Audacity"));
      return false;
#endif
   }
   wxMessageBox(wxString::Format(_("Command %s not implemented yet"),command.c_str()));
   return false;
}
// end CLEANSPEECH remnant

bool BatchCommands::ApplyEffectCommand(const PluginID & ID, const wxString & command, const wxString & params)
{
   //Possibly end processing here, if in batch-debug
   if( ReportAndSkip(command, params))
      return true;

   AudacityProject *project = GetActiveProject();

   // FIXME: for later versions may want to not select-all in batch mode.
   // IF nothing selected, THEN select everything
   // (most effects require that you have something selected).
   project->SelectAllIfNone();

   bool res = false;

   EffectManager::Get().SetBatchProcessing(ID, true);

   // transfer the parameters to the effect...
   if (EffectManager::Get().SetEffectParameters(ID, params))
   {
      // and apply the effect...
      res = project->OnEffect(ID, AudacityProject::OnEffectFlags::kConfigured |
                                  AudacityProject::OnEffectFlags::kSkipState |
                                  AudacityProject::OnEffectFlags::kDontRepeatLast);
   }

   EffectManager::Get().SetBatchProcessing(ID, false);

   return res;
}

bool BatchCommands::ApplyCommand(const wxString & command, const wxString & params)
{

   unsigned int i;
   // Test for a special command.
   // CLEANSPEECH remnant
   for(i=0;i<sizeof(SpecialCommands)/sizeof(SpecialCommands[0]);i++)
   {
      if( command == SpecialCommands[i] )
         return ApplySpecialCommand( i, command, params );
   }
   // end CLEANSPEECH remnant

   // Test for an effect.
   const PluginID & ID = EffectManager::Get().GetEffectByIdentifier( command );
   if (!ID.empty())
   {
      return ApplyEffectCommand(ID, command, params);
   }

   wxMessageBox(
      wxString::Format(
      _("Your batch command of %s was not recognized."), command.c_str() ));

   return false;
}

bool BatchCommands::ApplyCommandInBatchMode(const wxString & command, const wxString &params)
{
   AudacityProject *project = GetActiveProject();
   bool rc;

   // enter batch mode...
   bool prevShowMode = project->GetShowId3Dialog();

   rc = ApplyCommand( command, params );

   // exit batch mode...
   project->SetShowId3Dialog(prevShowMode);

   return rc;
}

// ApplyChain returns true on success, false otherwise.
// Any error reporting to the user has already been done.
bool BatchCommands::ApplyChain(const wxString & filename)
{
   mFileName = filename;
   unsigned int i;
   bool res = true;

   mAbort = false;

   for (i = 0; i < mCommandChain.GetCount(); i++) {
      if (!ApplyCommandInBatchMode(mCommandChain[i], mParamsChain[i]) || mAbort) {
         res = false;
         break;
      }
   }

   mFileName.Empty();
   AudacityProject *proj = GetActiveProject();

   if (!res)
   {
      if(proj) {
         // Chain failed or was cancelled; revert to the previous state
         proj->RollbackState();
      }
      return false;
   }

   // Chain was successfully applied; save the NEW project state
   wxString longDesc, shortDesc;
   wxString name = gPrefs->Read(wxT("/Batch/ActiveChain"), wxEmptyString);
   if (name.IsEmpty())
   {
      longDesc = wxT("Applied batch chain");
      shortDesc = wxT("Apply chain");
   }
   else
   {
      longDesc = wxString::Format(wxT("Applied batch chain '%s'"), name.c_str());
      shortDesc = wxString::Format(wxT("Apply '%s'"), name.c_str());
   }

   if (!proj)
      return false;
   proj->PushState(longDesc, shortDesc);
   return true;
}

// AbortBatch() allows a premature terminatation of a batch.
void BatchCommands::AbortBatch()
{
   mAbort = true;
}

void BatchCommands::AddToChain(const wxString &command, int before)
{
   AddToChain(command, GetCurrentParamsFor(command), before);
}

void BatchCommands::AddToChain(const wxString &command, const wxString &params, int before)
{
   if (before == -1) {
      before = (int)mCommandChain.GetCount();
   }

   mCommandChain.Insert(command, before);
   mParamsChain.Insert(params, before);
}

void BatchCommands::DeleteFromChain(int index)
{
   if (index < 0 || index >= (int)mCommandChain.GetCount()) {
      return;
   }

   mCommandChain.RemoveAt(index);
   mParamsChain.RemoveAt(index);
}

void BatchCommands::ResetChain()
{
   mCommandChain.Clear();
   mParamsChain.Clear();
}

// ReportAndSkip() is a diagnostic function that avoids actually
// applying the requested effect if in batch-debug mode.
bool BatchCommands::ReportAndSkip(const wxString & command, const wxString & params)
{
   int bDebug;
   gPrefs->Read(wxT("/Batch/Debug"), &bDebug, false);
   if( bDebug == 0 )
      return false;

   //TODO: Add a cancel button to these, and add the logic so that we can abort.
   if( params != wxT("") )
   {
      wxMessageBox( wxString::Format(_("Apply %s with parameter(s)\n\n%s"),command.c_str(), params.c_str()),
         _("Test Mode"));
   }
   else
   {
      wxMessageBox( wxString::Format(_("Apply %s"),command.c_str()),
         _("Test Mode"));
   }
   return true;
}

wxArrayString BatchCommands::GetNames()
{
   wxArrayString names;
   wxArrayString files;
   wxDir::GetAllFiles(FileNames::ChainDir(), &files, wxT("*.txt"), wxDIR_FILES);
   size_t i;

   wxFileName ff;
   for (i = 0; i < files.GetCount(); i++) {
      ff = (files[i]);
      names.Add(ff.GetName());
   }

   return names;
}

bool BatchCommands::IsFixed(const wxString & name)
{
   if (name == MP3Conversion)
      return true;
   return false;
}

void BatchCommands::RestoreChain(const wxString & name)
{
// TIDY-ME: Effects change their name with localisation.
// Commands (at least currently) don't.  Messy.

   if (name == MP3Conversion)
      SetWavToMp3Chain();
}

void BatchCommands::Split(const wxString & str, wxString & command, wxString & param)
{
   int splitAt;

   command.Empty();
   param.Empty();

   if (str.IsEmpty()) {
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

wxString BatchCommands::Join(const wxString & command, const wxString & param)
{
   return command + wxT(": ") + param;
}
