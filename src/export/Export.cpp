/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Export
\brief Main class to control the export function.

*//****************************************************************//**

\class ExportType
\brief Container for information about supported export types.

*//********************************************************************/



#include "Export.h"

#include <wx/bmpbuttn.h>
#include <wx/filectrl.h>
#include <wx/simplebook.h>

#include "sndfile.h"

#include "FileDialog/FileDialog.h"

#include "AllThemeResources.h"
#include "BasicUI.h"
#include "Mix.h"
#include "MixAndRender.h"
#include "Prefs.h"
#include "../prefs/ImportExportPrefs.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "../ProjectSettings.h"
#include "../ProjectWindow.h"
#include "../ProjectWindows.h"
#include "ShuttleGui.h"
#include "../TagsEditor.h"
#include "Theme.h"
#include "WaveTrack.h"
#include "AudacityMessageBox.h"
#include "../widgets/Warning.h"
#include "FileNames.h"
#include "HelpSystem.h"
#include "ProgressDialog.h"
#include "wxFileNameWrapper.h"

#include "ExportMixerDialog.h"

namespace {

bool IsExtension(const ExportPlugin& plugin, int formatIndex, const FileExtension& ext)
{
   if(formatIndex >= 0 && formatIndex < plugin.GetFormatCount())
   {
      auto formatInfo = plugin.GetFormatInfo(formatIndex);
      if(formatInfo.mExtensions[0].empty() || formatInfo.mExtensions.Index(ext, false) != wxNOT_FOUND)
         return true;
   }
   return false;
}

}

//----------------------------------------------------------------------------
// Export
//----------------------------------------------------------------------------


wxDEFINE_EVENT(AUDACITY_FILE_SUFFIX_EVENT, wxCommandEvent);

BEGIN_EVENT_TABLE(Exporter, wxEvtHandler)
   EVT_FILECTRL_FILTERCHANGED(wxID_ANY, Exporter::OnFilterChanged)
   EVT_BUTTON(wxID_HELP, Exporter::OnHelp)
   EVT_COMMAND(wxID_ANY, AUDACITY_FILE_SUFFIX_EVENT, Exporter::OnExtensionChanged)
END_EVENT_TABLE()

namespace {
   const auto PathStart = L"Exporters";

   using ExportPluginFactories = std::vector< Exporter::ExportPluginFactory >;
   ExportPluginFactories &sFactories()
   {
      static ExportPluginFactories theList;
      return theList;
   }
}

Registry::GroupItemBase &Exporter::ExporterItem::Registry()
{
   static Registry::GroupItem<Registry::DefaultTraits> registry{ PathStart };
   return registry;
}

Exporter::ExporterItem::ExporterItem(
   const Identifier &id, const Exporter::ExportPluginFactory &factory )
   : SingleItem{ id }
   , mFactory{ factory }
{}

Exporter::RegisteredExportPlugin::RegisteredExportPlugin(
   const Identifier &id,
   const ExportPluginFactory &factory,
   const Registry::Placement &placement )
   : RegisteredItem{
      factory ? std::make_unique< ExporterItem >( id, factory ) : nullptr,
      placement
   }
{
}

Exporter::Exporter( AudacityProject &project )
: mProject{ &project }
{
   using namespace Registry;
   static OrderingPreferenceInitializer init{
      PathStart,
      { {wxT(""), wxT("PCM,MP3,OGG,FLAC,MP2,CommandLine,FFmpeg") } },
   };

   mMixerSpec = NULL;
   mBook = NULL;

   // build the list of export plugins.
   for ( const auto &factory : sFactories() )
      mPlugins.emplace_back( factory() );

   struct MyVisitor final : Visitor {
      MyVisitor()
      {
         // visit the registry to collect the plug-ins properly
         // sorted
         GroupItem<Registry::DefaultTraits> top{ PathStart };
         Registry::Visit( *this, &top, &ExporterItem::Registry() );
      }

      void Visit( SingleItem &item, const Path &path ) override
      {
         mPlugins.emplace_back(
            static_cast<ExporterItem&>( item ).mFactory() );
      }

      ExportPluginArray mPlugins;
   } visitor;

   mPlugins.swap( visitor.mPlugins );

   SetFileDialogTitle( XO("Export Audio") );
}

Exporter::~Exporter()
{
}

void Exporter::OnExtensionChanged(wxCommandEvent &evt)
{
   mDialog->SetFileExtension(evt.GetString().BeforeFirst(' ').Lower());
}

void Exporter::OnHelp(wxCommandEvent& WXUNUSED(evt))
{
   wxWindow * pWin = FindProjectFrame( mProject );
   HelpSystem::ShowHelp(pWin, L"File_Export_Dialog", true);
}

void Exporter::SetFileDialogTitle( const TranslatableString & DialogTitle )
{
   // The default title is "Export File"
   mFileDialogTitle = DialogTitle;
}

const ExportPluginArray &Exporter::GetPlugins()
{
   return mPlugins;
}

bool Exporter::Process(bool selectedOnly, double t0, double t1)
{
   // Save parms
   mSelectedOnly = selectedOnly;
   mT0 = t0;
   mT1 = t1;

   // Gather track information
   if (!ExamineTracks()) {
      return false;
   }

   // Ask user for file name
   if (!GetFilename()) {
      return false;
   }

   // Check for down mixing
   if (!CheckMix()) {
      return false;
   }
   
   auto exportFormatInfo = mPlugins[mFormat]->GetFormatInfo(mSubFormat);

   // Let user edit MetaData
   if (exportFormatInfo.mCanMetaData) {
      if (!TagsEditorDialog::DoEditMetadata( *mProject,
         XO("Edit Metadata Tags"), XO("Exported Tags"),
         ProjectSettings::Get( *mProject ).GetShowId3Dialog())) {
         return false;
      }
   }

   // Ensure filename doesn't interfere with project files.
   if (!CheckFilename()) {
      return false;
   }

   // Export the tracks
   std::unique_ptr<BasicUI::ProgressDialog> pDialog;
   bool success = ExportTracks(pDialog);

   // Get rid of mixerspec
   mMixerSpec.reset();

   if (success) {
      if (mFormatName.empty()) {
         gPrefs->Write(wxT("/Export/Format"), exportFormatInfo.mFormat);
      }

      FileNames::UpdateDefaultPath(FileNames::Operation::Export, mFilename.GetPath());
   }

   return success;
}

bool Exporter::Process(unsigned numChannels,
                       const FileExtension &type, const wxString & filename,
                       bool selectedOnly, double t0, double t1)
{
   std::unique_ptr<BasicUI::ProgressDialog> pDialog;
   return Process(numChannels, type, filename, selectedOnly, t0, t1, pDialog);
}

bool Exporter::Process(
   unsigned numChannels, const FileExtension& type, const wxString& filename,
   bool selectedOnly, double t0, double t1,
   std::unique_ptr<BasicUI::ProgressDialog>& progressDialog)
{
   // Save parms
   mChannels = numChannels;
   mFilename = filename;
   mSelectedOnly = selectedOnly;
   mT0 = t0;
   mT1 = t1;
   mActualName = mFilename;

   int i = -1;
   for (const auto& pPlugin : mPlugins)
   {
      ++i;
      for (int j = 0; j < pPlugin->GetFormatCount(); j++)
      {
         auto formatInfo = pPlugin->GetFormatInfo(j);
         if (formatInfo.mFormat.IsSameAs(type, false))
         {
            mFormat = i;
            mSubFormat = j;
            return CheckFilename() && ExportTracks(progressDialog);
         }
      }
   }
   
   return false;
}

bool Exporter::ExamineTracks()
{
   // Init
   mNumSelected = 0;

   // First analyze the selected audio, perform sanity checks, and provide
   // information as appropriate.

   // Tally how many are right, left, mono, and make sure at
   // least one track is selected (if selectedOnly==true)

   double earliestBegin = mT1;
   double latestEnd = mT0;

   auto &tracks = TrackList::Get(*mProject);

   bool anySolo =
      !((tracks.Any<const WaveTrack>() + &WaveTrack::GetSolo).empty());

   const auto range = tracks.Leaders<const WaveTrack>()
      + (mSelectedOnly ? &Track::IsSelected : &Track::Any)
      - (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute);

   mMono = std::all_of(range.begin(), range.end(), [](const WaveTrack *pTrack){
      return IsMono(*pTrack) && pTrack->GetPan() == 0.0;
   });

   for (auto tr : range) {
      mNumSelected++;

      if (tr->GetOffset() < earliestBegin) {
         earliestBegin = tr->GetOffset();
      }

      if (tr->GetEndTime() > latestEnd) {
         latestEnd = tr->GetEndTime();
      }
   }

   if (mNumSelected == 0) {
      TranslatableString message;
      if(mSelectedOnly)
         message = XO("All selected audio is muted.");
      else
         message = XO("All audio is muted.");
      ShowExportErrorDialog(
         ":576",
         message, AudacityExportCaptionStr(), false);
      return false;
   }

   // The skipping of silent space could be cleverer and take 
   // into account clips.
   // As implemented now, it can only skip initial silent space that 
   // has no clip before it, and terminal silent space that has no clip 
   // after it.
   if (mT0 < earliestBegin){
      // Bug 1904 
      // Previously we always skipped initial silent space.
      // Now skipping it is an opt-in option.
      bool skipSilenceAtBeginning;
      gPrefs->Read(wxT("/AudioFiles/SkipSilenceAtBeginning"),
                                      &skipSilenceAtBeginning, false);
      if (skipSilenceAtBeginning)
         mT0 = earliestBegin;
   }

   // We still skip silent space at the end
   if (mT1 > latestEnd)
      mT1 = latestEnd;

   return true;
}

bool Exporter::GetFilename()
{
   mFormat = -1;

   FileNames::FileTypes fileTypes;
   auto defaultFormat = mFormatName;
   if( defaultFormat.empty() )
      defaultFormat = gPrefs->Read(wxT("/Export/Format"),
                                         wxT("WAV"));

   mFilterIndex = 0;

   {
      int i = -1;
      for (const auto &pPlugin : mPlugins) {
         ++i;
         for (int j = 0; j < pPlugin->GetFormatCount(); j++)
         {
            auto formatInfo = pPlugin->GetFormatInfo(j);
            fileTypes.insert(fileTypes.end(), { formatInfo.mDescription, formatInfo.mExtensions });
            if (formatInfo.mFormat == defaultFormat) {
               mFormat = i;
               mSubFormat = j;
            }
            if (mFormat == -1) mFilterIndex++;
         }
      }
   }
   if (mFormat == -1)
   {
      mFormat = 0;
      mFilterIndex = 0;
      mSubFormat = 0;
   }
   wxString defext = mPlugins[mFormat]->GetFormatInfo(mSubFormat).mExtensions[0].Lower();

   //Bug 1304: Set a default path if none was given.  For Export.
   mFilename.SetPath(FileNames::FindDefaultPath(FileNames::Operation::Export));
   mFilename.SetName(mProject->GetProjectName());
   if (mFilename.GetName().empty())
      mFilename.SetName(_("untitled"));
   while (true) {
      // Must reset each iteration
      mBook = NULL;

      {
         auto useFileName = mFilename;
         if (!useFileName.HasExt())
            useFileName.SetExt(defext);
         FileDialogWrapper fd( ProjectWindow::Find( mProject ),
                       mFileDialogTitle,
                       mFilename.GetPath(),
                       useFileName.GetFullName(),
                       fileTypes,
                       wxFD_SAVE | wxRESIZE_BORDER);
         mDialog = &fd;
         mDialog->PushEventHandler(this);

         fd.SetUserPaneCreator(CreateUserPaneCallback, (wxUIntPtr) this);
         fd.SetFilterIndex(mFilterIndex);

         int result = fd.ShowModal();

         mDialog->PopEventHandler();

         if (result == wxID_CANCEL) {
            return false;
         }

         mFilename = fd.GetPath();
         if (mFilename == wxT("")) {
            return false;
         }

         mFormat = fd.GetFilterIndex();
         mFilterIndex = fd.GetFilterIndex();
      }

      {
         int c = 0;
         int i = -1;
         for (const auto &pPlugin : mPlugins)
         {
            ++i;
            for (int j = 0; j < pPlugin->GetFormatCount(); j++)
            {
               if (mFilterIndex == c)
               {
                  mFormat = i;
                  mSubFormat = j;
               }
               c++;
            }
         }
      }

      const auto ext = mFilename.GetExt();
      defext = mPlugins[mFormat]->GetFormatInfo(mSubFormat).mExtensions[0].Lower();

      //
      // Check the extension - add the default if it's not there,
      // and warn user if it's abnormal.
      //
      if (ext.empty()) {
         //
         // Make sure the user doesn't accidentally save the file
         // as an extension with no name, like just plain ".wav".
         //
         if (mFilename.GetName().Left(1) == wxT(".")) {
            auto prompt =
               XO("Are you sure you want to export the file as \"%s\"?\n")
                  .Format( mFilename.GetFullName() );

            int action = AudacityMessageBox(
               prompt,
               XO("Warning"),
               wxYES_NO | wxICON_EXCLAMATION);
            if (action != wxYES) {
               continue;
            }
         }

         mFilename.SetExt(defext);
      }

      if (!mPlugins[mFormat]->CheckFileName(mFilename, mSubFormat))
      {
         continue;
      }
      else if (!ext.empty() && IsExtension(*mPlugins[mFormat], mSubFormat, ext) && ext.CmpNoCase(defext)) {
         auto prompt = XO("You are about to export a %s file with the name \"%s\".\n\nNormally these files end in \".%s\", and some programs will not open files with nonstandard extensions.\n\nAre you sure you want to export the file under this name?")
               .Format(mPlugins[mFormat]->GetFormatInfo(mSubFormat).mFormat,
                       mFilename.GetFullName(),
                       defext);

         int action = AudacityMessageBox(
            prompt,
            XO("Warning"),
            wxYES_NO | wxICON_EXCLAMATION);
         if (action != wxYES) {
            continue;
         }
      }

      if (mFilename.GetFullPath().length() >= 256) {
         AudacityMessageBox(
            XO( "Sorry, pathnames longer than 256 characters not supported.") );
         continue;
      }

// For Mac, it's handled by the FileDialog
#if !defined(__WXMAC__)
      if (mFilename.FileExists()) {
         auto prompt = XO("A file named \"%s\" already exists. Replace?")
            .Format( mFilename.GetFullPath() );

         int action = AudacityMessageBox(
            prompt,
            XO("Warning"),
            wxYES_NO | wxICON_EXCLAMATION);
         if (action != wxYES) {
            continue;
         }
      }
#endif

      break;
   }

   return true;
}

//
// For safety, if the file already exists it stores the filename
// the user wants in actualName, and returns a temporary file name.
// The calling function should rename the file when it's successfully
// exported.
//
bool Exporter::CheckFilename()
{
   //
   // To be even safer, return a temporary file name based
   // on this one...
   //

   mActualName = mFilename;

   int suffix = 0;
   while (mFilename.FileExists()) {
      mFilename.SetName(mActualName.GetName() +
                        wxString::Format(wxT("%d"), suffix));
      suffix++;
   }

   return true;
}

bool Exporter::CheckMix(bool prompt /*= true*/ )
{
   // Clean up ... should never happen
   mMixerSpec.reset();

   // Determine if exported file will be stereo or mono or multichannel,
   // and if mixing will occur.

   auto downMix = ImportExportPrefs::ExportDownMixSetting.ReadEnum();
   int exportedChannels = mPlugins[mFormat]->SetNumExportChannels();

   if (downMix) {
      unsigned channels = mMono ? 1 : 2;
      mChannels =
         std::min(channels, mPlugins[mFormat]->GetFormatInfo(mSubFormat).mMaxChannels);

      if (mNumSelected > 1 || channels > mChannels) {
         // May give a message about mixing-down
         wxString exportFormat = mPlugins[mFormat]->GetFormatInfo(mSubFormat).mFormat;
         if (exportFormat != wxT("CL") && exportFormat != wxT("FFMPEG") && exportedChannels == -1)
            exportedChannels = mChannels;

         if (prompt) {
            auto pWindow = ProjectWindow::Find(mProject);
            if (exportedChannels == 1) {
               if (ShowWarningDialog(pWindow,
                  wxT("MixMono"),
                  XO("Your tracks will be mixed down and exported as one mono file."),
                  true) == wxID_CANCEL)
                  return false;
            }
            else if (exportedChannels == 2) {
               if (ShowWarningDialog(pWindow,
                  wxT("MixStereo"),
                  XO("Your tracks will be mixed down and exported as one stereo file."),
                  true) == wxID_CANCEL)
                  return false;
            }
            else {
               if (ShowWarningDialog(pWindow,
                  wxT("MixUnknownChannels"),
                  XO("Your tracks will be mixed down to one exported file according to the encoder settings."),
                  true) == wxID_CANCEL)
                  return false;
            }
         }
      }
   }
   else
   {
      if (exportedChannels < 0)
         exportedChannels = mPlugins[mFormat]->GetFormatInfo(mSubFormat).mMaxChannels;

      ExportMixerDialog md(&TrackList::Get( *mProject ),
                           mSelectedOnly,
                           exportedChannels,
                           NULL,
                           1,
                           XO("Advanced Mixing Options"));
      if (prompt) {
         if (md.ShowModal() != wxID_OK) {
            return false;
         }
      }

      mMixerSpec = std::make_unique<MixerSpec>(*(md.GetMixerSpec()));
      mChannels = mMixerSpec->GetNumChannels();
   }

   return true;
}

bool Exporter::ExportTracks(
   std::unique_ptr<BasicUI::ProgressDialog>& progressDialog)
{
   // Keep original in case of failure
   if (mActualName != mFilename) {
      ::wxRenameFile(mActualName.GetFullPath(), mFilename.GetFullPath());
   }

   bool success = false;

   auto cleanup = finally( [&] {
      if (mActualName != mFilename) {
         // Remove backup
         if ( success )
            ::wxRemoveFile(mFilename.GetFullPath());
         else {
            // Restore original, if needed
            ::wxRemoveFile(mActualName.GetFullPath());
            ::wxRenameFile(mFilename.GetFullPath(), mActualName.GetFullPath());
         }
         // Restore filename
         mFilename = mActualName;
      }
      else {
         if ( ! success )
            // Remove any new, and only partially written, file.
            ::wxRemoveFile(mFilename.GetFullPath());
      }
   } );

   auto result = mPlugins[mFormat]->Export(mProject,
                                       progressDialog,
                                       mChannels,
                                       mActualName.GetFullPath(),
                                       mSelectedOnly,
                                       mT0,
                                       mT1,
                                       mMixerSpec.get(),
                                       NULL,
                                       mSubFormat);

   success =
      result == ProgressResult::Success || result == ProgressResult::Stopped;

   return success;
}

void Exporter::CreateUserPaneCallback(wxWindow *parent, wxUIntPtr userdata)
{
   Exporter *self = (Exporter *) userdata;
   if (self)
   {
      self->CreateUserPane(parent);
   }
}

void Exporter::CreateUserPane(wxWindow *parent)
{
   ShuttleGui S(parent, eIsCreating);

   S.StartStatic(XO("Format Options"), 1);
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         mBook = S.Position(wxEXPAND).StartSimplebook();
         {
            for (const auto &pPlugin : mPlugins)
            {
               for (int j = 0; j < pPlugin->GetFormatCount(); j++)
               {
                  // Name of simple book page is not displayed
                  S.StartNotebookPage( {} );
                  {
                     pPlugin->OptionsCreate(S, j);
                  }
                  S.EndNotebookPage();
               }
            }
         }
         S.EndSimplebook();

         auto b = safenew wxBitmapButton(S.GetParent(), wxID_HELP, theTheme.Bitmap( bmpHelpIcon ));
         b->SetToolTip( XO("Help").Translation() );
         b->SetLabel(XO("Help").Translation());       // for screen readers
         S.Position(wxALIGN_BOTTOM | wxRIGHT | wxBOTTOM).AddWindow(b);
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();

   return;
}

void Exporter::OnFilterChanged(wxFileCtrlEvent & evt)
{
   int index = evt.GetFilterIndex();

   // On GTK, this event can fire before the userpane is created
   if (mBook == NULL || index < 0 || index >= (int) mBook->GetPageCount())
   {
      return;
   }

#if defined(__WXGTK__)
   // On Windows and MacOS, changing the filter in the dialog
   // automatically changes the extension of the current file
   // name. GTK doesn't, so do it here.
   {
      FileNames::FileTypes fileTypes;

      int i = -1;
      for (const auto &pPlugin : mPlugins)
      {
         ++i;
         for (int j = 0; j < pPlugin->GetFormatCount(); j++)
         {
            const auto formatInfo = pPlugin->GetFormatInfo(j);
            fileTypes.insert( fileTypes.end(), { formatInfo.mDescription, formatInfo.mExtensions } );
         }
      }

      if (index < fileTypes.size())
      {
         mDialog->SetFileExtension(fileTypes[index].extensions[0].Lower());
      }
   }
#endif

   mBook->ChangeSelection(index);
}

bool Exporter::ProcessFromTimerRecording(bool selectedOnly,
                                         double t0,
                                         double t1,
                                         wxFileName fnFile,
                                         int iFormat,
                                         int iSubFormat,
                                         int iFilterIndex)
{
   // Save parms
   mSelectedOnly = selectedOnly;
   mT0 = t0;
   mT1 = t1;

   // Auto Export Parameters
   mFilename = fnFile;
   mFormat = iFormat;
   mSubFormat = iSubFormat;
   mFilterIndex = iFilterIndex;

   // Gather track information
   if (!ExamineTracks()) {
      return false;
   }

   // Check for down mixing
   if (!CheckMix(false)) {
      return false;
   }

   // Ensure filename doesn't interfere with project files.
   if (!CheckFilename()) {
      return false;
   }

   // Export the tracks
   std::unique_ptr<BasicUI::ProgressDialog> pDialog;
   bool success = ExportTracks(pDialog);

   // Get rid of mixerspec
   mMixerSpec.reset();

   return success;
}

int Exporter::GetAutoExportFormat() {
   return mFormat;
}

int Exporter::GetAutoExportSubFormat() {
   return mSubFormat;
}

int Exporter::GetAutoExportFilterIndex() {
   return mFormat;
}

wxFileName Exporter::GetAutoExportFileName() {
   return mFilename;
}

bool Exporter::SetAutoExportOptions() {
   mFormat = -1;

   if( GetFilename()==false )
        return false;

   // Let user edit MetaData
   if (mPlugins[mFormat]->GetFormatInfo(mSubFormat).mCanMetaData) {
      if (!TagsEditorDialog::DoEditMetadata( *mProject,
         XO("Edit Metadata Tags"),
         XO("Exported Tags"),
         ProjectSettings::Get(*mProject).GetShowId3Dialog())) {
         return false;
      }
   }

   return true;
}

TranslatableString AudacityExportCaptionStr()
{
   return XO("Warning");
}
TranslatableString AudacityExportMessageStr()
{
   return XO("Unable to export.\nError %s");
}


// This creates a generic export error dialog
// Untranslated ErrorCodes like "MP3:1882" are used since we don't yet have
// a good user facing error message.  They allow us to 
// distinguish where the error occurred, and we can update the landing
// page as we learn more about when (if ever) these errors actually happen.
// The number happens to at one time have been a line number, but all
// we need from them is that they be distinct.
void ShowExportErrorDialog(wxString ErrorCode,
   TranslatableString message,
   const TranslatableString& caption,
   bool allowReporting)
{
   using namespace BasicUI;
   ShowErrorDialog( {},
      caption,
      message.Format( ErrorCode ),
      "Error:_Unable_to_export", // URL.
      ErrorDialogOptions { allowReporting ? ErrorDialogType::ModalErrorReport : ErrorDialogType::ModalError });
}

void ShowDiskFullExportErrorDialog(const wxFileNameWrapper &fileName)
{
   BasicUI::ShowErrorDialog( {},
      XO("Warning"),
      FileException::WriteFailureMessage(fileName),
      "Error:_Disk_full_or_not_writable"
   );
}


