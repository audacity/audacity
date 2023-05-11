/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Export
\brief Main class to control the export function.

*//*****************************************************************/

#include "Export.h"

#include <numeric>

#include "sndfile.h"

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
#include "../TagsEditor.h"
#include "Theme.h"
#include "WaveTrack.h"
#include "../widgets/Warning.h"
#include "FileNames.h"
#include "ProgressDialog.h"
#include "wxFileNameWrapper.h"
#include "StretchingSequence.h"

#include "ExportMixerDialog.h"
#include "ExportUtils.h"

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
}

Exporter::~Exporter()
{
}

const ExportPluginArray &Exporter::GetPlugins()
{
   return mPlugins;
}

void Exporter::Configure(const wxFileName &filename, int pluginIndex, int formatIndex)
{
   mFilename = filename;
   mActualName = filename;
   mFormat = pluginIndex;
   mSubFormat = formatIndex;
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
   FixFilename();

   // Export the tracks
   std::unique_ptr<BasicUI::ProgressDialog> pDialog;
   bool success = ExportTracks(pDialog);

   // Get rid of mixerspec
   mMixerSpec.reset();


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
            FixFilename();
            return ExportTracks(progressDialog);
         }
      }
   }
   
   return false;
}

bool Exporter::ExamineTracks()
{
   // Init

   // First analyze the selected audio, perform sanity checks, and provide
   // information as appropriate.

   // Tally how many are right, left, mono, and make sure at
   // least one track is selected (if selectedOnly==true)

   double earliestBegin = mT1;
   double latestEnd = mT0;

   auto &tracks = TrackList::Get(*mProject);
   

   const auto range = ExportUtils::FindExportWaveTracks(tracks, mSelectedOnly);
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

//
// For safety, if the file already exists it stores the filename
// the user wants in actualName, and returns a temporary file name.
// The calling function should rename the file when it's successfully
// exported.
//
void Exporter::FixFilename()
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
      constexpr auto MaxExportChannels = 32;
      
      if (exportedChannels < 0)
         exportedChannels = mPlugins[mFormat]->GetFormatInfo(mSubFormat).mMaxChannels;

      const auto exportTracks = ExportUtils::FindExportWaveTracks(TrackList::Get( *mProject ), mSelectedOnly);
      mMixerSpec = std::make_unique<MixerOptions::Downmix>(
         std::accumulate(
            exportTracks.begin(),
            exportTracks.end(),
            0,
            [](int sum, const auto& track) { return sum + track->NChannels(); }),
         // JKC: This is an attempt to fix a 'watching brief' issue, where the slider is
         // sometimes not slidable.  My suspicion is that a mixer may incorrectly
         // state the number of channels - so we assume there are always at least two.
         // The downside is that if someone is exporting to a mono device, the dialog
         // will allow them to output to two channels. Hmm.  We may need to revisit this.
         // STF (April 2016): AMR (narrowband) and MP3 may export 1 channel.
         std::clamp(exportedChannels,
            1,
            MaxExportChannels));
      if(prompt)
      {
         ExportMixerDialog md(exportTracks, mMixerSpec.get(),
                              NULL,
                              1,
                              XO("Advanced Mixing Options"));
         if(md.ShowModal() != wxID_OK)
            return false;
      }
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

bool Exporter::ProcessFromTimerRecording(double t0,
                                         double t1,
                                         wxFileName fnFile,
                                         int iFormat,
                                         int iSubFormat)
{
   // Save parms
   mSelectedOnly = false;
   mT0 = t0;
   mT1 = t1;

   // Auto Export Parameters
   mFilename = fnFile;
   mFormat = iFormat;
   mSubFormat = iSubFormat;

   // Gather track information
   if (!ExamineTracks()) {
      return false;
   }

   // Check for down mixing
   if (!CheckMix(false)) {
      return false;
   }

   // Ensure filename doesn't interfere with project files.
   FixFilename();

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

wxFileName Exporter::GetAutoExportFileName() {
   return mFilename;
}

bool Exporter::SetAutoExportOptions() {
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


