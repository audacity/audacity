/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Export
\brief Main class to control the export function.

*//*****************************************************************/

#include "Export.h"

#include "sndfile.h"

#include "BasicUI.h"
#include "Mix.h"
#include "MixAndRender.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "../ProjectSettings.h"
#include "../TagsEditor.h"
#include "Theme.h"
#include "WaveTrack.h"
#include "FileNames.h"
#include "ProgressDialog.h"
#include "wxFileNameWrapper.h"

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

Registry::GroupItem &Exporter::ExporterItem::Registry()
{
   static Registry::TransparentGroupItem<> registry{ PathStart };
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
         TransparentGroupItem<> top{ PathStart };
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

bool Exporter::SetExportRange(double t0, double t1, bool selectedOnly, bool skipSilenceAtBeginning)
{
   int numLeft = 0;
   int numRight = 0;
   int numMono = 0;

   // First analyze the selected audio, perform sanity checks, and provide
   // information as appropriate.

   // Tally how many are right, left, mono, and make sure at
   // least one track is selected (if selectedOnly==true)

   double earliestBegin = t1;
   double latestEnd = t0;

   auto &tracks = TrackList::Get( *mProject );
   
   for (auto tr : ExportUtils::FindExportWaveTracks(tracks, selectedOnly))
   {
      if (tr->GetChannel() == Track::LeftChannel) {
         numLeft++;
      }
      else if (tr->GetChannel() == Track::RightChannel) {
         numRight++;
      }
      else if (tr->GetChannel() == Track::MonoChannel) {
         // It's a mono channel, but it may be panned
         float pan = tr->GetPan();

         if (pan == -1.0)
            numLeft++;
         else if (pan == 1.0)
            numRight++;
         else if (pan == 0)
            numMono++;
         else {
            // Panned partially off-center. Mix as stereo.
            numLeft++;
            numRight++;
         }
      }

      if (tr->GetOffset() < earliestBegin) {
         earliestBegin = tr->GetOffset();
      }

      if (tr->GetEndTime() > latestEnd) {
         latestEnd = tr->GetEndTime();
      }
   }

   if (numLeft == 0 && numRight == 0 && numMono == 0)
      return false;

   mT0 = t0;
   mT1 = t1;
   mNumLeft = numLeft;
   mNumRight = numRight;
   mNumMono = numMono;
   mSelectedOnly = selectedOnly;
   
   // The skipping of silent space could be cleverer and take
   // into account clips.
   // As implemented now, it can only skip initial silent space that
   // has no clip before it, and terminal silent space that has no clip
   // after it.
   if (t0 < earliestBegin){
      // Bug 1904
      // Previously we always skipped initial silent space.
      // Now skipping it is an opt-in option.
      if (skipSilenceAtBeginning)
         mT0 = earliestBegin;
   }

   // We still skip silent space at the end
   if (mT1 > latestEnd)
      mT1 = latestEnd;

   return true;
}

bool Exporter::Process()
{
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

bool Exporter::ProcessFromTimerRecording()
{
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

MixerOptions::Downmix* Exporter::CreateMixerSpec()
{
   constexpr auto MaxExportChannels = 32u;

   auto exportTracks = ExportUtils::FindExportWaveTracks(TrackList::Get( *mProject ), mSelectedOnly);
   mMixerSpec = std::make_unique<MixerOptions::Downmix>(exportTracks.size(),
                                                        std::clamp(mPlugins[mFormat]->GetFormatInfo(mSubFormat).mMaxChannels,
                                                                   // JKC: This is an attempt to fix a 'watching brief' issue, where the slider is
                                                                   // sometimes not slidable.  My suspicion is that a mixer may incorrectly
                                                                   // state the number of channels - so we assume there are always at least two.
                                                                   // The downside is that if someone is exporting to a mono device, the dialog
                                                                   // will allow them to output to two channels. Hmm.  We may need to revisit this.
                                                                   // STF (April 2016): AMR (narrowband) and MP3 may export 1 channel.
                                                                   1u,
                                                                   MaxExportChannels));
   return mMixerSpec.get();
}

Exporter::DownMixMode Exporter::SetUseStereoOrMonoOutput()
{
   // Clean up ... should never happen
   mMixerSpec.reset();
   
   if (mNumRight > 0 || mNumLeft > 0) {
      mChannels = 2;
   }
   else {
      mChannels = 1;
   }
   mChannels = std::min(mChannels,
                        mPlugins[mFormat]->GetFormatInfo(mSubFormat).mMaxChannels);
   
   auto numLeft =  mNumLeft + mNumMono;
   auto numRight = mNumRight + mNumMono;

   wxString exportFormat = mPlugins[mFormat]->GetFormatInfo(mSubFormat).mFormat;
   //TODO: violates OCP
   if(exportFormat == wxT("CL") || exportFormat == wxT("FFMPEG"))
      return DownMixMode::FormatDefined;
   
   if(numLeft > 1 || numRight > 1 || mNumLeft + mNumRight + mNumMono > mChannels)
      return mChannels == 1 ? DownMixMode::Mono : DownMixMode::Stereo;
   
   return DownMixMode::None;
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


